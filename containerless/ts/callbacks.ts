import * as request from 'request';
import * as express from 'express';
import * as state from './state';
import { Trace, newTrace } from './tracing';
import { string, number, identifier, unknown, clos } from './exp';

export type Request = {
    path: string
};


export type ResponseCallback = (response: string) => void;

export class Callbacks {

    private app: express.Express | undefined;
    public trace: Trace;

    constructor() {
        this.app = undefined;
        this.trace = newTrace();
    }

    /**
     * Executes a callback body in the context of its callback trace.
     * 
     * @param trace Callback trace.
     * @param body Callback body.
     */
    private withTrace(trace: Trace, body: () => void) {
        let outerTrace = this.trace;
        this.trace = trace;
        try {
            trace.newTrace();
            return body();
        }
        finally {
            trace.exitBlock();
            // If this callback is called again, which occurs with listen,
            // then we need to reset the cursor to start from the top.
            trace.newTrace();
            this.trace = outerTrace;
        }
    }

    /**
     * Mock callback function used for testing.
     * 
     * 1. Creates a callback trace.
     * 2. Wraps the callback trace with the callback body.
     * 3. Returns 2 as a function.
     * 
     * @param callback
     */
    mockCallback(callback: (value: any) => void): ((value: any) => void) {
        const callbackArgStr = '$response';
        let innerTrace = this.trace.traceCallback('mock', number(0), callbackArgStr);
        return (value: any) => {
            this.withTrace(innerTrace, () => {
                innerTrace.pushArgs([clos({ }), identifier(callbackArgStr)]);
                callback(value);
            });
        };
    }

    /**
     * Passes a callback to setImmediate().
     * 
     * 1. Creates a callback trace.
     * 2. Wraps the callback trace with the callback body.
     * 3. Passes 2 as a callback to setImmediate().
     * 
     * @param eventArgStr
     * @param callback
     */
    immediate(eventArgStr: string, callback: (callbackArg: string) => void) {
        const callbackArgStr = '$x';
        let innerTrace = this.trace.traceCallback('immediate', string(eventArgStr), callbackArgStr);
        setImmediate(() => {
            this.withTrace(innerTrace, () => {
                innerTrace.pushArg(identifier(callbackArgStr));
                callback(eventArgStr);
            });
        });
    }

    /**
     * Issues an HTTP GET request
     * @param uri URL for the request
     * @param callback receives the request body or undefined if request failed
     */
    get(
        uri: string,
        callback: (response: undefined | string) => void) {
        // TODO(arjun): string(uri) is not right. This needs to be the expression
        // passed to the function.
        let innerTrace = this.trace.traceCallback('get', this.trace.popArg(), '$response');
        request.get(uri, undefined, (error, resp) => {
            this.withTrace(innerTrace, () => {
                if (error !== null) {
                    innerTrace.pushArg(identifier('$response'));
                    callback(undefined);
                }
                else {
                    // TODO(arjun): Test case with nested closures
                    innerTrace.pushArg(identifier('$response'));
                    callback(String(resp.body));
                }
            });
        });
        this.trace.traceReturn(number(0));
    }

    public listen(
        callback: (request: Request, responseCallback: ResponseCallback) => void) {
        this.app = express();

        // There isn't anything inherently wrong with this, but calling listen
        // multiple times makes the application unpredictable.
        if (state.isListening()) {
            console.error(`Serverless function called listen more than once`);
            process.exit(1);
        }
        state.setListening();

        this.app.get('/trace', (req, resp) => {
            resp.send(JSON.stringify(this.trace.getTrace()));
        });

        this.app.get('/:path*', (req, resp) => {
            callback({ path: req.path }, (response) => {
                resp.send(response);
            });
        });



        const port = state.getListenPort();
        this.app.listen(port);
        console.error(`Serverless function has started listening on port ${port}`);
    }

}