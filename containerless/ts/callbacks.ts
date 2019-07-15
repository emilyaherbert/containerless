import * as request from 'request';
import * as express from 'express';
import * as state from './state';
import { Trace, newTrace } from './tracing';
import { string, number, identifier } from './exp';

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

    mockCallback(
        callback: (value: any) => void): (value: any) => void {
        let innerTrace = this.trace.traceCallback('mock', number(0), '$response');
        return (value: any) => {
            this.withTrace(innerTrace, () => {
                innerTrace.pushArg(identifier('$response'));
                callback(value);
            });
        };
    }

    immediate(arg: string, callback: (arg: string) => void) {
        let innerTrace = this.trace.traceCallback('immediate', string(arg), '$x');
        setImmediate(() => {
            this.withTrace(innerTrace, () => {
                innerTrace.pushArg(identifier('$x'));
                callback(arg);
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