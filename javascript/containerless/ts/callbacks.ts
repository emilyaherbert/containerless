import * as request from 'request';
import * as express from 'express';
import * as bodyParser from "body-parser";
import * as state from './state';
import { newTrace } from './tracing';
import { newMockTrace } from './mockTracing';
import { number, identifier } from './exp';
import { TracingInterface } from './types';

const defaultEventArg = number(0);

export type Request = {
    path: string,
    query: JSON,
    body: JSON
}

export class Callbacks {

    private app: express.Express | undefined;
    private response: express.Response | undefined;
    public trace: TracingInterface;

    constructor() {
        this.app = undefined;
        this.response = undefined;
        this.trace = state.isTracing() ? newTrace() : newMockTrace();
    }

    /**
     * Executes a callback body in the context of its callback trace.
     * 
     * @param trace Callback trace.
     * @param body Callback body.
     */
    private withTrace(trace: TracingInterface, body: () => void) {
        let outerTrace = this.trace;
        this.trace = trace;
        trace.newTrace();
        let result = body();
        trace.exitBlock();
        trace.newTrace();
        this.trace = outerTrace;
        return result;
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
    mockCallback(callback: (value: any) => void, arg: any): ((value: any) => void) {
        const [_, callbackClos, argRep] = this.trace.popArgs();
        let innerTrace = this.trace.traceCallback('mock', argRep, ['clos', 'response'], callbackClos);
        return (value: any) => {
            this.withTrace(innerTrace, () => {
                innerTrace.pushArgs([identifier('clos'), identifier('response')]);
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
    immediate(callbackArgStr: string, callback: (callbackArg: string) => void) {
        const [_, argRep, callbackClos] = this.trace.popArgs();
        let innerTrace = this.trace.traceCallback('immediate', argRep, ['clos', 'x'], callbackClos);
        setImmediate(() => {
            this.withTrace(innerTrace, () => {
                innerTrace.pushArgs([identifier('clos'), identifier('x')]);
                callback(callbackArgStr);
            });
        });
    }

    /**
     * Issues an HTTP GET request
     * @param uri URL for the request
     * @param callback receives the request body or undefined if request failed
     */
    get(
        obj: any,
        callback: (response: undefined | JSON) => void) {
        // TODO(arjun): string(uri) is not right. This needs to be the expression
        // passed to the function.
        let [_, $argRep, $callbackClos] = this.trace.popArgs();
        let innerTrace = this.trace.traceCallback('get', $argRep, ['clos', 'response'], $callbackClos);
        
        // TODO(emily): Bad. Fix.
        if (state.getListenPort() === 'test') {
            this.withTrace(innerTrace, () => {
                innerTrace.pushArgs([identifier('clos'), identifier('response')]);
                callback(JSON.parse(String("{ message: 'GENERIC RESPONSE' }")));
            });
        } else {

            request.get(obj, undefined, (error, resp) => {
                this.withTrace(innerTrace, () => {
                    innerTrace.pushArgs([identifier('clos'), identifier('reponse')]);
                    if (error !== null) {
                        callback(undefined);
                    }
                    else {
                        // TODO(arjun): Test case with nested closures
                        callback(JSON.parse(String(resp.body)));
                    }

                    //innerTrace.traceReturn(number(0));
                });
            });
        }
    }

    /**
     * Issues an HTTP POST request
     */
    post(obj: string | any, callback: (response: undefined | string) => void) {
        let [_, $callbackClos] = this.trace.popArgs();
        let innerTrace = this.trace.traceCallback('post', defaultEventArg, ['clos', 'request'], $callbackClos);

        /*
        // https://stackoverflow.com/questions/37870594/how-to-post-with-request-in-express
        request(obj, function(error: any, response: any, body: any){
            console.log(body);
        });
        */

        if (state.getListenPort() === 'test') {
            this.withTrace(innerTrace, () => {
                innerTrace.pushArgs([identifier('clos'), identifier('response')]);
                // TODO(emily): This is probably wrong.
                callback(String("{ message: 'GENERIC RESPONSE' }"));
            });
        } else {
            // TODO(emily): Fix by creating a type for the argument that is either string | Type. Default arguments, the whole 9 yards.
            let obj2 = obj;
            if(typeof obj !== 'string') {
                obj2.body = JSON.stringify(obj2.body);
            }
            request.post(obj2, (error: any, resp: any) => {
                this.withTrace(innerTrace, () => {
                    innerTrace.pushArgs([identifier('clos'), identifier('reponse')]);
                    if (error !== null) {
                        callback(undefined);
                    }
                    else {
                        callback(String(resp.body));
                    }
                });
            });
        }
    }

    public tracedListenCallback(callback: (request: Request) => void) {
        let [_, $callbackClos] = this.trace.popArgs();
        let innerTrace = this.trace.traceCallback('listen', defaultEventArg, ['clos', 'request'], $callbackClos);

        return (req: Request) => {
            this.withTrace(innerTrace, () => {
                innerTrace.pushArgs([identifier('clos'), identifier('request')]);
                if (typeof req === 'string') {
                    // TODO(arjun): This is a bit of a hack to allow us to
                    // test tracing by sending raw strings as input. We should
                    // instead put in the effort to construct mock request
                    // objects.
                    callback(req);
                }
                else {
                    callback({ path: req.path, query: req.query, body: req.body });
                }
            });
        };
    }

    public listen(callback: (request: Request) => void) {
        let tracedCallback = this.tracedListenCallback(callback);
        this.app = express();
        this.app.use(bodyParser.json());
        this.app.use(bodyParser.urlencoded({ extended: false }));

        // There isn't anything inherently wrong with this, but calling listen
        // multiple times makes the application unpredictable.
        if (state.isListening()) {
            //console.error(`Serverless function called listen more than once`);
            process.exit(1);
        }
        state.setListening();

        this.app.get('/', (req, resp) => {
            resp.send('Hello world!\n');
        });

        this.app.post('/', (req, resp) => {
            resp.send('Hello world 2.0!\n');
        });

        this.app.get('/clear', (req, resp) => {
            this.trace.newTrace();
            resp.send('Cleared!\n');
        })

        this.app.get('/trace', (req, resp) => {
            resp.send(JSON.stringify(this.trace.getTrace()));
        });

        this.app.get('/:path*', (req, resp) => {
            resp.send("Can only do this with POST!\n");
        });

        this.app.post('/:path*', (req, resp) => {
            this.response = resp;
            tracedCallback({ path: req.path, query: req.query, body: req.body });
        });

        const port = state.getListenPort();

        this.app.listen(port);
        console.error(`Serverless function has started listening on port ${port}`);
    }


    public respond(response: any) {
        let [_, $response] = this.trace.popArgs();
        this.trace.tracePrimApp('send', [$response]);
        if(this.response !== undefined) {
            this.response.send('' + response + '\n');
        } else if(state.getListenPort() !== 'test') {
            throw new Error("No express.Response found.");
        }
    }
}