import * as state from './state';
import * as callbacks from './callbacks';
import * as exp from './exp';
import * as fs from 'fs';
export { exp };

if (!(process.argv.length >= 3 && process.argv.length <= 4)) {
    console.error(`Expected port number on command line`);
    process.exit(1);
}

if (process.argv.length === 4) {
    if (process.argv[3] === 'disable-tracing') {
        state.disableTracing();
    }
    else {
        console.error(`4th argument must be disable-tracing or omitted`);
        process.exit(1);
    }
}

if (process.argv[2] === 'test') {
    state.setListenPort('test');
}
else {
    const listenPort = Number(process.argv[2]) | 0;
    if (listenPort <= 1024) {
        console.error(`Expected port argument to be > 1024 or the word 'test'`);
        process.exit(1);
    }
    state.setListenPort(listenPort);
}

export let cb = new callbacks.Callbacks();

export function getTrace() {
    return cb.trace;
}

export function get(
    uri: string,
    callback: (response: undefined | JSON) => void) {
    return cb.get(uri, callback);
}

export function post(obj: any, callback: (response: undefined | string) => void) {
    return cb.post(obj, callback);
}

export function put(obj: any, callback: (response: undefined | string) => void) {
    return cb.put(obj, callback);
}

function delete_(obj: any, callback: (response: undefined | string) => void) {
    return cb.delete(obj, callback);
}
export { delete_ as delete };


function validateRequests(requests: unknown): requests is callbacks.Request[] {
    // NOTE(arjun): TS doesn't let us statically check this function in any
    // meaningful way.
    let requests_ = requests as any;
    if (requests_ instanceof Array === false) {
        return false;
    }
    return requests_.every((r: any) =>
        typeof r === 'object' &&
        typeof r.path === 'string' &&
        typeof r.body !== 'undefined' &&
        typeof r.query !== 'undefined');
}

export function listen(
    callback: (request: callbacks.Request) => void) {
    if (state.getListenPort() !== 'test') {
        return cb.listen(callback);
    }

    state.setListening();
    let tracedCallback = cb.tracedListenCallback(callback);
    let requests =
        JSON.parse(fs.readFileSync(0, { encoding: 'utf-8' })) as unknown;
    if (!validateRequests(requests)) {
        throw new Error(`Expected an array of mock requests from stdin`);
    }
    for (let request of requests) {
        tracedCallback(request as callbacks.Request);
    }

    // setImmediate is necessary so that execution reaches the end of the
    // main body of the program. E.g., if the last function call in the program
    // is the listen, the end of the main body calls exitBlock to exit the
    // main body of the program. If we call getTrace immediately, then the
    // trace for the main body will end with 'unknown'.
    setImmediate(() => {
        console.log(JSON.stringify(cb.trace.getTrace()));
    });
}

export function respond(response: any) {
    return cb.respond(response);
}

export function hello() {
    return cb.hello();
}

export function helloWithID(requestID: any) {
    return cb.helloWithID(requestID);
}

// Check if the application called listening. If not, blow up.
setImmediate(() => {
    // If this library is loaded using require, this code will run after the
    // serverless function has finished its first turn. In that turn, it needs
    // to call listen. If not, we exit with an error.
    if (state.isListening() === false) {
        console.error(`Serverless function did not call listen()`);
        process.exit(1);
    }
});
