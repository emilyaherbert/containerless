import * as state from './state';
import * as callbacks from './callbacks';
import { from, clos, identifier, number, binop, string } from './exp';
export { from, clos, identifier, number, binop, string };

export let cb = new callbacks.Callbacks();

export function getTrace() {
    return cb.trace;
}

export function get(
    uri: string,
    callback: (response: undefined | string) => void) {
    return cb.get(uri, callback);
}

export function listen(
    callback: (request: callbacks.Request,
               responseCallback: callbacks.ResponseCallback) => void) {
    return cb.listen(callback);
}


if (process.argv.length !== 3) {
    console.error(`Expected port number on command line`);
    process.exit(1);
}

const listenPort = Number(process.argv[2]) | 0;
if (listenPort <= 1024) {
    console.error(`Expected port argument to be > 1024`);
    process.exit(1);
}
state.setListenPort(listenPort);

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
