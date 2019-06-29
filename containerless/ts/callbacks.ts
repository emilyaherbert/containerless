import * as request from 'request';
import * as express from 'express';
import * as state from './state';

const app = express();

export type Request = {
    path: string
};

export type ResponseCallback = (response: string) => void;
/**
 * Issues an HTTP GET request
 * @param uri URL for the request
 * @param callback receives the request body or undefined if request failed
 */
export function get(
    uri: string,
    callback: (response: undefined | string) => void) {
    request.get(uri, undefined, (error, resp) => {
        if (error === null) {
            callback(undefined);
        }
        else {
            callback(String(resp.body));
        }
    });
}

export function listen(
    callback: (request: Request, responseCallback: ResponseCallback) => void) {
    // There isn't anything inherently wrong with this, but calling listen
    // multiple times makes the application unpredictable.
    if (state.isListening()) {
        console.error(`Serverless function called listen more than once`);
        process.exit(1);
    }
    state.setListening();

    app.get('/:path*', (req, resp) => {
        callback({ path: req.path }, (response) => {
            resp.send(response);
        });
    });

    const port = state.getListenPort();
    app.listen(port);
    console.error(`Serverless function has started listening on port ${port}`);
}
