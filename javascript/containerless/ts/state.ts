let listening = false;
let listenPort: number | 'test' = 0;
let tracing = true;

export function setListening() {
    listening = true;
}

export function isListening() {
    return listening;
}

export function setListenPort(port: number | 'test') {
    listenPort = port;
}

export function getListenPort() {
    return listenPort;
}

export function disableTracing() {
    tracing = false;
}

export function isTracing() {
    return tracing;
}