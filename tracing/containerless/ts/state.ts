let listening = false;
let listenPort: number | 'test' = 0;

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