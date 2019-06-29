let listening = false;
let listenPort = 0;

export function setListening() {
    listening = true;
}

export function isListening() {
    return listening;
}

export function setListenPort(port: number) {
    listenPort = port;
}

export function getListenPort() {
    return listenPort;
}