#!/usr/bin/env python3
from http.server import BaseHTTPRequestHandler, HTTPServer
from sys import argv
import json

class S(BaseHTTPRequestHandler):
    # The implementation of log_message in the base class logs the request. The definition below
    # supresses logging.
    def log_message(self, format, *args):
        pass

    def do_POST(self):
        content_length = int(self.headers['Content-Length']) # <--- Gets the size of data
        post_data = self.rfile.read(content_length).decode('utf-8')
        json_log = json.loads(post_data)
        for log_line in json_log:
            if log_line['kubernetes']['namespace_name'] != 'containerless':
                continue
            print(f"{log_line['kubernetes']['pod_name']} {log_line['log']}")
        self.send_response(200)
        self.wfile.write(b'')

def run(port):
    server_address = ('', port)
    httpd = HTTPServer(server_address, S)
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        pass
    httpd.server_close()

if __name__ == '__main__':
    if len(argv) == 2:
        run(port=int(argv[1]))
    else:
        run(port=8081)
