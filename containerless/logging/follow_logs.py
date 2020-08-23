#!/usr/bin/env python3
from http.server import BaseHTTPRequestHandler, HTTPServer
from sys import argv
import json
import re

# FilterDups(N) is a logger that filters duplicate messages. To do so, it stores the last N messages
# in an internal buffer, and the 'maybe_log' method will only display message if it is not in the
# buffer. This class is effective because our log messages have precise timestamps. So, we are
# unlikely to mistakenly suppress a message that should have been repeated.
class FilterDups:
    def __init__(self, max_buf_len):
        self.buf = [ ]
        self.max_buf_len = max_buf_len
        self.index = 0
    
    def maybe_log(self, message):
        if not (message in self.buf):
            print(message)
        if len(self.buf) < self.max_buf_len:
            self.buf.append(message)
        else:
            self.buf[self.index] = message
        self.index = (self.index + 1) % self.max_buf_len

filter_dups = FilterDups(500)

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
            the_line = log_line['log']
            the_line = re.sub('(?<=\[)[^\s]*', '', the_line)
            filter_dups.maybe_log(f"{log_line['kubernetes']['pod_name']} {the_line}")
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
