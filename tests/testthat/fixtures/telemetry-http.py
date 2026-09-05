"""Loopback extractor fixture: one retry, then success; or a private 403 body."""

from http.server import BaseHTTPRequestHandler
from socketserver import TCPServer


class Handler(BaseHTTPRequestHandler):
    attempts = 0

    def do_GET(self):
        Handler.attempts += 1
        forbidden = self.path.startswith('/forbidden/')
        status = 403 if forbidden else (503 if Handler.attempts == 1 else 200)
        self.send_response(status)
        self.send_header('Content-Type', 'text/plain')
        self.send_header('Retry-After', '0')
        self.end_headers()
        self.wfile.write(b'private-response-body' if forbidden else b'# Article')

    def log_message(self, *args):
        pass


# HTTPServer resolves its server name during binding; this fixture needs no DNS.
server = TCPServer(('127.0.0.1', 0), Handler)
print(server.server_address[1], flush=True)
server.serve_forever()
