import BaseHTTPServer
import os.path

ann_dir = '.'
dest_dir = '.'

class Handler(BaseHTTPServer.BaseHTTPRequestHandler):
    def do_GET(self):
        f = ann_dir+self.path
        if not os.path.exists(f):
            self.send_response(404)
        else:
            self.send_response(200)
            self.end_headers()
            self.wfile.write(open(f).read())

    def do_POST(self):
        with open(os.path.join(dest_dir, 'hi'), 'w') as f:
            f.write(str(self.headers))
        print self.rfile.read()

        self.send_response(200)
        self.send_header('Content-Type', 'text/plain')
        self.end_headers()
       
if __name__ == '__main__':
    address = ('', 8000)
    httpd = BaseHTTPServer.HTTPServer(address, Handler)
    httpd.serve_forever()
