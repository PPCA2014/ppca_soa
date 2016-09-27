#/bin/bash
openssl s_client -cert cert.pem -key cert.key -state -connect 127.0.0.1:2302
