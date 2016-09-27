#/bin/bash
openssl s_client -cert priv/ssl/cert.pem -key priv/ssl/cert.key -state -connect 127.0.0.1:2302
