#!/usr/bin/python3
#
# Test HTTP requests
# Author: Everton de Vargas Agilar
# Data: 17/01/2017
#
# Modo de uso:
#

import requests
from requests.auth import HTTPDigestAuth
import json

def request(Url):
	myResponse = requests.get(Url,auth=HTTPDigestAuth("evertonagilar@gmail.com", "123456"), verify=True)
	Str = myResponse.content
	print(Str.decode("utf-8"))
	

request("http://localhost:2301/unb_aula/pessoa/1")
request("http://localhost:2301/unb_aula/pessoa/2")
request("http://localhost:2301/unb_aula/pessoa/3")
request("http://localhost:2301/unb_aula/pessoa/4")
print("")
request("http://localhost:2301/unb_aula/pessoa/1")
request("http://localhost:2301/unb_aula/pessoa/2")
request("http://localhost:2301/unb_aula/pessoa/3")
request("http://localhost:2301/unb_aula/pessoa/4")
print("")
request("http://localhost:2301/unb_aula/pessoa/1")
request("http://localhost:2301/unb_aula/pessoa/2")
request("http://localhost:2301/unb_aula/pessoa/3")
request("http://localhost:2301/unb_aula/pessoa/4")
print("")
request("http://localhost:2301/unb_aula/pessoa/1")
request("http://localhost:2301/unb_aula/pessoa/1")
request("http://localhost:2301/unb_aula/pessoa/2")
request("http://localhost:2301/unb_aula/pessoa/2")
print("")
request("http://localhost:2301/unb_aula/pessoa/8")



