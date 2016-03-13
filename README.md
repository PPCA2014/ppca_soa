ErlangMS
=====

ErlangMS is a platform developed in Erlang/OTP to facilitate the integration of systems through a service-oriented approach for the systems of the University of Brazilia. This work is the result of efforts made in the Master of Applied Computing at the University of Brasilia by graduate student Everton Vargas agilar. 

The platform consists of a Enterprise Service Bus (ESB), called EmsBus, and a documented architecture to implement the services in Erlang, Java and future in .NET Framework languages.

###Main design features

* Modular and multi-platform;

* Communication services is through asynchronous messages and requests for services by customers through REST and LDAP;

* Published services are specified in a service catalog in JSON format;

* Services can be published in one or more nodes in the cluster (eg, Containers JBoss in Java EE) to avoid single points of failure (SPOFs);

* Support HTTP Authentication;
 
* Preliminary support Lightweight Directory Access Protocol (LDAP v3) for authentication;

* OAuth2 authentication (in progress)

* Front-end lightweight and optimized for service-oriented computing (https://github.com/eliot-framework/eliot)



*See the platform architecture em https:*//github.com/erlangMS/msbus/blob/master/doc/arquitetura_erlangms.pdf


Running the bus
-----------------------

If the project is already installed and configured, run the *start* command, according to the operating system:

If you are in Linux, type:

```console
$ ./start.sh
(msbus@puebla)1> ErlangMS Development Version 1.0
Initializing the module pool:
   Module msbus_eventmgr with 1 worker.
   Module msbus_catalogo with 2 workers.
   Module msbus_user with 2 workers.
   Module msbus_cache with 1 worker.
   Module msbus_http_server with 1 worker.
   Module msbus_ldap_server with 1 worker.
   Module msbus_request with 1 worker.
   Module msbus_http_worker with 3 workers.
   Module msbus_ldap_worker with 3 workers.
   Module msbus_health with 6 workers.
   Module msbus_dispatcher with 6 workers.
   Module msbus_static_file_service with 6 workers.
   Module msbus_user_service with 2 workers.
   Module msbus_catalogo_service with 2 workers.
   Module msbus_info_service with 2 workers.
   Module msbus_favicon_service with 2 workers.
   Module msbus_options_service with 2 workers.
   Module msbus_ldap_service with 2 workers.
   Module msbus_health_service with 12 workers.
config_file_dest: /home/viper/.erlangms/msbus@puebla.conf
cat_host_alias: #{<<"local">> => <<"puebla">>}
cat_host_search: local
cat_node_search: node01, node02, node03
log_file_dest: logs
log_file_checkpoint: 6000ms
tcp_listen_address: ["127.0.0.1"]
tcp_allowed_address: []
tcp_port: 2301
tcp_keepalive: true
tcp_nodelay: true
tcp_max_http_worker: 128
Portal Api Management: http://127.0.0.1:2301/portal/index.html
msbus@puebla started in 244ms.
Listening http packets on 127.0.0.1:2301.
Listening ldap packets on 127.0.0.1:2389.
```

```

If everything is OK, go to http://localhost:2301/samples/hello_world on your browser.

*{"message": "Hello World!!!"}*


###Compiling the project:

Check the wiki below to see how to download the project, compile and start the bus: https://github.com/erlangMS/msbus/wiki/Instalar-o-EBS-ErlangMS-msbus


###Project dependencies
------------------------

* Erlang R18 - <http://www.erlang.org/download.html>
* jsx - encode/decore JSON <https://github.com/talentdeficit/jsx>


###Documentation of functional programming
-----------------------------------------

Documentação sobre Erlang

<http://www.erlang.org/>

Excellent online book about distributed programming in Erlang

<http://learnyousomeerlang.com/>

List of articles about programming in Erlang

<https://github.com/0xAX/erlang-bookmarks/blob/master/ErlangBookmarks.md>
