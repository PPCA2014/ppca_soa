# ErlangMS

ErlangMS is a Enterprise Service Bus (ESB) developed in *Erlang/OTP* to facilitate the integration of systems through a service-oriented approach for the systems of the University of Brazilia. This work is the result of efforts made in the Master of Applied Computing at the University of Brasilia by graduate student *Everton Vargas Agilar*. 

The ESB consists of a server called *ems-bus* and a *documented architecture* to implement the services in Erlang, Java and future in .NET Framework languages.

## Main design features

* Back-end modular and based on the concept of service catalogs;

* Communication services is through asynchronous messages and requests for services by customers through HTTP/REST or LDAP;

* Published services are specified in a service catalogs in JSON format;

* Published services can be implemented in Erlang or Java;

* Support HTTP Basic authentication;
 
* Support Lightweight Directory Access Protocol (LDAP v3) authentication (Proxy LDAP);

* Support OAuth2 authentication;

* Users, clients, and access profiles to authentication are stored externally to the bus to simplify integration with the enterprise system or SGBD used by the organization.


## Installation instructions

The ErlangMS software is installed in the /usr/lib/ems-bus folder under the proprietary user erlangms in the operating system. For security reasons, the bus runs as erlangms instead of root. In addition, the home directory /var/opt/erlangms is created. In this folder you will find the configuration file the emsbus.conf of the bus and the configuration file odbc.ini used to access the database.

To install ErlangMS in the Debian, Ubuntu or CentOS, follow the instructions below:


###### 1) Download setup

```console
$ wget https://raw.githubusercontent.com/erlangMS/releases/master/setup/setup-emsbus-linux.x86_64.sh
$ chmod +x setup-emsbus-linux.x86_64.sh
```


###### 2) Run the setup as root or with the sudo command

```console
$ sudo ./setup-emsbus-linux.x86_64.sh
Preparing for installation, please wait...
Downloading https://github.com/erlangms/releases/raw/master/1.0.12/ems-bus-1.0.12-centos.7.x86_64.rpm...
Starting the ERLANGMS installation on CentOS Linux 7 (Core)
Purpose: A service-oriented bus developed in Erlang/OTP by Everton de Vargas Agilar
Version: ems-bus-1.0.12-centos.7.x86_64
Log file: setup_emsbus__06032017_084841.log
Host ip: 164.41.103.35
Date: 06/03/2017 08:48:45
=============================================================================
Skipping EPEL 7 repository installation because it is already installed.
Skipping Erlang Runtime Library installation because it is already installed.
Skipping python34 installation because it is already installed.
Skipping openldap installation because it is already installed.
Skipping openldap-clients installation because it is already installed.
Skipping driver SQL-Server freetds installation because it is already installed.
Removing previously installed 1.0.12 version.
Installing ems-bus-1.0.12-centos.7.x86_64.rpm...
Preparing...                          ########################################
Updating / installing...
ems-bus-1.0.12-centos.7               ########################################
Installation was unsuccessful.
You want to send the installation log via email? [Yn]n
```

## Running instructions

ErlangMS is installed as a service on the systemd. The systemd start, stop, and status options can be used to start, stop and query the bus status.

If the bus is stopped, use the following command to start it.

```console
$ sudo systemctl start ems-bus
```

If everything is ok, go to http://localhost:2301/ on your browser.

Exemplifying with the curl utility

```console
$ curl http://localhost:2301
*{"message": "It works!!!"}*
```


## Implementing a Hello World service in Erlang or Java language

To implement a new service, you must clone the project in github and save the services implemented in the src folder.

###### 1) First, you must specify the service contract

Open your text editor, enter the following specification and save it to a file called priv/catalog/samples/hello_world.json

```console
[
	{
		"name" : "/samples/hello_world",
		"comment": "Hello World em Erlang",
		"owner": "samples",
		"version": "1",
		"service" : "helloworld_service:execute",
		"url": "/samples/hello_world",
		"type": "GET",
		"authorization" : "public",
		"lang" : "erlang"
	},

	{
		"name" : "/samples/hello_world_java",
		"comment": "Hello World em Java",
		"owner": "samples",
		"version": "1",
		"service" : "br.erlangms.samples.service.HelloWorldService:helloWorld",
		"url": "/samples/hello_world_java",
		"type": "GET",
		"authorization" : "public",
		"lang" : "java"
	}
]
```

###### 2) After, tell the bus about the new service, including an entry in the file priv/catalog/catalog.json

```console
[

	{
		"catalog": "emsbus", 
		"file": "emsbus/ems_main.json"
	},

	{
		"catalog": "hello_world", 
		"file": "samples/hello_world.json"
	}

]
```
Obs.: The priv/catalog/catalog.json file is called the service master catalog and is the first catalog read by the bus during its execution. In this file, there are only includes for other service catalog files.

###### 3) Now, code the service and save in src/samples

```console

-module(helloworld_service).

-include("../include/ems_schema.hrl").

-export([execute/1]).
 
execute(Request) -> 
	{ok, Request#request{code = 200, 
		response_data = <<"{\"message\": \"Hello World!!!\"}">>}
	}.
	
```

During the execution of a service by a client, the bus finds the required service contract and executes the corresponding service code. The Request object is passed to the service code and after it is finished, the result is sent to the client according to the protocol used (HTTP, LDAP, etc).

###### 4) Compile the project and restart the bus

If the service is written in Erlang, you must compile the bus project with the build.sh utility. Later to run ErlangMS in the foreground, use the start.sh utility.

```console
$ ./build.sh
Build erlangms tool ( Version: 2.0.0   Hostname: localhost )
Verify if exist conf file /etc/default/erlangms-build... OK
Reading settings from /etc/default/erlangms-build... OK
Checking installed erlang version... OK
Distro: Ubuntu Artful Aardvark (development branch)
Erlang version: 20
SKIP DEPS: true
SKIP CLEAN: true
KEEP DB: true
Date: 14/10/2017 17:19:11
=============================================================================
Compiling the project erlangms...
==> jsx (compile)
==> rfc3339 (compile)
==> cowlib (compile)
==> ranch (compile)
==> cowboy (compile)
==> parse_trans (compile)
==> json_rec (compile)
==> poolboy (compile)
==> inotify (compile)
WARN:  Missing plugins: [pc]
==> esqlite (compile)
==> mochiweb (compile)
==> oauth2 (compile)
==> erlydtl (compile)
==> jesse (compile)
==> rel (compile)
==> ems-bus (compile)
Ok!

$ ./start.sh
Erlang/OTP 20 [erts-9.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [kernel-poll:false]

Eshell V9.1  (abort with ^G)
(emsbus@localhost)1> 
ems_config loading configuration file "/home/agilar/desenvolvimento/erlangms/ems-bus/priv/conf/emsbus.conf"...
INFO 14/10/2017 16:12:42  Loading ESB ems-bus-1.0.18 instance on Erlang/OTP 20.
INFO 14/10/2017 16:12:42  ems_logger debug mode disabled.
INFO 14/10/2017 16:12:42  ems_logger archive log file checkpoint.
INFO 14/10/2017 16:12:42  ems_logger open "/home/agilar/desenvolvimento/erlangms/ems-bus/priv/log/emsbus@philco/2017/out/emsbus_out_14102017_161242.log" for append.
INFO 14/10/2017 16:12:42  Start ems_http_server.
INFO 14/10/2017 16:12:42  Start ems_file_watcher.
INFO 14/10/2017 16:12:42  Start ems_https_server.
INFO 14/10/2017 16:12:42  Start ems_cache.
INFO 14/10/2017 16:12:42  Start ems_odbc_pool.
INFO 14/10/2017 16:12:42  Hosts in the cluster: [philco].
INFO 14/10/2017 16:12:42  Default authorization mode: <<"oauth2">>.
INFO 14/10/2017 16:12:42  ESB ems-bus-1.0.18 (PID 11580) started in 48ms.
INFO 14/10/2017 16:12:42  ems_logger set level info.
INFO 14/10/2017 16:12:45  ems_http_listener listener http in 127.0.0.1:2301.
INFO 14/10/2017 16:12:45  ems_http_listener listener http in 192.168.0.11:2301.
INFO 14/10/2017 16:12:45  ems_http_listener listener https in 127.0.0.1:2344.
INFO 14/10/2017 16:12:45  ems_http_listener listener https in 192.168.0.11:2344.
INFO 14/10/2017 16:12:45  ems_catalog_loader_fs sync 0 inserts(s), 0 updates(s), 0 error(s) since 14/10/2017 15:25:30.

$ curl http://localhost:2301/samples/hello_world
{"message": "Hello World!!!"}
```


###### 5) Now, code the service in Java version

The coded version of the Java service has a design similar to the Erlang version. The service code will receive a Request object and must return the result to the bus. The developer does not have to worry about data serialization since everything is done automatically through the ems_java package provided in the ErlangMS site in github.

```java
package br.erlangms.samples.service;

import javax.ejb.Singleton;
import javax.ejb.Startup;
import br.erlangms.EmsServiceFacade;
import br.erlangms.IEmsRequest;

@Singleton
@Startup
public class HelloWorldFacade extends EmsServiceFacade {

	 public String helloWorld(IEmsRequest request) {
		    return "Hello World!!!";
	 }

}

```
Running a service in Java is a little more complex due to the need to publish the service code in a Java EE container. For example, in UnB, web services in Java are published in JBoss containers. Bus communication with Java containers is performed automatically by the ems_java package but the necessary configuration will not be discussed here.



###### 6) Consuming the service

Exemplifying with the curl utility

```
curl -X GET localhost:2301/samples/hello_world_java
{"message": "Hello World!!!"}
```




Project dependencies for the bus
=====

* Erlang R20 - <http://www.erlang.org/download.html>



Documentation of functional programming
=====

Documentation on the Erlang

<http://www.erlang.org/>

Excellent online book about distributed programming in Erlang

<http://learnyousomeerlang.com/>

List of articles about programming in Erlang

<https://github.com/0xAX/erlang-bookmarks/blob/master/ErlangBookmarks.md>
