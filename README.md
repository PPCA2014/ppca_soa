= ErlangMS

ErlangMS is a Enterprise Service Bus (ESB) developed in *Erlang/OTP* to facilitate the integration of systems through a service-oriented approach for the systems of the University of Brazilia. This work is the result of efforts made in the Master of Applied Computing at the University of Brasilia by graduate student *Everton Vargas Agilar*. 

The ESB consists of a server, called *ems-bus*, and a *documented architecture* to implement the services in Erlang, Java and future in .NET Framework languages.

== Main design features

* Back-end modular and based on the concept of service catalogs;

* Communication services is through asynchronous messages and requests for services by customers through HTTP/REST or LDAP;

* Published services are specified in a service catalogd in JSON format;

* Published services can be implemented in Erlang or Java;

* Support HTTP Basic authentication;
 
* Support Lightweight Directory Access Protocol (LDAP v3) authentication (Proxy LDAP);

* Support OAuth2 authentication;

* User, customer, and access profiles are stored externally to the bus to simplify integration with the system used by the organization;


== Installation instructions


To install ErlangMS in the Debian, Ubuntu or CentOS, follow the instructions below:


##1) Download setup

```console
$ wget https://raw.githubusercontent.com/erlangMS/releases/master/setup/setup-emsbus-linux.x86_64.sh
$ chmod +x setup-emsbus-linux.x86_64.sh
```


##2) Run the setup as root or with the sudo command

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


Running instructions
=====


```console
$ sudo systemctl start ems-bus
```

Test instructions
=====


If everything is OK, go to http://localhost:2301/ on your browser.

```console
$ curl http://localhost:2301
*{"message": "It works!!!"}*
```




Implementing a Hello World Service in Java EE
=====

##1) First, you must specify the service contract
```console
{
	"name" : "/samples/hello_world",
	"comment": "Hello World in Java",
	"owner": "samples",
	"version": "1",
	"service" : "br.erlangms.samples.service.HelloWorldFacade:helloWorld",
	"url": "/samples/hello_world",
	"type": "GET",
	"lang" : "java"
}
```

*This contract is saved in the catalog directory of the bus (localized in the folder priv/conf/catalog)*

##2) Service implementation

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

### Details of the architecture

* The architecture provides that the services are implemented according to the design *Domain Driven Design (DDD)* but for simplicity only the facade of the service is displayed here;

* The publication of services in a node depends on the programming language. Java services can be published in a *JBoss or Wildfly* container;

* The services can communicate with each other with any other service on the same node or another node within the cluster transparently;

* When a consumer invokes a service on the bus through a REST request is made the order for the code of the appropriate service at any node in the cluster;

* If more than one node with the same published service, the request is sent to only one node following a round-robin strategy.



##3) Consuming the service

*To execute the specified service can make an HTTP/REST request to the service through your url.*

Exemplifying with the curl utility

```
curl -X GET localhost:2301/samples/hello_world
{"message": "Hello World!!!"}
```

Log data bus

```console
REQUEST ROWID <<"GET#/samples/hello_world">>.
CAST helloworld_facade:execute em puebla {RID: 1457890196200613870, URI: /samples/hello_world}.
GET /samples/hello_world HTTP/1.1 {
        RID: 1457890196200613870
        Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8:
        User-Agent: Mozilla/5.0 (X11; Linux x86_64) Chrome/49.0.2623.87 Safari/537.36
        Content-Type: application/json
        Payload: 
        Service: br.erlangms.samples.service.HelloWorldFacade:helloWorld
        Query: []
        Authorization: 
        Status: 200 <<ok>> (2ms)
        Send: ok
}
```

Communication between services in the cluster
=====

Remember that *consumers* invoke services through REST calls. Moreover, within the cluster, the services(who are also consumers) can communicate with each other asynchronously instead of making REST calls. 

The following example shows two services in two modules: unb_questionario and unb_sae. The second service invokes the first service by calling the *GetStream()* method.


#### Class *PerguntaQuestionarioService* of the module *unb_questionario*

```java
@Stateless
public class PerguntaQuestionarioService {
	
	public Pergunta findById(Integer id) {
		return QuestionarioInfra.getInstance()
			.getPerguntaRepository()
			.findById(id);
	}

	public Pergunta update(Pergunta pergunta){
		pergunta.validar();
		return QuestionarioInfra.getInstance()
			.getPerguntaRepository()
			.update(pergunta);
	}

	public Pergunta insert(Pergunta pergunta) {
		pergunta.validar();
		return QuestionarioInfra.getInstance()
			.getPerguntaRepository()
			.insert(pergunta);
	}
	
	...
```


#### Class *service proxy* of the module *unb_sae* for access *PerguntaQuestionarioService*

```java
@Stateless
public class PerguntaQuestionarioProxy extends EmsServiceProxy {

	public PerguntaVo findById(Integer id){
		return getStream().from("/questionario/pergunta/:id")
				.setParameter(id)
				.request()
				.getObject(PerguntaVo.class);
	}
	
	...
}
```

*Facade classes omitted in this code*


Compiling the project:
=====

Check the wiki below to see how to download the project, compile and configure: <https://github.com/erlangMS/msbus/wiki/Instalar-o-EBS-ErlangMS-msbus>


Project dependencies for the bus
=====

* Erlang R18 - <http://www.erlang.org/download.html>
* jsx - encode/decore JSON <https://github.com/talentdeficit/jsx>


Documentation of functional programming
=====

Documentation on the Erlang

<http://www.erlang.org/>

Excellent online book about distributed programming in Erlang

<http://learnyousomeerlang.com/>

List of articles about programming in Erlang

<https://github.com/0xAX/erlang-bookmarks/blob/master/ErlangBookmarks.md>
