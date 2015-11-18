ErlangMS
=====

ErlangMS é um Enterprise Service Bus (ESB) desenvolvido por Everton de Vargas Agilar e visa facilitar a integração de sistemas legados através de uma abordagem orientada a serviço.

O projeto é resultado de esforços para obter uma ferramenta simples e que fosse orientado a contrato de serviços para sustentar uma abordagem de modernização de software ágil.

###As principais características do projeto são

* multiplataforma (desenvolvido na linguagem Erlang/OTP) 

* estilo arquitetural RESTful

* orientado a contratos de serviços no formato JSON

* representação de dados em JSON
 
* implementação dos serviços independente de linguagem de programação


Veja a arquitetura do barramento em https://github.com/erlangMS/msbus/blob/master/doc/arquitetura_erlangms.pdf


Como executar ErlangMS
-----------------------

Se o projeto já estiver instalado e configurado, execute os comandos a seguir, de acordo com o sistema operacional:

Se estiver no Linux, digite:

```console
./start.sh
INFO 1/9/2015 21:11:30  Erlang Microservices (ErlangMS 1.0)
INFO 1/9/2015 21:11:30  Inicializando o pool de módulos:
INFO 1/9/2015 21:11:30     Módulo msbus_eventmgr com 1 worker.
INFO 1/9/2015 21:11:30     Módulo msbus_catalogo com 2 workers.
INFO 1/9/2015 21:11:30     Módulo msbus_user com 2 workers.
INFO 1/9/2015 21:11:30     Módulo msbus_cache com 1 worker.
INFO 1/9/2015 21:11:30     Módulo msbus_server com 1 worker.
INFO 1/9/2015 21:11:30     Módulo msbus_request com 2 workers.
INFO 1/9/2015 21:11:30     Módulo msbus_server_worker com 12 workers.
INFO 1/9/2015 21:11:30     Módulo msbus_health com 6 workers.
INFO 1/9/2015 21:11:30     Módulo msbus_dispatcher com 12 workers.
INFO 1/9/2015 21:11:30     Módulo msbus_static_file_service com 12 workers.
INFO 1/9/2015 21:11:30     Módulo msbus_user_service com 2 workers.
INFO 1/9/2015 21:11:30     Módulo msbus_catalogo_service com 2 workers.
INFO 1/9/2015 21:11:30     Módulo msbus_info_service com 2 workers.
INFO 1/9/2015 21:11:30     Módulo msbus_favicon_service com 2 workers.
INFO 1/9/2015 21:11:30     Módulo msbus_option_service com 1 worker.
INFO 1/9/2015 21:11:30     Módulo msbus_health_service com 6 workers.
INFO 1/9/2015 21:11:31  Escutando no endereço 127.0.0.1:2301.
INFO 1/9/2015 21:11:31  Portal ErlangMS Api Management em http://127.0.0.1:2301/portal/index.html
INFO 1/9/2015 21:11:31  ErlangMS iniciado em 326ms.
```

Caso estiver no Windows, digite:

```console
start.bat
INFO 1/9/2015 21:11:30  Erlang Microservices (ErlangMS 1.0)
INFO 1/9/2015 21:11:30  Inicializando o pool de módulos:
INFO 1/9/2015 21:11:30     Módulo msbus_eventmgr com 1 worker.
INFO 1/9/2015 21:11:30     Módulo msbus_catalogo com 2 workers.
INFO 1/9/2015 21:11:30     Módulo msbus_user com 2 workers.
INFO 1/9/2015 21:11:30     Módulo msbus_cache com 1 worker.
INFO 1/9/2015 21:11:30     Módulo msbus_server com 1 worker.
INFO 1/9/2015 21:11:30     Módulo msbus_request com 2 workers.
INFO 1/9/2015 21:11:30     Módulo msbus_server_worker com 12 workers.
INFO 1/9/2015 21:11:30     Módulo msbus_health com 6 workers.
INFO 1/9/2015 21:11:30     Módulo msbus_dispatcher com 12 workers.
INFO 1/9/2015 21:11:30     Módulo msbus_static_file_service com 12 workers.
INFO 1/9/2015 21:11:30     Módulo msbus_user_service com 2 workers.
INFO 1/9/2015 21:11:30     Módulo msbus_catalogo_service com 2 workers.
INFO 1/9/2015 21:11:30     Módulo msbus_info_service com 2 workers.
INFO 1/9/2015 21:11:30     Módulo msbus_favicon_service com 2 workers.
INFO 1/9/2015 21:11:30     Módulo msbus_option_service com 1 worker.
INFO 1/9/2015 21:11:30     Módulo msbus_health_service com 6 workers.
INFO 1/9/2015 21:11:31  Escutando no endereço 127.0.0.1:2301.
INFO 1/9/2015 21:11:31  Portal ErlangMS Api Management em http://127.0.0.1:2301/portal/index.html
INFO 1/9/2015 21:11:31  ErlangMS iniciado em 326ms.
```

Se estiver tudo Ok, visite http://localhost:2301/hello_world em seu browser.

Obs.: Veja como baixar o projeto, compilar, configurar e fazer deploy na wiki https://github.com/erlangMS/msbus/wiki/Instalar-o-Barramento-ErlangMS-msbus.


###Dependências do projeto
------------------------

* Erlang R17B ou versão mais recente -

    <http://www.erlang.org/download.html>

  * Verifique com `erlang:system_info(otp_release)`.


* No Windows -

  * Erlang e Rebar bin devem estar na variável PATH.


* jsx - encode/decore JSON

    <https://github.com/talentdeficit/jsx>


###Documentação sobre programação funcional
-----------------------------------------

Documentação sobre Erlang

<http://www.erlang.org/>

Para quem quiser iniciar na programação Erlang, visite este livro online:

<http://learnyousomeerlang.com/>

Lista de artigos sobre Erlang

<https://github.com/0xAX/erlang-bookmarks/blob/master/ErlangBookmarks.md>
