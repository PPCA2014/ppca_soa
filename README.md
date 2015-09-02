ErlangMS
=====

ErlangMS é um Enterprise Service Bus (ESB) que segue uma arquitetura microservices composto por módulos que implementam serviços bem definidos e independentes.

Erlang/OTP é a linguagem/plataforma escolhida para o projeto. A linguagem possui características funcionais e um ambiente de execução para criação de aplicações distribuídas, tolerante a falhas e altamente escalável.

O projeto está sendo desenvolvido como projeto de mestrado e visa a integração de sistemas através de uma abordagem orientada a serviço.

A interface de comunicação segue o estilo arquitetural REST e a representação de dados é em JSON.

Veja a arquitetura do barramento em https://github.com/erlangMS/msbus/blob/master/doc/arquitetura_erlangms.pdf


Como executar ErlangMS
-----------------------

Se estiver no Linux, digite:

```console
./start-server.sh
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
start-server.bat
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

Obs.: Veja como baixar o projeto, compilar e fazer deploy na Wiki do projeto.


Dependências do projeto
------------------------

* Erlang R17B ou versão mais recente -

    <http://www.erlang.org/download.html>

  * Verifique com `erlang:system_info(otp_release)`.


* No Windows -

  * Erlang e Rebar bin devem estar na variável PATH.


* jsx - encode/decore JSON

    <https://github.com/talentdeficit/jsx>


Documentação sobre programação funcional
-----------------------------------------

Documentação sobre Erlang

<http://www.erlang.org/>

Para quem quiser iniciar na programação Erlang, visite este livro online:

<http://learnyousomeerlang.com/>

Lista de artigos sobre Erlang

<https://github.com/0xAX/erlang-bookmarks/blob/master/ErlangBookmarks.md>
