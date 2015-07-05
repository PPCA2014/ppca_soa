# erlangMS

erlangMS é um Enterprise Service Bus (ESB) que segue uma arquitetura microservices composto por pequenos módulos que implementam serviços bem definidos e independentes, executando em seu próprio processo e se comunicando através dos mecanismos de mensagens da plataforma Erlang/OTP.

O code base inicial foi herdado do projeto PPCA->SOA, desenvolvido no Mestrado em Computação Aplicada da Universidade de Brasília pela Turma de Construção de Software de 2014.

A linguagem de programação e a plataforma escolhida para o projeto foi Erlang R17/OTP. Esta linguagem possui fortes características funcionais e um ambiente de execução para criação de aplicações distribuídas, tolerante a falhas e altamente escalável.

A principal interface de comunicação de erlangMS é REST com JSON, embora possa se comportar também como um servidor Web convencional através dos seus módulos de serviços para gerenciamento de arquivos.


Como iniciar erlangMS
-----------------------

Se estiver no Linux, digite:

```console
./start-server.sh
```

Caso estiver no Windows, digite:

```console
start-server.bat
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
