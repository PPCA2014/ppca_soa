# erlangMS

erlangMS é um Enterprise Service Bus (ESB) que segue uma arquitetura Microservices composto por pequenos módulos que implementam serviços bem definidos e independentes, executam em seu próprio processo e se comunicam através dos mecanismos de mensagens da plataforam Erlang/OTP.

O code base inicial foi herdado do projeto PPCA->SOA, desenvolvido no Mestrado em Computação Aplicada da Universidade de Brasília pela Turma de Construção de Software de 2014.

A linguagem de programação e plataforma escolhida para o projeto foi Erlang R17/OTP. Esta linguagem possui características funcionais e possui um ambiente de execução para criação de aplicações distribuídas, tolerante a falhas e altamente escalável.

A principal interface de comunicação de erlangMS é REST, embora possa também se comportar como um servidor HTTP convencional, através dos seus módulos de serviços para gerenciamento de arquivos.


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

Obs.: Veja como baixar o projeto, compilar e fazer deploy na Wiki do projeto no link  <https://github.com/PPCA2014/ppca_soa/wiki/Getting-Started>.


Dependências
------------

* Erlang R17B ou versão mais recente -

    <http://www.erlang.org/download.html>

  * Verifique com `erlang:system_info(otp_release)`.


* On Windows Vista or Windows 7 -

  * Erlang e Rebar bin devem estar no diretório PATH.


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


Bons estudos e boa programação!!!

```
Att.
Everton de Vargas Agilar
Arquiteto da Turma de Construção de Software
Mestrando em Computação Aplicada - Turma PPCA 2014
Universidade de Brasília
2015 / Brasília / DF
```
