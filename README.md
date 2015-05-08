# PPCA->SOA

PPCA->SOA é um barramento orientado a serviço da Turma de Construção de Software do Mestrado em Computação Aplicada da Universidade de Brasília.

A linguagem de programação escolhida para o projeto foi Erlang, uma linguagem funcional e um ambiente de execução para criação de aplicações distribuídas, tolerante a falhas e altamente escalável.

O projeto tem propósito acadêmico. Seu objetivo é permitir invocar serviços no barramento, implementados através de módulos Erlang, mas que futuramente possam ser implementados em outras linguagens, como Java. 

O barramento aceita requisições ao estilo REST e suporta dados no formato JSON. Um requisito básico do projeto é evitar o uso de bibliotecas de terceiros, para maximar o aprendizado na plataforma Erlang.


Executando o barramento
-----------------------

Para iniciar o PPCA_SOA no Linux:

```console
./start-server.sh
```

No Windows, digite:

```console
start-server.bat
```

Se estiver tudo Ok, visite http://localhost:2301/hello_world em seu browser. Parabéns, o barramento SOA estará respondendo suas requisições na porta 2301.

Obs.: Antes de executar, primeiro deve-se fazer o build do projeto. Consulte a wiki "Getting-Started" em <https://github.com/PPCA2014/ppca_soa/wiki/Getting-Started>.


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
