# PPCA->SOA
Barramento SOA da Turma de Construção de Software do Mestrado em Computação Aplicada da Universidade de Brasília.

PPCA->SOA é um barramento orientado a serviço que está sendo desenvolvido nas aulas de Construção de Software. 

A linguagem de programação escolhida para o projeto foi Erlang, uma linguagem funcional e um ambiente de execução para criação de aplicações distribuídas, altamente escalável.

O projeto tem um propósito puramente acadêmico. Seu ojetivo principal é permitir invocar serviços no barramento, implementados através de módulos Erlang, mas que futuramente possam ser implementados também em outras linguagens, como java. 

O barramento aceita requisições ao estilo REST e somente é suportado dados no formato JSON.


60 segundos Quickstart
--------------------

Para iniciar o PPCA_SOA no Linux:

```console
./start-server.sh
```

No Windows, digite:

```console
start-server.bat
```

Se estiver tudo Ok, visite http://localhost:2301/hello_world em seu browser. Parabéns, o barramento SOA estará respondendo suas requisições na porta 2301.



Dependências
------------

* Erlang R16B or later -

    <http://www.erlang.org/download.html>

  * Check with `erlang:system_info(otp_release)`.


* On Windows Vista or Windows 7 -

  * Erlang e Rebar bin devem estar no diretório PATH.


* jsx - encode/decore JSON

    <https://github.com/talentdeficit/jsx>


Documentação sobre programação funcional
-----------------------------------------

Documentação sobre Erlang

<http://www.erlang.org/>

Para quem quiser iniciar na programação Erlang, visite este livro:

http://learnyousomeerlang.com/>


Bons estudos e boa programação!!!

Att.
Everton de Vargas Agilar

Mestrando em Computação Aplicada - Turma PPCA 2014

Universidade de Brasília

2015 / Brasília / DF

