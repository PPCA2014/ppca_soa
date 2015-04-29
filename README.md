# PPCA_SOA
Barramento SOA da Turma PPCA 2014

PPCA_SOA é um barramento orientado a serviço desenvolvido nas aulas de Construção de Software da Turma de Engenharia de Software do Mestrado Profissional em Computação Aplicada (MPCA) da Universidade de Brasília. 

É escrito em Erlang e permite manipular grande quantidade de requisições sem perda de performance. O que o torna diferente de outros barramentos SOA é a facilidade de configuração e uso.


60 segundos Quickstart
--------------------

Após download e extrair, digite

```console
./start-server.sh
```

For Windows, type

```console
start-server.bat
```

Então visite http://sistemas.unb.br:2301/about em seu browser. Parabéns, você tem o barramento SOA funcionando. Deve haver um monte de relatórios de progressos em seu console mas tudo deve estar executando de acordo.

O nome do projeto deve ser um atom Erlang (o nome deve iniciar com caracter em caixa baixa). Para facilitar recomenda-se a pasta e o nome do projeto sejam iguais.



Dependências
------------

* Erlang R16B or later -

    <http://www.erlang.org/download.html>

  * Check with `erlang:system_info(otp_release)`.


* On Windows Vista or Windows 7 -

  * Erlang bin directory must be in PATH.


* jiffy - encode/decore de JSON

    <https://github.com/davisp/jiffy>




Configuração do Banco de Dados
--------------

ppca_soa utiliza o Apache CouchDB para armazenamentos dos dados. Para iniciar a configuração, veja README_DATABASE


Filosofia e Recursos
-----------------------

ppca_soa obtêm as vantagens da programação funcional em Erlang ao entregar um barramento orientado a serviço simples e ao mesmo tempo funcional. A melhor parte deste projeto é oferecer comunicação 100% assíncrona quando necessário.




Documentação sobre programação funcional
---------------

Documentação sobre Erlang

<http://www.erlang.org/>

Para quem quiser iniciar na programação Erlang, visite este livro:

http://learnyousomeerlang.com/>



