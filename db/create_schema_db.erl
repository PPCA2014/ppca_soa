%% ---
%%  create_schema_db
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%  Aluno: Everton de Vargas Agilar (evertonagilar@gmail.com)
%%---

-module(create_schema_db).

-export([start/0]).

-include("../include/db_schema.hrl").

start() ->
	mnesia:start(),
	mnesia:create_table(user, [{attributes, record_info(fields, user)}]),
	mnesia:stop().

	

