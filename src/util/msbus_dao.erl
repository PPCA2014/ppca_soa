%%********************************************************************
%% @title Módulo msbus_dao
%% @version 1.0.0
%% @doc Módulo utilitário para rotinas de persistência e validação.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_dao).

-include("../include/msbus_config.hrl").
-include("../include/msbus_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").


-export([get/2, all/1, insert/1, update/1, delete/2, existe/1, match_object/1]).
-export([is_email_valido/1]).
-export([msg_campo_obrigatorio/2, msg_email_invalido/2, mensagens/1]).
-export([msg_registro_ja_existe/1, msg_registro_ja_existe/2]).


get(RecordType, Id) when is_list(Id) ->
	Id2 = list_to_integer(Id),
	get(RecordType, Id2);

get(RecordType, Id) when is_number(Id) ->
	Query = fun() ->
		mnesia:read(RecordType, Id)
	end,
	case mnesia:transaction(Query) of
		{atomic, []} -> {erro, notfound};
		{atomic, [Record|_]} -> {ok, Record};
		{aborted, _Reason} -> {erro, aborted}
	end;

get(_RecordType, _) -> {erro, notfound}.


all(RecordType) ->
	Query = fun() ->
		  qlc:e(
			 qlc:q([X || X <- mnesia:table(RecordType)])
		  )
	   end,
	{atomic, Records} = mnesia:transaction(Query),
	{ok, Records}.

insert(Record) ->
	RecordType = element(1, Record),
	Id = msbus_sequence:sequence(RecordType),
	Record1 = setelement(2, Record, Id),
	Write = fun() -> mnesia:write(Record1) end,
	mnesia:transaction(Write),
	{ok, Record1}.

update(Record) ->
	Write = fun() -> mnesia:write(Record) end,
	mnesia:transaction(Write),
	ok.

delete(RecordType, Id) when is_list(Id) ->
	Id2 = list_to_integer(Id),
	delete(RecordType, Id2);
	
delete(RecordType, Id) ->
	Delete = fun() -> mnesia:delete({RecordType, Id}) end,
	mnesia:transaction(Delete),
	ok.

%% @doc Verifica se um registro existe
match_object(Pattern) ->	
	{atomic, Records} = mnesia:transaction(fun() -> 
												mnesia:match_object(Pattern) 
										   end),
	Records.


%% @doc Verifica se um registro existe
existe(Pattern) ->	
	case match_object(Pattern) of
		[] -> false;
		_ -> true
	end.


%% Funções para validação de dados

is_email_valido(Value) -> 
	case re:run(Value, "\\b[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,4}\\b") of
		nomatch -> false;
		_ -> true
	end.

%% @doc Retorna mensagem registro já existente
msg_registro_ja_existe(Pattern) ->
	case existe(Pattern) of
		false -> [];
		_ -> <<"Registro já está cadastrado."/utf8>>
	end.

%% @doc Retorna mensagem registro já existente
msg_registro_ja_existe(Pattern, Message) ->
	case existe(Pattern) of
		false -> [];
		_ -> Message
	end.
		
%% @doc Mensagens de campo obrigatório
msg_campo_obrigatorio(NomeCampo, []) -> 
	iolist_to_binary(io_lib:format(<<"Campo não preenchido: '~s'."/utf8>>, [NomeCampo]));
msg_campo_obrigatorio(NomeCampo, <<>>) -> 
	iolist_to_binary(io_lib:format(<<"Campo não preenchido: '~s'."/utf8>>, [NomeCampo]));
msg_campo_obrigatorio(_NomeCampo, _Value) -> [].

%% @doc Mensagem de e-mail inválido
msg_email_invalido(_NomeCampo, []) -> [];
msg_email_invalido(_NomeCampo, Value) -> 
	case is_email_valido(Value) of
		false -> iolist_to_binary(io_lib:format(<<"Email informado é inválido: '~s'."/utf8>>, [Value]));
		_ -> []
	end.

%% @doc Retorna somente mensagens não vazias
mensagens(L) -> lists:filter(fun(X) -> X /= [] end, L).



