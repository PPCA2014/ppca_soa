%%********************************************************************
%% @title Module ems_consist
%% @version 1.0.0
%% @doc Utility module for validation routines and consistency.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(ems_consist).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([is_email_valido/1, is_range_valido/3]).
-export([msg_campo_obrigatorio/2, msg_email_invalido/2, mensagens/1]).
-export([msg_registro_ja_existe/1, msg_registro_ja_existe/2]).


%% *********** Functions for data validation ************

is_range_valido(Number, RangeIni, RangeFim) when Number >= RangeIni andalso Number =< RangeFim -> true;
is_range_valido(_Number, _RangeIni, _RangeFim) -> false.


is_email_valido(Value) -> 
	case re:run(Value, "\\b[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,4}\\b") of
		nomatch -> false;
		_ -> true
	end.

%% @doc Retorna mensagem registro já existente
msg_registro_ja_existe(Pattern) ->
	case ems_db:existe(Pattern) of
		false -> [];
		_ -> <<"Registro já está cadastrado."/utf8>>
	end.

%% @doc Retorna mensagem registro já existente
msg_registro_ja_existe(Pattern, Message) ->
	case ems_db:existe(Pattern) of
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

     
     

