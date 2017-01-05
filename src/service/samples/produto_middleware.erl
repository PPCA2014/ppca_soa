-module(produto_middleware).

-export([onvalidate/2]).

-include("../include/ems_schema.hrl").

onvalidate(Operation, Produto = #produto{price = Price}) ->
	case unique(Operation, Produto) of
		true ->
			case Price > 99999 of
				true -> {error, {preco_invalido, Price}};
				_ -> ok
			end;
		false -> {error, ealready_exist}
	end.

unique(update, #produto{id = Id, name = Name}) ->
	case ems_db:find_first(produto, [{id, "=/=", Id}, {name, "==", Name}]) of
		{error, enoent} -> true;
		_ -> false
	end;
unique(insert, #produto{name = Name}) ->
	case ems_db:find_first(produto, [{name, "==", Name}]) of
		{error, enoent} -> true;
		_ -> false
	end.
	
