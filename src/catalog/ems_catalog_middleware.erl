-module(ems_catalog_middleware).

-export([onvalidate/2]).

-include("../include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").


onvalidate(Operation, Service) ->
	case unique(Operation, Service) of
		true -> ok;
		false -> {error, ealready_exist}
	end.

unique(update, #service{id = Id, name = Name, url = Url}) ->
	Fun = fun() -> 
				qlc:e(qlc:q([S || S <- mnesia:table(service), 
									(S#service.url == Url orelse S#service.name == Name) 
									  andalso S#service.id =/= Id])) 
		  end,
	case mnesia:activity(transaction, Fun) of
		[] -> true;
		_ -> false
	end;
unique(insert, #service{name = Name, url = Url}) ->
	Fun = fun() -> 
				qlc:e(qlc:q([S || S <- mnesia:table(service), 
									S#service.url == Url orelse	S#service.name == Name])) 
		  end,
	case mnesia:activity(transaction, Fun) of
		[] -> true;
		_ -> false
	end.
	
