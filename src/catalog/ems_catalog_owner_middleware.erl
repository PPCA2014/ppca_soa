-module(ems_catalog_owner_middleware).

-export([onvalidate/2]).

-include("../include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").


onvalidate(Operation, ServiceOwner) ->
	case unique(Operation, ServiceOwner) of
		true -> ok;
		false -> {error, ealready_exist}
	end.

unique(update, #service_owner{id = Id, name = Name, title = Title}) ->
	Fun = fun() -> 
				qlc:e(qlc:q([S || S <- mnesia:table(service_owner), 
									(S#service_owner.title == Title orelse S#service_owner.name == Name) 
									  andalso S#service_owner.id =/= Id])) 
		  end,
	case mnesia:activity(transaction, Fun) of
		[] -> true;
		_ -> false
	end;
unique(insert, #service_owner{name = Name, title = Title}) ->
	Fun = fun() -> 
				qlc:e(qlc:q([S || S <- mnesia:table(service_owner), 
									S#service_owner.title == Title orelse	S#service_owner.name == Name])) 
		  end,
	case mnesia:activity(transaction, Fun) of
		[] -> true;
		_ -> false
	end.
	
