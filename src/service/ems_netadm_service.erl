%%********************************************************************
%% @title Module ems_netadm_service
%% @version 1.0.0
%% @doc It provides information about cluster.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************tn************************************************

-module(ems_netadm_service).

-include("../include/ems_schema.hrl").

-export([names/1, world/1, hostfile/1, hostname/1]).
  
names(Request) -> 
	ContentData = case net_adm:names() of
		{ok, Names} -> ems_util:json_encode(Names);
		Error -> Error
	end,
	{ok, Request#request{code = 200, 
						 response_data = ContentData}
	}.

world(Request) -> 
	try
		ContentData = [ atom_to_list(R) || R <- net_adm:world() ],
		{ok, Request#request{code = 200, 
							 response_data = ems_util:json_encode(ContentData)}
		}
	catch 
		_Exception:_Reason -> 
			{ok, Request#request{code = 200, 
								 response_data = {error, enoent}}
			}
	end.

hostfile(Request) -> 
	ContentData = net_adm:host_file(),
	{ok, Request#request{code = 200, 
						 response_data = ems_util:json_encode(ContentData)}
	}.

hostname(Request) -> 
	ContentData = {ok, net_adm:localhost()},
	{ok, Request#request{code = 200, 
						 response_data = ContentData}
	}.
	
