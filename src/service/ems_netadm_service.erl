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
	{ok, ContentData} = net_adm:names(),
	{ok, Request#request{code = 200, 
						 response_data = ems_util:json_encode(ContentData)}
	}.

world(Request) -> 
	ContentData = [ atom_to_list(R) || R <- net_adm:world() ],
	{ok, Request#request{code = 200, 
						 response_data = ems_util:json_encode(ContentData)}
	}.

hostfile(Request) -> 
	{ok, Request#request{code = 200, 
						 response_data = ems_util:json_encode(net_adm:host_file())}
	}.
	
hostname(Request) -> 
	ContentData = {ok, net_adm:localhost()},
	{ok, Request#request{code = 200, 
						 response_data = ContentData}
	}.
	
