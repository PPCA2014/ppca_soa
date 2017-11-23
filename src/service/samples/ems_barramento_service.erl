%%********************************************************************
%% @title Module ems_barramento_service
%% @version 1.0.0
%% @doc It provides information about cluster.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************tn************************************************

-module(ems_barramento_service).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

-export([execute/1]).
  
execute(Request) -> 
	Conf = ems_config:getConfig(),
	ContentData = iolist_to_binary([<<"{"/utf8>>,
										<<"\"ip\":\""/utf8>>, Conf#config.tcp_listen_main_ip, <<"\","/utf8>>, 
										<<"\"http_port\":"/utf8>>, integer_to_binary(2301), <<","/utf8>>, 
										<<"\"https_port\":"/utf8>>, integer_to_binary(2344), <<","/utf8>>, 
										<<"\"base_url\":\""/utf8>>, <<"http://"/utf8>>, Conf#config.tcp_listen_main_ip, <<":"/utf8>>, <<"2301"/utf8>>, <<"\","/utf8>>, 
										<<"\"auth_url\":\""/utf8>>, <<"http://"/utf8>>, Conf#config.tcp_listen_main_ip, <<":"/utf8>>, <<"2301"/utf8>>, <<"/authorize\","/utf8>>, 
										<<"\"auth_protocol\":\"auth2\""/utf8>>, 
									<<"}"/utf8>>]),
	{ok, Request#request{code = 200, 
						 response_data = ContentData}
	}.
