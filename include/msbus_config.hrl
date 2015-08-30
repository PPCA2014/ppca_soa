%%********************************************************************
%% @title Arquivo de configuração ErlangMS
%% @version 1.0.0
%% @doc Arquivo com configurações gerais de funcionamento de ErlangMS.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

% Tamanho máximo do payload do POST. Por default é 1M
-define(HTTP_MAX_POST_SIZE, 1024 * 1024 * 1024).

% Nome do servidor
-define(SERVER_NAME, io_lib:format(<<"Erlang Microservices (ErlangMS ~s)">>, [case application:get_key(msbus, vsn) of 
																					{ok, Version} -> Version;
																					undefined -> "1"
																			  end])).

% Caminho do favicon
-define(FAVICON_PATH, code:priv_dir(msbus) ++ "/favicon.ico").

% Caminho do catálogo de serviços
-define(CATALOGO_PATH, code:priv_dir(msbus) ++ "/conf/catalogo.conf").

% Caminho do arquivo de configuração
-define(CONF_FILE_PATH, code:priv_dir(msbus) ++ "/conf/msbus.conf").

% Caminho inicial para os arquivos estáticos
-define(STATIC_FILE_PATH, code:priv_dir(msbus) ++ "/www").

% TCP Timeout para envio do response
-define(TCP_SEND_TIMEOUT, 3000).

%  Armazena o estado do servico. 
-record(config, {tcp_listen_address,
				 tcp_port, 
 				 tcp_keepalive, 
				 tcp_nodelay, 
				 tcp_max_http_worker,			 
				 log_file_dest,
				 log_file_checkpoint,
				 service_names}). 



