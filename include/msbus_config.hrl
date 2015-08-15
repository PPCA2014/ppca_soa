%%********************************************************************
%% @title Arquivo de configuração ErlangMS
%% @version 1.0.0
%% @doc Arquivo com configurações gerais de funcionamento de ErlangMS.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************


% Porta default do barramento SOA
-define(CONF_PORT, 2301).

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

% Caminho inicial para os arquivos estáticos
-define(STATIC_FILE_PATH, code:priv_dir(msbus) ++ "/www").

% Caminho inicial para os arquivos estáticos
-define(STATIC_FILE_RESULT_CACHE, 2592000000). %% 30 dias

% De quanto em quanto tempo vai descarregar o buffer do módulo msbus_health
-define(HEALTH_CHECKPOINT, 6000). %% 6 segundos

% Tamanho do pool de workers para a função gen_tcp:accept do módulo HTTP
-define(TCP_MAX_HTTP_WORKER, 50).

% TCP Timeout para envio do response
-define(TCP_SEND_TIMEOUT, 8000).

% Ativa fluxo keepalive do TCP
-define(TCP_KEEPALIVE, true).

% Ativa a propriedade no_delay do TCP
-define(TCP_NODELAY, true).

% Configurações para o log de operações
-record(logger, {%% nome do arquivo do logger
				 filename="logs/server.log",   
				 
				 %% de quanto em quanto tempo vai descarregar o buffer do log em disco
				 checkpoint_timeout = 6000,  %% 6 segundos
				 
				 %% de quanto em quanto tempo vai rotacionar o arquivo. 
				 rotacao_timeout = 1000 * 60 * 60 * 24   %% 24h
				}).
     
     	



