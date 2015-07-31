%%********************************************************************
%% @title Arquivo de configuração
%% @version 1.0.0
%% @doc Arquivo com configurações gerais de funcionamento.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************


% Porta default do barramento SOA
-define(CONF_PORT, 2301).

% Tamanho máximo do payload do POST. Por default é 1M
-define(HTTP_MAX_POST_SIZE, 1024 * 1024 * 1024).

% Nome do servidor
-define(SERVER_NAME, <<"erlang Microservices (erlangMS 1.0)">>).

% Caminho do favicon
-define(FAVICON_PATH, "./img/favicon.ico").

% Caminho do catálogo de serviços
-define(CATALOGO_PATH, "./conf/catalogo.json").

% Caminho inicial para os arquivos estáticos
-define(STATIC_FILE_PATH, "./").

% De quanto em quanto tempo vai descarregar o buffer do módulo msbus_health
-define(HEALTH_CHECKPOINT, 6000). %% 6 segundos

% Configurações para o log de operações
-record(logger, {%% nome do arquivo do logger
				 filename="logs/server.log",   
				 
				 %% de quanto em quanto tempo vai descarregar o buffer do log em disco
				 checkpoint_timeout = 6000,  %% 6 segundos
				 
				 %% de quanto em quanto tempo vai rotacionar o arquivo. 
				 rotacao_timeout = 1000 * 60 * 60 * 24   %% 24h
				}).
     
     	



