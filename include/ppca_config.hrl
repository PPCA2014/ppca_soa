%%********************************************************************
%% @title Arquivo de configuração
%% @version 1.0.0
%% @doc Arquivo com configurações gerais de funcionamento.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************


% Seta a porta default do barramento SOA
-define(CONF_PORT, 2301).

% Seta o tamanho máximo do payload do POST. Por default é 1M.
-define(HTTP_MAX_POST_SIZE, 1024 * 1024 * 1024).

% Nome do servidor
-define(SERVER_NAME, <<"erlangMS 1.0">>).

% Local onde está o favicon do projeto
-define(FAVICON_PATH, "./img/favicon.ico").

% Local onde está o catálogo de serviços
-define(CATALOGO_PATH, "./conf/catalogo.json").

% Caminho inicial para os arquivos estáticos
-define(STATIC_FILE_PATH, "./").


% Configurações para o logger do PPCA_SOA
-record(logger, {%% nome do arquivo do logger
				 filename="logs/server.log",   
				 
				 %% de quanto em quanto tempo vai descarregar o buffer do log em disco
				 checkpoint_timeout = 6000,  %% 6 segundos
				 
				 %% de quanto em quanto tempo vai rotacionar o arquivo. 
				 rotacao_timeout = 1000 * 60 * 60 * 24   %% 24h
				}).

-record(request, {%% cabeçalho da requisição HTTP
				  http_headers = [],
				  %% The raw HTTP request body
				  payload = [],
				  %% querystring
				  querystring = [],
				  %% parâmetros na url
				  params_url = [],
				  %% serviço que representa esta requisição
				  servico = null
				  }).
				  	




	
     
     
     	




