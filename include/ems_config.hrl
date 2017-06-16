%%********************************************************************
%% @title Arquivo de configuração ErlangMS
%% @version 1.0.0
%% @doc Arquivo com configurações gerais de funcionamento de ErlangMS.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-define(DEBUG(Msg), ems_logger:debug(Msg)).
-define(DEBUG(Msg, Params), ems_logger:debug(Msg, Params)).

-ifdef(native_json).
	-define(JSON_LIB, jiffy).
-else.
	-define(JSON_LIB, jsx).
-endif.

-ifdef(win32_plataform).
	-define(UTF8_STRING(Text), ems_util:utf8_string_win(Text)).
-else.
	-define(UTF8_STRING(Text), ems_util:utf8_string_linux(Text)).
-endif.

% Tamanho máximo do payload do POST. Por default é 1M
-define(HTTP_MAX_POST_SIZE, 1024 * 1024 * 1024).

% Nome do servidor
-define(SERVER_NAME, io_lib:format(<<"ems-bus-~s">>, [case application:get_key(ems_bus, vsn) of 
																{ok, Version} -> Version;
																undefined -> "1.0.0"
														end])).


% Caminho do diretório privado
-define(PRIV_PATH, ems_util:get_priv_dir()).

% Caminho do diretório de trabalho
-define(WORKING_PATH, ems_util:get_working_dir()).

% Caminho do catálogo de serviços
-define(CONF_PATH, ?PRIV_PATH ++ "/conf").

% Caminho da pasta log
-define(LOG_PATH, ?PRIV_PATH ++ "/log").

% Caminho do favicon
-define(FAVICON_PATH, ?PRIV_PATH ++ "/favicon.ico").

% Caminho do catálogo de serviços
-define(CATALOGO_PATH, ?PRIV_PATH ++ "/catalog").

% Caminho do catálogo de serviços
-define(CATALOGO_ESB_PATH, ?CATALOGO_PATH  ++ "/catalog.json").

% Caminho da pasta de databases
-define(DATABASE_PATH, ?PRIV_PATH ++ "/db").

% Caminho do arquivo de configuração padrão (Pode ser incluído também na pasta ~/.erlangms do usuário)
-define(CONF_FILE_PATH, ?CONF_PATH ++ "/emsbus.conf").

% Caminho inicial para os arquivos estáticos
-define(STATIC_FILE_PATH, ?PRIV_PATH ++ "/www").

% Caminho inicial para os arquivos estáticos
-define(WEBAPPS_PATH, ?PRIV_PATH ++ "/www").

% Caminho inicial para os arquivos de carga de dados em formato CSV
-define(CSV_FILE_PATH, ?PRIV_PATH ++ "/csv").

% Caminho dos certificados ssl
-define(SSL_PATH, ?PRIV_PATH ++ "/ssl").

% Propriedade TCP Timeout para envio do response
-define(TCP_SEND_TIMEOUT, 30000).

% Number of TCP connections that have completed the SYN/ACK handshake and not yet been accepted by user
-define(TCP_BACKLOG, 128).

% Armazena em cache as novas requisições por REQ_CACHE_SYNC_CHECKPOINT ms antes de persistir no banco
-define(REQ_CACHE_SYNC_CHECKPOINT, 6000).

% Armazena o buffer do log a cada LOG_FILE_CHECKPOINT ms (Aumente este valor se existir muita contenção na escrita em disco)
-define(LOG_FILE_CHECKPOINT, 6000).  

% Tamanho em KB máximo permitido para os arquivos de logs
-define(LOG_FILE_MAX_SIZE, 51200000).  

% Arquiva o log a cada LOG_ARCHIVE_CHECKPOINT ms
-define(LOG_ARCHIVE_CHECKPOINT, 1000 * 60 * 60 * 24).  % Por default são 24 horas

% Quantos workers HTTP instanciar se não especificado no arquivo de configuração
-define(MIN_HTTP_WORKER, 1).

% Quantos workers HTTP instanciar se não especificado no arquivo de configuração
-define(MAX_HTTP_WORKER, 1000).

% Quantos workers HTTP são permitidos especificar no arquivo de configuração (1 até MAX_HTTP_WORKER_RANGE)
-define(MAX_HTTP_WORKER_RANGE, 1000).  % a cada 4 horas

% Quanto tempo o listener vai aguardar uma conexão antes de ocorrer um timeout
-define(TCP_ACCEPT_CONNECT_TIMEOUT, 1000 * 60). % 1 minuto

% Quanto tempo aguardar um serviço
-define(SERVICE_TIMEOUT, 12500). % 12,5 segundos segundos

% Caminho do utilitário que importa dados csv para um banco sqlite
-define(CSV2SQLITE_PATH, ?PRIV_PATH ++ "/scripts/csv2sqlite.py"). 

% Quanto tempo uma parsed query mnesia fica em cache para reutilização (módulo ems_db)
-define(LIFE_TIME_PARSED_QUERY, 60000 * 15). % 15 minutos

% Quanto tempo uma parsed query mnesia fica em cache para reutilização (módulo ems_db)
-define(LIFE_TIME_ODBC_CONNECTION, 60000). % 1 minuto

% Limits of API query
-define(MAX_LIMIT_API_QUERY, 99999).
-define(MAX_OFFSET_API_QUERY, 99999).
-define(MAX_TIME_ODBC_QUERY, 30000).
-define(MAX_ID_RECORD_QUERY, 9999999999).  

% Timeout in ms to expire cache of get request (ems_dispatcher_cache)
-define(TIMEOUT_DISPATCHER_CACHE, 8000).

% Number of datasource entries by odbc connection pool
-define(MAX_CONNECTION_BY_POOL, 5).


% Define the checkpoint to update user for ems_user_loader process
-define(USER_LOADER_UPDATE_CHECKPOINT, 60000).
-define(CLIENT_LOADER_UPDATE_CHECKPOINT, 60000).


%Define the checkpoint to update permission for ems_user_permission_loader process
-define(PERMISSION_LOADRES_UPDATE_CHECKPOINT, 120000).

% HTTP access control (CORS) headers
-define(ACCESS_CONTROL_ALLOW_HEADERS, <<"Accept, Accept-Language, Content-Language, Content-Type, X-ACCESS_TOKEN, X-CSRF-Token, Access-Control-Allow-Origin, Authorization, Origin, x-requested-with, Content-Range, Content-Disposition, Content-Description">>).
-define(ACCESS_CONTROL_MAX_AGE, <<"604800">>).
-define(ACCESS_CONTROL_ALLOW_ORIGIN, <<"*">>).
-define(ACCESS_CONTROL_ALLOW_METHODS, <<"GET, POST, PUT, DELETE, OPTIONS">>).
-define(ACCESS_CONTROL_EXPOSE_HEADERS, <<"Cache-Control, Content-Language, Content-Type, Expires, Last-Modified, Content-Length, ems-catalog, ems_owner, ems_node">>).


-define(AUTHORIZATION_TYPE_DEFAULT, <<"oauth2">>).


-define(CONTENT_TYPE_JSON, <<"application/json; charset=utf-8"/utf8>>).
-define(CACHE_CONTROL_NO_CACHE, <<"no-cache, public">>).
-define(CACHE_CONTROL_1_SECOND, <<"max-age=1, public">>).

% Default ports
-define(LDAP_SERVER_PORT, 2389).
-define(LDAP_MAX_CONNECTIONS, 100000).

-define(HTTP_SERVER_PORT, 2381).
-define(HTTP_MAX_CONNECTIONS, 100000).

-define(TCP_PORT_MIN, 1024).
-define(TCP_PORT_MAX, 99999).


%  Definição para o arquivo de configuração
-record(config, {cat_host_alias, 							%% Lista (Chave-Valor) com os names alternativos para os hosts. Ex.: ["negocio01", "192.168.0.103", "negocio02", "puebla"]
				 cat_host_search,							%% Lista de hosts para pesquisar os serviços
				 cat_node_search,							%% Lista de nodes para pesquisar os serviços
				 cat_path_search :: list(tuple()),			%% Lista de tuplas com caminhos alternativos para catálogos
				 cat_disable_services :: list(binary()),	%% Lista de serviços para desativar
				 cat_enable_services :: list(binary()),		%% Lista de serviços para habilitar
				 static_file_path :: list(string()),		%% Lista de diretórios para arquivos estáticos
				 ems_hostname,								%% Nome da maquina onde o barramento está sendo executado
				 ems_host,									%% Atom do name da maquina onde o barramento está sendo executado
				 ems_file_dest,								%% Nome do arquivo de configuração (útil para saber o local do arquivo)
				 ems_debug,
				 ems_result_cache,
				 ems_datasources :: map(),
				 tcp_listen_address			= [<<"0.0.0.0">>],
				 tcp_allowed_address		= [],
				 authorization :: binary(),
				 oauth2_with_check_constraint :: boolean(),
				 config_file,
				 http_port_offset :: non_neg_integer(),
				 https_port_offset :: non_neg_integer(),
				 http_enable :: boolean(),
				 https_enable :: boolean(),
				 params :: map()
		 }). 	



