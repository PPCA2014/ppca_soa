%%********************************************************************
%% @title Arquivo de configuração ErlangMS
%% @version 1.0.0
%% @doc Arquivo com configurações gerais de funcionamento de ErlangMS.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

%-ifndef(PRINT).
%-define(PRINT(Var), io:format("DEBUG: ~p", [??Var, Var])).
%-endif.

% Tamanho máximo do payload do POST. Por default é 1M
-define(HTTP_MAX_POST_SIZE, 1024 * 1024 * 1024).

% Nome do servidor
-define(SERVER_NAME, io_lib:format(<<"ErlangMS Development Version ~s">>, [case application:get_key(ems_bus, vsn) of 
																					{ok, Version} -> Version;
																					undefined -> "1.0"
																			end])).


% Caminho do diretório privado
-define(PRIV_PATH, ems_util:get_priv_dir()).

% Caminho do diretório de trabalho
-define(WORKING_PATH, ems_util:get_working_dir()).

% Caminho do catálogo de serviços
-define(CONF_PATH, ?PRIV_PATH ++ "/conf").

% Caminho do favicon
-define(FAVICON_PATH, ?PRIV_PATH ++ "/favicon.ico").

% Caminho do catálogo de serviços
-define(CATALOGO_PATH, ?PRIV_PATH ++ "/catalog").

% Caminho da pasta de databases
-define(DATABASE_PATH, ?PRIV_PATH ++ "/db").

% Caminho do arquivo de configuração padrão (Pode ser incluído também na pasta ~/.erlangms do usuário)
-define(CONF_FILE_PATH, ?CONF_PATH ++ "/emsbus.conf").

% Caminho inicial para os arquivos estáticos
-define(STATIC_FILE_PATH, ?PRIV_PATH ++ "/www").

% Caminho inicial para os arquivos de carga de dados em formato CSV
-define(CSV_FILE_PATH, ?PRIV_PATH ++ "/csv").

% Propriedade TCP Timeout para envio do response
-define(TCP_SEND_TIMEOUT, 30000).

% Number of TCP connections that have completed the SYN/ACK handshake and not yet been accepted by user
-define(TCP_BACKLOG, 128).

% Armazena em cache as novas requisições por REQ_CACHE_SYNC_CHECKPOINT ms antes de persistir no banco
-define(REQ_CACHE_SYNC_CHECKPOINT, 6000).

% Armazena o buffer do log a cada LOG_FILE_CHECKPOINT ms (Aumente este valor se existir muita contenção na escrita em disco)
-define(LOG_FILE_CHECKPOINT, 6000).  

% Arquiva o log a cada LOG_ARCHIVE_CHECKPOINT ms
-define(LOG_ARCHIVE_CHECKPOINT, 10000 * 60 * 60 * 4).  % Por default são 4 horas


% Quantos workers HTTP instanciar se não especificado no arquivo de configuração
-define(MIN_HTTP_WORKER, 1).

% Quantos workers HTTP instanciar se não especificado no arquivo de configuração
-define(MAX_HTTP_WORKER, 1000).

% Quantos workers HTTP são permitidos especificar no arquivo de configuração (1 até MAX_HTTP_WORKER_RANGE)
-define(MAX_HTTP_WORKER_RANGE, 1000).  % a cada 4 horas

% Quanto tempo o listener vai aguardar uma conexão antes de ocorrer um timeout
-define(TCP_ACCEPT_CONNECT_TIMEOUT, 1000). % 1 minuto

% Caminho do utilitário que importa dados csv para um banco sqlite
-define(CSV2SQLITE_PATH, ?WORKING_PATH ++ "/bin/csv2sqlite.py"). 

% Caminho do banco de dados sqlite
-define(DATABASE_SQLITE_PATH, ?DATABASE_PATH ++ "/ems_dynamic_view.dat").	

% String de conexão do banco de dados sqlite 
-define(DATABASE_SQLITE_STRING_CONNECTION, lists:flatten(io_lib:format("DRIVER=SQLite;Version=3;Database=~s;", [?DATABASE_SQLITE_PATH]))).	

% Quanto tempo uma parsed query mnesia fica em cache para reutilização (módulo ems_db)
-define(LIFE_TIME_PARSED_QUERY, 60000 * 15). % 15 minutos

% Quanto tempo uma parsed query mnesia fica em cache para reutilização (módulo ems_db)
-define(LIFE_TIME_ODBC_CONNECTION, 60000). % 1 minuto

% Limits of API query
-define(MAX_LIMIT_API_QUERY, 99999).
-define(MAX_OFFSET_API_QUERY, 99999).
-define(MAX_TIME_ODBC_QUERY, 30000).
-define(MAX_ID_RECORD_QUERY, 9999999999).  


%  Definição para o arquivo de configuração
-record(config, {tcp_listen_address,    		%% Quais IPs das interfaces de rede que o barramento vai ouvir
				 tcp_listen_address_t,			%% Quais IPs das interfaces de rede que o barramento vai ouvir (formato de tupla para inet)
				 tcp_port, 						%% Qual a porta que será utilizada para o barramento
 				 tcp_keepalive, 				%% Propriedade keepalive do TCP (true/false)
				 tcp_nodelay, 					%% Propriedade nodelay do TCP (true/false)
				 tcp_max_http_worker,			%% Quantos workers serão criados para cada listener
				 tcp_allowed_address,			%% Faixa de ips que são permitidos acessar os serviços do barramento
				 tcp_allowed_address_t,			%% Faixa de ips que são permitidos acessar os serviços do barramento (formato de tupla para inet)
				 log_file_dest,					%% Caminho para a pasta dos logs do barramento
				 log_file_checkpoint,			%% De quanto em quanto tempo será descarregado os buffers do módulo msbus_logger (DEFAULT 6 segundos)
				 cat_host_alias, 				%% Lista (Chave-Valor) com os names alternativos para os hosts. Ex.: ["negocio01", "192.168.0.103", "negocio02", "puebla"]
				 cat_host_search,				%% Lista de hosts para pesquisar os serviços
				 cat_node_search,				%% Lista de nodes para pesquisar os serviços
				 ems_hostname,					%% Nome da maquina onde o barramento está sendo executado
				 ems_host,						%% Atom do name da maquina onde o barramento está sendo executado
				 ems_file_dest,					%% Nome do arquivo de configuração (útil para saber o local do arquivo)
				 ems_debug,						%% Modo debug (true/false)
				 ldap_tcp_port, 				%% ldap tcp port
				 ldap_datasource = "",			%% ldap datasource
				 ldap_sql_find_user = "",		%% sql to find user
				 ldap_admin = "",				%% admin of ldap
				 ldap_password_admin = ""		%% password of admin ldap
		 }). 	



%  Definição para o arquivo de configuração
-record(tcp_config, {tcp_listen_address,    		%% Quais IPs das interfaces de rede que o barramento vai ouvir
					 tcp_listen_address_t,			%% Quais IPs das interfaces de rede que o barramento vai ouvir (formato de tupla para inet)
					 tcp_port, 						%% Qual a porta que será utilizada para o barramento
					 tcp_keepalive, 				%% Propriedade keepalive do TCP (true/false)
					 tcp_nodelay, 					%% Propriedade nodelay do TCP (true/false)
					 tcp_max_http_worker,			%% Max workers criados para cada listener
					 tcp_min_http_worker,			%% Min workers criados para cada listener
					 tcp_accept_timeout,			%% Timeout accept
					 tcp_send_timeout,				
					 tcp_backlog,
					 tcp_buffer,
					 tcp_delay_send,
					 tcp_allowed_address,			%% Faixa de ips que são permitidos acessar os serviços do barramento
					 tcp_allowed_address_t
			 }). 	


