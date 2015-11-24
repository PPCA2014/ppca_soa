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
-define(CONF_PATH, code:priv_dir(msbus) ++ "/conf").

% Caminho do catálogo de serviços
-define(CATALOGO_PATH, ?CONF_PATH ++ "/catalogo.conf").

% Caminho do arquivo de configuração
-define(CONF_FILE_PATH, ?CONF_PATH ++ "/msbus.conf").

% Caminho inicial para os arquivos estáticos
-define(STATIC_FILE_PATH, code:priv_dir(msbus) ++ "/www").

% Propriedade TCP Timeout para envio do response
-define(TCP_SEND_TIMEOUT, 3000).

%  Definição para o arquivo de configuração
-record(config, {tcp_listen_address,    		%% Quais interfaces de rede que o barramento vai ouvir (Permitido informar o IP ou DNS Name)
				 tcp_port, 						%% Qual a porta que será utilizada para o barramento
 				 tcp_keepalive, 				%% Propriedade keepalive do TCP
				 tcp_nodelay, 					%% Propriedade nodelay do TCP
				 tcp_max_http_worker,			%% Quantos workers serão criados para cada listener
				 log_file_dest,					%% Caminho para a pasta dos logs do barramento
				 log_file_checkpoint,			%% De quanto em quanto tempo será descarregado os buffers do módulo msbus_logger (DEFAULT 6 segundos)
				 cat_host_alias, 				%% Lista (Chave-Valor) com os nomes alternativos para os hosts. Ex.: ["negocio01", "192.168.0.103", "negocio02", "puebla"]
				 cat_host_search,				%% Lista de hosts para pesquisar os serviços
				 cat_node_search,				%% Lista de nodes para pesquisar os serviços
				 msbus_hostname,				%% Nome da maquina onde o barramento está sendo executado
				 msbus_host						%% Atom do nome da maquina onde o barramento está sendo executado
		 }). 	



