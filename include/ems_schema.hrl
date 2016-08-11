%%********************************************************************
%% @title ems_schema
%% @version 1.0.0
%% @doc It contains definitions of the data structures used.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************


-record(sequence, {key :: atom(), 
				   index :: non_neg_integer()}).

-record(user, {id :: non_neg_integer(), 
			   name :: string(), 
			   email :: string(), 
			   password :: string()}).
			   
-record(request, {
					  rid,       								%% Request ID (Identificador da requisição gerada automaticamente)
					  rowid,									%% Identificador interno da requisição
					  service,   								%% Contrato que estabelece o serviço que vai atender a requisição
					  timestamp, 								%% Timestamp de quando que a requisição ocorreu
					  latency :: non_neg_integer(),				%% Latência (tempo que levou para processar a requisição)
					  code, 	   								%% Código de retorno HTTP (Ex.: 202 OK, 404 Não Encontrado)
					  reason,									%% Registra a mensagem de erro, quando status indicar um erro
					  type :: string(),							%% Verbo HTTP (GET, POST, PUT, DELETE e OPTIONS)
					  uri :: string(),							%% URI da requisição do serviço
					  url :: string(),							%% URL da requisição do serviço
					  version :: string(),						%% Versão do cabeçalho HTTP
					  payload :: string(),						%% Corpo da requisição (aceita somente JSON)
					  payload_map :: map(),						%% Corpo da requisição convertida para map após o parser e validação
					  querystring :: string(),					%% Querystring da requisição
					  querystring_map,							%% Querystring convertida para map após o parser e validação
					  params_url,								%% Map com os parâmetros da URL
					  content_length :: non_neg_integer(), 		%% Largura da requisição
					  content_type :: string(),					%% Tipo de conteúdo (Ex.: application/json)
					  accept :: string(),						%% Parâmetro ACCEPT HTTP
					  user_agent :: string(),					%% Parâmetro USER_AGENT HTTP
					  accept_encoding :: string(),				%% Parâmetro ACCEPT_ENCODING HTTP
					  cache_control :: string(),				%% Parâmetro CACHE-CONTROL HTTP
					  host :: string(),							%% Host que iniciou a requisição
					  t1,										%% Utilizado para cálculo da latência (Tempo inicial em milisegundos)
					  socket :: gen_tcp:socket(),				%% Socket da requisição
					  worker :: pid(),							%% Processo worker http que vai atender a requisição
					  status_send,								%% Registra que a mensagem foi entregue ou o erro ocorrido na entrega
					  authorization :: string(),				%% Dados da autenticação da requisição
					  user :: user,								%% Usuário da requisição ou anonimo
					  node_exec = undefined,					%% Node que foi enviado a solicitação
					  status = latency,							%% status: latency, req_done, req_send
					  worker_send,
					  protocol = http							%% Protocol (http, ldap)
				  }).


-record(service_datasource, {type :: string(),
							 connection :: string(),
							 table_name :: string(),
							 primary_key :: string(),
							 conn_ref,
							 pid_module :: string(),
							 csv_delimiter :: string()
							}).

-record(service, {
					rowid,				  						%% Identificador interno do contrato (utilizado para localizar o contrato)
					id :: non_neg_integer(), 					%% Id sequencial gerado automaticamente e visível no portal API Management
					name :: string(), 							%% Nome do contrato do serviço (Por default usa-se a própria URL como name)
					url :: string(),  							%% URL do contrato do serviço
					type :: string(),							%% Verbo HTTP do contrato (GET, POST, PUT, DELETE e OPTIONS)
					service :: string(),						%% Serviço que será executado no contrato
					module_name :: string(), 					%% Nome do módulo do serviço que vai atender a requisição. Ex.: br.erlangms.HelloWorldService  
					module_name_canonical :: string(), 			%% Nome do módulo canonico do serviço que vai atender a requisição. Ex.: HelloWorldService  
					module :: atom(),  							%% Atom do processo do módulo de serviço que vai atender a requisição
					function_name :: string(),					%% Nome da mensagem ou função que vai ser invocada no processo que vai atender a requisição
					function :: atom(),  						%% Atom da mensagem ou função que vai ser invocada no processo que vai atender a requisição
					id_re_compiled,   							%% Identificador da expressão regular que vai verificar se a URL bate com a URL da requisição
					apikey  :: boolean(), 						%% Indica se o contrato estará listado no Portal API Management
					comment :: string(), 						%% Comentário sobre o que o contrato oferece em termos de serviço
					version :: string(), 						%% Versão do contrato do serviço
					owner :: string(),  						%% Quem é o proprietário pelo serviço
					async :: boolean(), 						%% Indica se o serviço será processado em segundo plano (chamada assíncrona)
					querystring :: string(),  					%% Definição da querystring para o contrato do serviço
					qtd_querystring_req :: non_neg_integer(), 	%% Indica quantas querystrings são obrigatórias
					host :: atom(),  							%% Atom do host onde está o módulo do serviço que vai processar a requisição
					host_name,				  					%% Nome do host onde está o módulo do serviço que vai processar a requisição
					result_cache :: non_neg_integer(), 			%% Indica quanto tempo em milisegundos o resultado vai ficar armazenado em cache (somente para o módulo msbus_static_file_service)
					authentication :: string(),					%% Forma de autenticação (Por enquanto somente Basic)
					node,										%% Node ou lista de node onde os serviços estão publicados
					lang,										%% Linguagem que foi utilizada para implementar o serviço
					datasource,									%% Datasource para a fonte de dados
					debug										%% Permite habilitar um modo debug (depende da implementação do serviço)
				}).


-record(ctrl_sqlite_table, {file_name :: string(), 
							last_modified :: file:date_time()}).
					

-record(catalog_schema, {id :: non_neg_integer(), 
						 name :: string(),	
						 description :: string(),
						 json_schema :: map()
						}).

-record(schema_type, {id :: non_neg_integer(), 
					  name :: string(),	
					  description :: string(),
					  json_schema :: map()
				}).
