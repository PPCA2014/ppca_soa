%%********************************************************************
%% @title ems_schema
%% @version 1.0.0
%% @doc It contains definitions of the data structures used.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-record(sequence, {key :: atom(), 
				   index :: non_neg_integer()}).

-record(counter, {key :: atom(), 
     			  index :: non_neg_integer()}).

-record(user, {id :: non_neg_integer(), 					%% identificador do usuário (required) (Na UnB é o campo Tb_Usuario.UsuId)
			   codigo :: non_neg_integer(),					%% código da pessoa se o usuário possui dados pessoais em outra tabela externa. (Na UnB é o campo Tb_Pessoa.PesCodigoPessoa)
			   login :: binary(),							%% login do usuário (required)
			   name :: binary(), 							%% nome do usuário (required)
			   cpf :: binary(),
			   email :: binary(), 							
			   password :: binary(),						%% password (required)
			   type = 0 :: non_neg_integer(),				%% 0 = interno  1 = tecnico  2 = docente  3 = discente
			   subtype = 0 :: non_neg_integer(),			%% se aluno,  1 = extensao 2 = graduacao 3 = aperfeicoamento 4 = especializacao 5 = mestrado 6 = doutorado 7 = pos-doutorado 8 = residencia 9 = aluno especial - graduacao 10 = aluno especial - pos-graduacao 11 = estagio em pos-graduacao
			   passwd_crypto :: binary(),					%% Algoritmo criptografia: SHA1
			   type_email :: non_neg_integer(),				%% undefined = desconhecido  1 = Institucional  2 = Pessoal
			   active = true :: boolean(),
			   endereco :: binary(),
			   complemento_endereco :: binary(),
			   bairro :: binary(),
			   cidade :: binary(),
			   uf :: binary(),
			   cep :: binary(),
			   rg :: binary(),
			   data_nascimento :: binary(),
			   sexo :: non_neg_integer(),
			   telefone :: binary(),
			   celular :: binary(),
			   ddd :: binary(),
			   nome_pai :: binary(),
			   nome_mae :: binary(),
			   nacionalidade :: non_neg_integer(),
			   matricula :: non_neg_integer(),				%% se o usuário tem alguma matrícula proveniente de dados funcionais
			   remap_user_id :: non_neg_integer(),
			   ctrl_path :: string(),
			   ctrl_file :: string(),
			   ctrl_insert,									%% Data que o serviço foi inserido no banco mnesia
			   ctrl_update, 								%% Data que o serviço foi atualiado no banco mnesia			
			   ctrl_modified,								%% Data que o serviço foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
			   ctrl_hash									%% Hash gerado para poder comparar dois registros
		}).
		
-record(user_dados_funcionais, {
			   id :: non_neg_integer(), 					%% %% identificador dos dados funcionais (Na UnB é o campo Tb_Usuario.UsuId)
			   type :: non_neg_integer(),					%% 0 = interno  1 = tecnico  2 = docente  3 = discente
			   subtype :: non_neg_integer(),				%% se aluno,  1 = extensao 2 = graduacao 3 = aperfeicoamento 4 = especializacao 5 = mestrado 6 = doutorado 7 = pos-doutorado 8 = residencia 9 = aluno especial - graduacao 10 = aluno especial - pos-graduacao 11 = estagio em pos-graduacao
			   active :: boolean(),
			   matricula :: non_neg_integer(),				%% matrícula proveniente de dados funcionais
			   ctrl_path :: string(),
			   ctrl_file :: string(),
			   ctrl_insert,									%% Data que o serviço foi inserido no banco mnesia
			   ctrl_update, 								%% Data que o serviço foi atualiado no banco mnesia			
			   ctrl_modified,								%% Data que o serviço foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
			   ctrl_hash									%% Hash gerado para poder comparar dois registros
		}).

-record(user_email, {
			   id :: non_neg_integer(), 					%% identificador dos email (Na UnB é o campo TB_Email.EmaCodigo)
			   codigo :: non_neg_integer(),					%% código da pessoa. (Na UnB é o campo Tb_Pessoa.PesCodigoPessoa)
			   email :: binary(),	
			   type :: non_neg_integer(),					%% 1 = institucional  2 = outro
			   ctrl_path :: string(),
			   ctrl_file :: string(),
			   ctrl_insert,									%% Data que o serviço foi inserido no banco mnesia
			   ctrl_update, 								%% Data que o serviço foi atualiado no banco mnesia			
			   ctrl_modified,								%% Data que o serviço foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
			   ctrl_hash									%% Hash gerado para poder comparar dois registros
		}).

-record(user_endereco, {
			   id :: non_neg_integer(), 					%% identificador do endereço (Na UnB é o campo TB_Endereco.EndCodigo)
			   codigo :: non_neg_integer(),					%% código da pessoa. (Na UnB é o campo Tb_Pessoa.PesCodigoPessoa)
			   endereco :: binary(),
			   complemento :: binary(),
			   bairro :: binary(),
			   cidade :: binary(),
			   uf :: binary(),
			   cep :: binary(),
			   type :: non_neg_integer(),					%% 1 = residencial  2 = comercial 3 = exterior 4 = outro
			   ctrl_path :: string(),
			   ctrl_file :: string(),
			   ctrl_insert,									%% Data que o serviço foi inserido no banco mnesia
			   ctrl_update, 								%% Data que o serviço foi atualiado no banco mnesia			
			   ctrl_modified,								%% Data que o serviço foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
			   ctrl_hash									%% Hash gerado para poder comparar dois registros
		}).

-record(user_telefone, {
			   id :: non_neg_integer(), 					%% identificador do endereço (Na UnB é o campo TB_Telefone.TelCodigo)
			   codigo :: non_neg_integer(),					%% código da pessoa. (Na UnB é o campo Tb_Pessoa.PesCodigoPessoa)
			   numero :: binary(),
			   ramal :: non_neg_integer(),
			   ddd :: binary(),
			   type :: non_neg_integer(),					%% 1 = celular  2 = comercial 3 = residencial
			   ctrl_path :: string(),
			   ctrl_file :: string(),
			   ctrl_insert,									%% Data que o serviço foi inserido no banco mnesia
			   ctrl_update, 								%% Data que o serviço foi atualiado no banco mnesia			
			   ctrl_modified,								%% Data que o serviço foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
			   ctrl_hash									%% Hash gerado para poder comparar dois registros
		}).

-record(user_permission, {id :: non_neg_integer(),			%% identificador do perfil (required) (Na UnB é o campo TB_Perfil_Transacao.PTrid)
						  user_id :: non_neg_integer(),		%% identificador do usuário (required) (Na UnB é o campo Tb_Usuario.UsuId)
						  client_id :: non_neg_integer(),	%% identificador do cliente (required) (Na UnB é o campo Tb_Sistemas.PerSisId)
						  hash :: non_neg_integer(),
						  hash2 :: non_neg_integer(),
						  name :: binary(),
						  url :: binary(),
						  grant_get :: boolean(),
						  grant_post :: boolean(),
						  grant_put :: boolean(),
						  grant_delete :: boolean(),
						  ctrl_path :: string(),
						  ctrl_file :: string(),
						  ctrl_insert,							%% Data que o serviço foi inserido no banco mnesia
						  ctrl_update, 							%% Data que o serviço foi atualiado no banco mnesia			
						  ctrl_modified,						%% Data que o serviço foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
						  ctrl_hash								%% Hash gerado para poder comparar dois registros
          }).


-record(user_perfil, {id :: non_neg_integer(), 				%% identificador do perfil (required) (Na UnB é o campo Tb_Perfil.PerId)				
					  user_id :: non_neg_integer(),			%% identificador interno do usuário (required)
					  client_id :: non_neg_integer(),		%% identificador interno do client (required)
					  name :: binary(), 					%% nome do perfil (required)
					  ctrl_path :: string(),
				      ctrl_file :: string(),
				      ctrl_insert,							%% Data que o serviço foi inserido no banco mnesia
				      ctrl_update, 							%% Data que o serviço foi atualiado no banco mnesia			
				      ctrl_modified,						%% Data que o serviço foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
				      ctrl_hash								%% Hash gerado para poder comparar dois registros
		}).
          

-record(client, {id :: non_neg_integer(), 					
				 name :: binary(), 
			     description :: binary(),
			     secret :: binary(),
				 redirect_uri :: binary(),
				 active :: boolean(),
				 scope :: binary(),
				 ctrl_path :: string(),
				 ctrl_file :: string(),
				 ctrl_insert,								%% Data que o serviço foi inserido no banco mnesia
				 ctrl_update, 								%% Data que o serviço foi atualiado no banco mnesia			
				 ctrl_modified,								%% Data que o serviço foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
				 ctrl_hash									%% Hash gerado para poder comparar dois registros
		}).






-record(ctrl_params, {name :: string(),
					  value
		}).

			   
-record(request, {
					  rid,       								%% Request ID (Identificador da requisição gerada automaticamente)
					  rowid,									%% Identificador interno da requisição
					  service,   								%% Contrato que estabelece o serviço que vai atender a requisição
					  timestamp, 								%% Timestamp de quando que a requisição ocorreu
					  latency :: non_neg_integer(),				%% Latência (tempo que levou para processar a requisição)
					  code = 200 :: non_neg_integer(), 			%% Código de retorno HTTP (Ex.: 202 OK, 404 Não Encontrado)
					  reason = ok :: atom(),					%% Registra a mensagem de erro, quando status indicar um erro
					  type :: string(),							%% Verbo HTTP (GET, POST, PUT, DELETE e OPTIONS)
					  uri :: string(),							%% URI da requisição do serviço
					  url :: string(),							%% URL da requisição do serviço
					  version :: string(),						%% Versão do cabeçalho HTTP
					  payload :: binary(),						%% Corpo da requisição (aceita somente JSON)
					  payload_map :: map(),						%% Corpo da requisição convertida para map após o parser e validação
					  querystring :: binary(),					%% Querystring da requisição
					  querystring_map,							%% Querystring convertida para map após o parser e validação
					  params_url,								%% Map com os parâmetros da URL
					  content_type_in :: binary(),				%% Tipo de conteúdo enviado ao barramento (Ex.: application/json, application/pdf)
					  content_length :: non_neg_integer(), 		%% Largura da requisição
					  content_type :: string(),					%% Tipo de conteúdo (Ex.: application/json)
					  accept :: binary(),						%% Parâmetro ACCEPT HTTP
					  user_agent :: binary(),					%% Nome do browser
					  user_agent_version :: binary(),			%% Versão do browser
					  accept_encoding :: string(),				%% Parâmetro ACCEPT_ENCODING HTTP
					  cache_control :: binary(),				%% Parâmetro CACHE-CONTROL HTTP
					  etag :: string(),							%% Parâmetro ETag
					  if_modified_since :: string(),			%% Parâmetro If-Modified-Since
					  if_none_match :: string(),			    %% Parâmetro If-None-Match
					  ip :: tuple(),
					  ip_bin :: binary(),						%% Peer que iniciou a requisição
					  t1,										%% Utilizado para cálculo da latência (Tempo inicial em milisegundos)
					  socket :: gen_tcp:socket(),				%% Socket da requisição
					  worker :: pid(),							%% Processo worker http que vai atender a requisição
					  status_send,								%% Registra que a mensagem foi entregue ou o erro ocorrido na entrega
					  authorization :: binary(),				%% Dados da autenticação da requisição
					  client :: #client{},
					  user :: #user{},							%% Usuário da requisição ou public
					  node_exec = undefined,					%% Node que foi enviado a solicitação
					  status = latency,							%% status: latency, req_done, req_send
					  worker_send,
					  protocol :: atom(),						%% Protocol (http, ldap)
					  protocol_bin :: binary(),	
					  port :: non_neg_integer(),				
					  result_cache = false :: boolean(),
					  result_cache_rid,
					  response_data = <<>>,
					  response_header = #{},
					  req_hash,
					  host :: binary(),							%% Ip do barramento
					  filename :: string(),						%% Qual arquivo foi lido do disco
					  referer :: binary(),
					  access_token :: binary(),
					  scope :: binary(),
					  oauth2_grant_type :: binary(),
					  oauth2_access_token :: binary(),
					  oauth2_refresh_token :: binary()
					  
				  }).


-record(service_datasource, {id :: non_neg_integer(),
							 rowid :: non_neg_integer(),
							 type :: atom(),								%% sqlserver, csvfile, mnesia
							 driver :: binary(),							%% sqlite3, odbc, undefined
							 connection :: binary(),
							 table_name :: binary() | atom() | list(atom()),
							 fields :: binary() | atom() | list(atom()),
							 remap_fields :: map(),							%% Permite expor um campo com outro nome
							 remap_fields_rev :: map(),						
							 show_remap_fields :: boolean(),				%% Indica se deve mostrar os campos remapeados
							 primary_key :: binary() | atom(),
							 foreign_key :: binary() | atom(),
							 foreign_table_name  :: binary() | atom(),			
							 csv_delimiter :: binary(),
							 sql :: binary(),
							 timeout :: non_neg_integer(),
							 max_pool_size :: non_neg_integer(),
							 conn_ref,
							 pid_module,
							 pid_module_ref,
							 owner,
							 owner_ref,
							 connection_count_metric_name :: atom(),		%% Quantas conexões alocadas
							 connection_created_metric_name :: atom(),		%% Quantas conexões criadas
							 connection_closed_metric_name :: atom(),   	%% Quantas conexões foram fechadas de forma normal
							 connection_shutdown_metric_name :: atom(), 	%% Quantas conexões foram fechadas devido algum erro
							 connection_reuse_metric_name :: atom(), 		%% Quantas conexões foram reutilizadas
							 connection_unavailable_metric_name :: atom(), 	%% Quantas vezes não houve conexão
							 connection_max_pool_size_exceeded_metric_name :: atom(), 	%% Quantas vezes excedeu o número de conexões permitidos
							 sql_check_valid_connection :: string(),
							 check_valid_connection_timeout :: non_neg_integer(),
							 close_idle_connection_timeout :: non_neg_integer(),
							 ctrl_path :: string(),
							 ctrl_file :: string(),
							 ctrl_insert,									%% Data que o serviço foi inserido no banco mnesia
							 ctrl_update, 									%% Data que o serviço foi atualiado no banco mnesia			
							 ctrl_modified,									%% Data que o serviço foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
							 ctrl_hash										%% Hash gerado para poder comparar dois registros
							}).


-record(service_owner, {  id :: non_neg_integer(),
						   name :: string(),
						   title :: string(),
						   comment :: string()
						}).


-record(service, {  id :: non_neg_integer(), 					%% Id do serviço
					rowid :: non_neg_integer(),					%% Identificador interno do contrato (utilizado para localizar o contrato)
					name :: binary(), 							%% Nome do contrato do serviço (Por default usa-se a própria URL como name)
					url :: string(),  							%% URL do contrato do serviço
					type = <<"GET">> :: binary(),				%% Verbo HTTP do contrato (GET, POST, PUT, DELETE e OPTIONS) ou KERNEL para módulos do barramento
					service :: binary(),						%% Serviço que será executado no contrato
					middleware :: atom(),						%% Miidleware definido para pós processamento do serviço
					module_name :: string(), 					%% Nome do módulo do serviço que vai atender a requisição. Ex.: br.erlangms.HelloWorldService  
					module_name_canonical :: string(), 			%% Nome do módulo canonico do serviço que vai atender a requisição. Ex.: HelloWorldService  
					module :: atom(),  							%% Atom do processo do módulo de serviço que vai atender a requisição
					function_name :: string(),					%% Nome da mensagem ou função que vai ser invocada no processo que vai atender a requisição
					function :: atom(),  						%% Atom da mensagem ou função que vai ser invocada no processo que vai atender a requisição
					use_re = false :: boolean(),				%% Flag que indica se usa expressão regular
					id_re_compiled = undefined, 				%% Identificador da expressão regular que vai verificar se a URL bate com a URL da requisição
					public = true :: boolean(), 				%% Indica se o contrato estará listado no Portal API Management
					comment :: binary(), 						%% Comentário sobre o que o contrato oferece em termos de serviço
					version = "1.0.0" :: binary(), 				%% Versão do contrato do serviço
					owner :: binary(),  						%% Quem é o proprietário pelo serviço
					async = false :: boolean(),					%% Indica se o serviço será processado em segundo plano (chamada assíncrona)
					querystring :: list(map()),					%% Definição da querystring para o contrato do serviço
					qtd_querystring_req :: non_neg_integer(), 	%% Indica quantas querystrings são obrigatórias
					host :: atom(),  							%% Atom do host onde está o módulo do serviço que vai processar a requisição
					host_name,				  					%% Nome do host onde está o módulo do serviço que vai processar a requisição
					result_cache :: non_neg_integer(), 			%% Indica quanto tempo em milisegundos o resultado vai ficar armazenado em cache (somente para o módulo msbus_static_file_service)
					authorization :: atom(),					%% Forma de autenticação (public, basic, oauth2)
					authorization_public_check_credential = false :: boolean(),		%% Faz a checagem da credencial do usuário quando o serviço é publico
					oauth2_with_check_constraint = false :: boolean(),
					oauth2_allow_client_credentials = false :: boolean(),
					oauth2_token_encrypt = false :: boolean(),
					page,										%% Page django file
					page_module,								%% Page module django file compiled
					page_mime_type = <<"text/html">>,			%% Page mime type
					node,										%% Node ou lista de node onde os serviços estão publicados
					lang = "erlang" :: binary(),				%% Linguagem que foi utilizada para implementar o serviço
					datasource :: #service_datasource{},		%% Datasource para a fonte de dados
					debug = false :: boolean(),					%% Permite habilitar um modo debug (depende da implementação do serviço)
					schema_in :: non_neg_integer(),
					schema_out :: non_neg_integer(),
					pool_size :: non_neg_integer(),
					pool_max :: non_neg_integer(),
					timeout :: non_neg_integer(),
					expires :: non_neg_integer(),
					cache_control :: binary(),
					enable = false :: boolean(),
					content_type :: binary(),					%% Tipo de conteúdo (Ex.: application/json, application/pdf)
					path :: string(),							%% Local para carregar arquivos estáticos
					filename :: binary(),						%% Alguns serviços podem precisar informar um nome de arquivo
					redirect_url :: binary(),					%% redirect url						
					tcp_listen_address,
					tcp_listen_address_t,
					tcp_allowed_address,
					tcp_allowed_address_t,
					tcp_max_connections :: non_neg_integer(),
					tcp_port :: non_neg_integer(),
					tcp_is_ssl = false :: boolean(),
					tcp_ssl_cacertfile,
					tcp_ssl_certfile,
					tcp_ssl_keyfile,
					protocol :: binary(),
					properties :: map(),						%% Outros parâmetros
					ctrl_path :: string(),						%% Local de onde o catálogo foi carregado
					ctrl_file :: string(),						%% Nome do arquivo onde está especificado o catálogo
				    ctrl_insert,								%% Data que o serviço foi inserido no banco mnesia
					ctrl_update, 								%% Data que o serviço foi atualiado no banco mnesia			
					ctrl_modified,								%% Data que o serviço foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
					ctrl_hash,									%% Hash gerado para poder comparar dois registros
					start_timeout :: non_neg_integer(),			%% Define um timeout inicial para o processo
					service_exec_metric_name :: atom(),
					service_result_cache_hit_metric_name :: atom(),
					service_host_denied_metric_name :: atom(),
					service_auth_denied_metric_name :: atom(),
					service_error_metric_name :: atom(),
					service_unavailable_metric_name :: atom(),
					service_timeout_metric_name :: atom(),
					http_max_content_length :: non_neg_integer()
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


-record(produto, {id :: non_neg_integer(), 
				  name :: string(), 
				  price :: non_neg_integer()}).

-record(stat_counter_hist, {  id :: non_neg_integer(),
							  stat_name :: atom(),
							  stat_value :: non_neg_integer,
							  stat_timestamp
							}).


