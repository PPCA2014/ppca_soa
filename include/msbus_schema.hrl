%%********************************************************************
%% @title db_schema
%% @version 1.0.0
%% @doc Contém definições das tabelas para persistência.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-record(sequence, {key, index}).
-record(user, {id, nome, email}).
-record(request, {
					  rid,       %% Request ID (Identificador da requisição)
					  servico,   %% serviço que vai atender a requisição
					  timestamp, %% Timestamp da requisição
					  latencia,
					  status,    %% Código de retorno HTTP
					  %% Próximos campos são cabecalhos do HTTP	
					  metodo,
					  url,
					  versao_http,
					  payload,
					  payload_map,
					  querystring,
					  querystring_map,
					  params_url,
					  content_length, 
					  content_type,
					  accept,
					  user_agent,
					  accept_encoding,
					  cache_control,
					  host
				  }).




