%%********************************************************************
%% @title msbus_schema
%% @version 1.0.0
%% @doc Contém definições das estruturas de dados utilizadas.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-record(sequence, {key :: atom(), 
				   index :: non_neg_integer()}).

-record(user, {id :: non_neg_integer(), 
			   nome :: string(), 
			   email :: string(), 
			   senha :: string()}).
			   
-record(request, {
					  rid,       %% Request ID (Identificador da requisição)
					  servico,   %% serviço que vai atender a requisição
					  timestamp, %% Timestamp da requisição
					  latencia :: non_neg_integer(),
					  status,    %% Código de retorno HTTP
					  %% Próximos campos são cabecalhos do HTTP	
					  type :: string(),
					  url :: string(),
					  versao_http :: string(),
					  payload :: string(),
					  payload_map :: map(),
					  querystring :: string(),
					  querystring_map,
					  params_url,
					  content_length :: non_neg_integer(), 
					  content_type :: string(),
					  accept :: string(),
					  user_agent :: string(),
					  accept_encoding :: string(),
					  cache_control :: string(),
					  host :: string(),
					  t1,
					  socket :: gen_tcp:socket(),
					  worker :: pid()
				  }).

-record(servico, {
					rowid :: string(),
					id :: non_neg_integer(),
					name :: string(),
					url :: string(),
					type :: string(),
					module :: atom(),
					function :: atom(),
					id_re_compiled,
					apikey  :: boolean(),
					comment :: string(),
					version :: string(),
					owner :: string(),
					async :: boolean(),
					querystring :: string(),
					qtd_querystring_req :: non_neg_integer(),
					host :: atom(),
					result_cache :: non_neg_integer()
				}).


