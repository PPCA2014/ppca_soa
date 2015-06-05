%% ---
%%  PPCA_SOA
%%  Autenticador de usuarios do barramento
%%  Mestrado em Computacao Aplicada - Universidade de Brasilia
%%  Turma de Construcao de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%  Alunos: Marcal de Lima Hokama (mhokama@hotmail.com)
%%---
-module(ppca_auth_user).

-export([autentica/3]).

%%init() ->
	%% Ao iniciar o modulo, zera a lista de sessoes ativas.     	
      %%loop().

%%
%% O modulo autentica e chamado por ppca_request:executa_servico
%% No payload vem dois pares de chave/valor: "user" e "pass"
%%
autentica(HeaderDict,From,Payload) ->
	%% Recupera o metodo da chamada
	Metodo = dict:fetch("Metodo", HeaderDict),
	%% So aceita chamadas com metodo POST
	if Metodo == "POST" ->
                  %% Teste de tamanho do Payload
                  PayloadLength = size(Payload),
                  io:format("To na area !!!~n", []),
                  io:format("Tamanho: " ++ PayloadLength ++ "~n", []),
   	            %% Separa a chave "user" 
                  io:format("To na area !!!~n", []),
                  {"user", Key} = lists:keyfind("user",1,json:parse(Payload)),
                  io:format("To na area !!!~n", []),
                  Response = "teste " ++ Metodo ++ " user: " ++ Key ++"~n",
			%Query = dict:fetch("Query", HeaderDict),
			%Response= ppca_util:json_encode([{<<"id">>,<<"Url">>}]),
			From ! { ok, Response}
			%ppca_logger:info_msg("rota atingida " ++ Url ++" metodo "++ Metodo )
	end.
	%% Outros metodos devem retornar erro
	%% Implementar:
      %% 1- Abrir novo processo?
	%% 2- Verificar se usuario+IP ja esta na lista de sessoes ativas
	%%   2.1 - Se tiver, eliminar sessao da lista, pois havera nova autenticacao
	%% 3- Decriptografar a senha (implementar posteriormente)
	%% 4- Verificar em uma lista de usuario+senha persistida (implementar posteriormente em banco de dados)
	%%    se existe um match
	%%   4.1 - Se existir, insere na lista de sessoes ativas o usuario, o IP, e a hora de inicio da sessao
	%%         e registra no log a autenticacao com sucesso
	%%         e retorna mensagem de autenticacao com sucesso
	%%   4.2 - Se nao existir, tem que retornar mensagem informando erro na autenticacao 
	%%         e registra no log uma autenticacao mal-sucedida 
