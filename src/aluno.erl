%% ---
%%  PPCA_SOA
%%  Servico 1
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%---

-module(aluno).

-behavior(gen_server). 

-include("../include/ppca_config.hrl").

%% Server API
-export([start/0, stop/0]).

%% Cliente interno API
-export([inserir_aluno/1, 
		 alterar_aluno/2, 
		 findby/1,
		 imprime_matricula/1,
		 relatorio_aluno_curso/4]).

%% Cliente externo API
-export([post_service/3,
		 put_service/3,
		 get_service/3]).

-export([teste_servico/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-import(lists, [map/2]).

-define(SERVER, ?MODULE).

%  Armazena o estado do servico. 
-record(state, {}). 


%%====================================================================
%% Server API
%%====================================================================

start() -> 
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    io:format("servico aluno iniciado.~n", []),
    Result.
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Cliente API Interno
%%====================================================================
 
inserir_aluno(JSON) -> 
	gen_server:call(?SERVER, {inserir_aluno, JSON}). 

alterar_aluno(Id, JSON) -> 
	gen_server:call(?SERVER, {alterar_aluno, Id, JSON}). 

findby(Id) -> 
	gen_server:call(?SERVER, {findby, Id}). 

imprime_matricula(Id) ->
	gen_server:call(?SERVER, {imprime_matricula, Id}). 

relatorio_aluno_curso(Tipo, Ano, Semestre, Curso) ->
	gen_server:call(?SERVER, {relatorio_aluno_curso, Tipo, Ano, Semestre, Curso}). 
	
	
%%====================================================================
%% Cliente API Externo
%%====================================================================

% curl:  POST /aluno/ -d"{"id":"1", "nome":"Everton"}" 
post_service([], _Query, JSON) ->
	inserir_aluno(JSON).

% curl:  PUT /aluno/10 -d"{"nome":"Everton"}" 
put_service([Id], _Query, JSON) ->
	alterar_aluno(Id, JSON).

% curl:  GET /aluno/10
get_service([Id], _Query, _JSON) ->
	findby(Id);

% curl: GET /aluno/imprime_matricula/10
get_service(["imprime_matricula", Id], _Query, _JSON) ->
	imprime_matricula(Id);

% curl: GET /aluno/relatorio/sintetico/aluno_curso/2015?semestre=2&curso=568
get_service(["relatorio", Tipo, "aluno_curso", Ano], [{"semestre", Semestre}, {"curso", Curso}], _JSON) ->
	relatorio_aluno_curso(Tipo, Ano, Semestre, Curso).


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State}.
    
handle_call({inserir_aluno, JSON}, _From, State) ->
	NewState = do_inserir_aluno(JSON, State),
	{reply, ok, NewState};

handle_call({alterar_aluno, Id, JSON}, _From, State) ->
	NewState = do_alterar_aluno(Id, JSON, State),
	{reply, ok, NewState};

handle_call({findby, Id}, _From, State) ->
	NewState = do_findby_aluno(Id, State),
	{reply, ok, NewState};

handle_call({imprime_matricula, Id}, _From, State) ->
	NewState = do_imprime_matricula(Id, State),
	{reply, ok, NewState};

handle_call({relatorio_aluno_curso, Tipo, Ano, Semestre, Curso}, _From, State) ->
	NewState = do_relatorio_aluno_curso(relatorio_aluno_curso, Tipo, Ano, Semestre, Curso, State),
	{reply, ok, NewState}.

handle_info(State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    io:format("servico aluno finalizado.~n"),
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Funções internas
%%====================================================================
    
do_inserir_aluno(JSON, State) ->
	ppca_logger:info_msg("aluno inserido. Registro: ~p.~n", [JSON]),
	State#state{}.

do_alterar_aluno(Id, JSON, State) ->
	ppca_logger:info_msg("aluno ~p alterado: Registro: ~p.~n", [Id, JSON]),
	State#state{}.

do_findby_aluno(Id, State) ->
	ppca_logger:info_msg("aluno ~p localizado.~n", [Id]),
	State.

do_imprime_matricula(Id, State) ->
	ppca_logger:info_msg("matrícula ~p impressa.~n", [Id]),
	State.
	
do_relatorio_aluno_curso(relatorio_aluno_curso, Tipo, Ano, Semestre, Curso, State) ->	
	ppca_logger:info_msg("relatório ~p gerado com sucesso: Ano: ~p, Semestre: ~p, Curso: ~p.~n", [Tipo, Ano, Semestre, Curso]),
	State.


	
%%
%% Teste do serviço aluno. 
%% Simular o serviço do módulo de roteamento
%%
teste_servico() ->
	% json de teste 
	JSON = ppca_util:json_decode(<<"{\"id\":\"1\", \"nome\":\"Everton\"}">>),
	JSON_Vazio = [],   % para simular que não tem paylad
	Query = [],
	
	% Exemplo 1
	% curl:  GET "/aluno/100"
	URL1 = ["100"],
	apply(?MODULE, get_service, [URL1, Query, JSON]),

	
	% Exemplo 2
	% curl:  POST /aluno/ -d"{"id":"1", "nome":"Everton"}" 
	URL2= [],
	apply(?MODULE, post_service, [URL2, Query, JSON]),
	
	
	% Exemplo 3
	% curl:  PUT /aluno/10 -d"{"nome":"Everton"}" 	
	URL3 = ["10"],
	apply(?MODULE, put_service, [URL3, Query, JSON]),
	
	
	% Exemplo 4
	% curl GET /aluno/imprime_matricula/100
	URL4 = ["imprime_matricula", "100"],
	apply(?MODULE, get_service, [URL4, Query, JSON_Vazio]),

	
	% Exemplo 5
	% curl GET /aluno/relatorio/sintetico/alunos_curso/2015?semestre=2&curso=568
	URL5 = ["relatorio", "sintetico", "aluno_curso", "2015"],
	Query5 = [{"semestre", "2"}, {"curso", "568"}],
	apply(?MODULE, get_service, [URL5, Query5, JSON_Vazio]),


	ok.

