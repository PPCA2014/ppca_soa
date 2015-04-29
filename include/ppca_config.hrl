%% ---
%%  PPCA_SOA
%%  Manipula opções de configuração.
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%---


% Seta a porta default do barramento SOA
-define(CONF_PORT, 2301).


% Seta o tamanho máximo do payload do POST. Por default é 1M.
-define(HTTP_MAX_POST_SIZE, 1024 * 1024 * 1024).


% Define um rota
-record(rota, {metodo="GET", url, async, funcao}).


% Configurações para o logger do PPCA_SOA
-record(logger, {%% nome do arquivo do logger
				 filename="server.log",   
				 
				 %% de quanto em quanto tempo vai descarregar o buffer do log em disco
				 checkpoint_timeout = 6000,  %% 6 segundos
				 
				 %% de quanto em quanto tempo vai rotacionar o arquivo. 
				 rotacao_timeout = 1000 * 60 * 60 * 24   %% 24h
				}).




-define(
    tabela_rota,
	[ #rota{metodo="GET"
	        url="/hello_world", 
			async=false, 
			funcao=fun() -> io:format("Hello world") end
			},
	  #rota{metodo="POST",
			url="/hello_world", 
			async=true, 
			funcao=fun() -> io:format("Hello world2") end
			}
			
	]
).
	



	
     
     
     	




