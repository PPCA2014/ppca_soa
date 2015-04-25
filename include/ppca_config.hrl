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


% Definições para o logger do PPCA_SOA
-record(logger, {filename="./log/server.log",
				 logger_level,
				 tam_max_logfile="1M"
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
	



	
     
     
     	




