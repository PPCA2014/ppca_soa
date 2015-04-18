%% ---
%%  PPCA_SOA
%%  Manipula opções de configuração.
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%---


-define(CONF_PORT, 2301).


-record(rota, {metodo="GET", url, async, funcao}).


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
	



	
     
     
     	




