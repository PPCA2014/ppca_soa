%%********************************************************************
%% @title Module ems_permission_loader
%% @version 1.0.0
%% @doc ems_permission_loader
%% @author Renato Carauta Ribeiro <rcarauta6@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_permission_loader).

-behavior(gen_server).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

%%Server API
-export([start/1,stop/0]).

%%get_setver callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-record(state,{datasource,
			   operation,
			   update_checkpoint,
			   last_update}).

-define(SERVER,?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(Service) ->
    gen_server:start_link({local,?SERVER}, ?MODULE, Service, []).
    
stop() ->
    get_server:cast(?SERVER, shutdown).
    
    
%%====================================================================
%% gen_server callbacks
%%====================================================================

init(#service{datasource = Datasource,
               properties = Props}) ->
        LastUpdate = ems_db:get_param(<<"ems_permission_loader_lastupdate">>),
        UpdateCheckpoint = maps:get(<<"update_checkpoint">>, Props, ?PERMISSION_LOADRES_UPDATE_CHECKPOINT),
        State = #state{datasource = Datasource,
        			   operation = load_or_update_operation(),
        			   update_checkpoint = UpdateCheckpoint,
        			   last_update = LastUpdate},
        {ok, State, 6000}.
        
handle_cast(shutdown, State) ->
     {stop, normal, State};
handle_cast(_Msg, State) ->
     {noreply, State}.
      
handle_call(Msg,_From, State) ->
     {reply, Msg, State}.

handle_info(State) ->
   {noreply, State, 1000}.
   
handle_info(timeout, State=#state{datasource = Datasource,
                                  operation = update_permissions,
                                  update_checkpoint = UpdateCheckpoint,
                                  last_update = LastUpdate}) ->
         ?DEBUG("ems_permission_loader checkpoint. operation: update_permissions."),
         NextUpdate = calendar:local_time(),
         case update_permission_from_datasource(Datasource, LastUpdate, ems_util:timestamp_str()) of
         	ok ->
         		ems_db:set_param(<<"ems_permission_loader_lastupdate">>, NextUpdate),
         		State2 = State#state{last_update = NextUpdate,
         							 operation = load_or_update_operation()},
         		{noreply, State2, UpdateCheckpoint};
         	_ ->
         		{noreply, State, UpdateCheckpoint}
           end;
handle_info(timeout, State=#state{datasource = Datasource,
                                  operation = load_permissions,
                                  update_checkpoint = UpdateCheckpoint}) ->
           ?DEBUG("ems_permission_loader checkpoint. operation: load_permissions."),
           NextUpdate = calendar:local_time(),
           case load_permissions_from_datasource(Datasource, ems_util:timestamp_str()) of
           	 ok ->
           	    ems_db:set_param(<<"ems_permission_loader_lastupdate">>, NextUpdate),
           	    State2 = State#state{operation = load_or_update_operation(),
           	                          last_update = NextUpdate},
           	    {noreply, State2, UpdateCheckpoint};
           	  _ ->
           	    {noreply, State, UpdateCheckpoint}
           end;
handle_info({_Pid,{error, Reason}}, State=#state{update_checkpoint = UpdateCheckpoint}) ->
	    ems_logger:warn("ems_permission_loader is unable to update permission. Reason: ~p",[Reason]),
	    {noreply, State, UpdateCheckpoint}.

terminate(_Reason, _State) ->
	  ok.
	  
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
    
%%====================================================================
%% Internal functions
%%====================================================================

-spec load_or_update_operation() -> 
	  load_permissions | update_permissions.
load_or_update_operation() ->
	  case mnesia:table_info(permission, size) of
	       0 -> load_permissions;
	       _ -> update_permissions
	  end.
	  
load_permissions_from_datasource(Datasource, CtrlInsert) ->
     try
       case ems_odbc_pool:get_connection(Datasource) of
          {ok, Datasource2} ->
			  ?DEBUG("ems_permission_loader got a connection to load permissions."),
              Result = case ems_odbc_pool:param_query(Datasource2,
              										  sql_load_permissions(),
              										  [],
              										  ?MAX_TIME_ODBC_QUERY) of
              			{_,_,[]} ->
              			   ?DEBUG("ems_permission_loader did not load any permissions."),
              			   ok;
              			{_,_, Records} ->
              			    F = fun() ->
              			       Count = insert_permissions(Records, 0, CtrlInsert),
              			   ems_logger:info("ems_permission_loader load ~p permissions.", [Count])    
              		    end,
              		    mnesia:ets(F),
              		    mnesia:change_table_copy_type(permission, node(), disc_copies),
              		    ok;
              	       {error, reason} = Error ->
              	           Error
              	       end,
              	    ems_db:release_connection(Datasource2),
              	    Result;
            Error2 ->
                ems_logger:warn("ems_permission_loader has no connection to load permissions from database."),
                Error2
         end
       catch 
           _Exception:Reason3 ->
			 ems_logger:error("ems_permission_loader load permissions error: ~p.", [Reason3]),
             {error, Reason3}
       end.
       
 update_permission_from_datasource(Datasource, LastUpdate, CtrlUpdate) ->
    try
       case ems_odbc_pool:get_connection(Datasource) of 
          {ok, Datasource2} ->
               ?DEBUG("ems_permission_loader got a connection ~p to update permissions.", [Datasource#service_datasource.id]),
               case LastUpdate of
                   undefined ->
                       Sql = sql_load_permissions(),
                       Params = [];
                   _ ->
                      Sql = sql_update_permissions(),
                      {{Year, Month, Day},{Hour, Min, _}} = LastUpdate,
                      DateInitial = {{Year, Month, Day}, {Hour, Min, 0}},
                      Params = [{sql_timestamp, [DateInitial]},
                                {sql_timestamp, [DateInitial]},
                                {sql_timestamp, [DateInitial]},
                                {sql_timestamp, [DateInitial]},
                                {sql_timestamp, [DateInitial]},
								{sql_timestamp, [DateInitial]}]
				  end,
				  Result = case ems_odbc_pool:param_query(Datasource2, Sql, Params, ?MAX_TIME_ODBC_QUERY) of
				  	  {_,_,[]} ->
				  	      ?DEBUG("ems_permission_loader did not update any permissions."),
				  	      ok;
				  	  {_,_,Records} ->
				  	       F = fun() ->
				  	           Count = update_permissions(Records, 0, CtrlUpdate),
				  	           ems_logger:info("ems_permission_loader update ~p permissions.", [Count])
				  	       end,
				  	       mnesia:activity(transaction, F),
				  	       ok;
				  	   {error, Reason} = Error ->
							ems_logger:error("ems_permission_loader update permissions error: ~p.", [Reason]),
				  	   		Error
				  	  end,
				  	  ems_db:release_connection(Datasource2),
				  	  Result;
				  	Error2 ->
					   ems_logger:warn("ems_permission_loader has no connection to update permissions from database."),
				  	   Error2
				  end
				catch
				  _Exception:Reason3 ->
					ems_logger:error("ems_permission_loader udpate permissions error: ~p.", [Reason3]),
				    {error, Reason3}
				end.
				
update_permissions([], Count, _CtrlUpdate) ->
		Count;
update_permissions([{Codigo, Login, PerId, PerNome, TraId, TraNameFrm,  TraNameMenu, TraInclude, TraExclude, TraUpdate, TraVisualize}|T], Count, CtrlUpdate) ->
	 case ems_permission:find_by_login(Login) of
	     {ok, Permission} ->
	          Permission2 = Permission#permission {codigo = Codigo,
	                             login = ?UTF8_STRING(Login),
	                             perId = PerId,
	                             perNome = ?UTF8_STRING(PerNome),
	                             traId = TraId,
	                             traNameFrm = ?UTF8_STRING(TraNameFrm),
	                             traNameMenu = ?UTF8_STRING(TraNameMenu),
	                             traInclude = TraInclude,
	                             traExclude = TraExclude,
	                             traUpdate = TraUpdate,
	                             traVisualize = TraVisualize,
	                             ctrl_update = CtrlUpdate};
	     {error, enoent} ->
	     		Permission2 = #permission {id = ems_db:sequence(permission),
	     						 codigo = Codigo,
	                             login = ?UTF8_STRING(Login),
	                             perId = PerId,
	                             perNome = ?UTF8_STRING(PerNome),
	                             traId = TraId,
	                             traNameFrm = ?UTF8_STRING(TraNameFrm),
	                             traNameMenu = ?UTF8_STRING(TraNameMenu),
	                             traInclude = TraInclude,
	                             traExclude = TraExclude,
	                             traUpdate = TraUpdate,
	                             traVisualize = TraVisualize,
	                             ctrl_update = CtrlUpdate}
	       end,
	       mnesia:write(Permission2),
	       ?DEBUG("ems_permission_loader update permissions: ~p.\n", [Permission2]),
	       update_permissions(T,Count+1,CtrlUpdate).

insert_permissions([], Count, _CtrlInsert) ->
       Count;
insert_permissions([{Codigo, Login, PerId, PerNome, TraId, TraNameFrm,  TraNameMenu, TraInclude, TraExclude, TraUpdate, TraVisualize}|T], Count, CtrlInsert) -> 
		Permission = #permission {codigo = Codigo,
	                             login = ?UTF8_STRING(Login),
	                             perId = PerId,
	                             perNome = ?UTF8_STRING(PerNome),
	                             traId = TraId,
	                             traNameFrm = ?UTF8_STRING(TraNameFrm),
	                             traNameMenu = ?UTF8_STRING(TraNameMenu),
	                             traInclude = TraInclude,
	                             traExclude = TraExclude,
	                             traUpdate = TraUpdate,
	                             traVisualize = TraVisualize,
	                             ctrl_insert = CtrlInsert},
	     mnesia:dirty_write(Permission),
	     insert_permissions(T, Count+1, CtrlInsert).


sql_load_permissions() ->
  " select distinct u.UsuId as codigo, 
					u.UsuLogin as login, 
					p.PerId as perId, 
					p.PerNome as perNome, 
					t.TraId as traId, 
					t.TraNomeFrm as traNameFrm, 
					t.TraNomeMenu as traNameMenu,
					t.TraIncluir as traInclude, 
					t.TraAlterar as traUpdate, 
					t.TraExcluir as traExclude, 
					t.TraVisualizar as traVisualize
	    from BDAcesso.dbo.TB_Usuario u inner join BDAcesso.dbo.TB_Acessos_Perfil up  
				on u.UsuId = up.APeUsuId 
		inner join BDAcesso.dbo.TB_Perfil p 
				on up.APePerId = p.PerId 
		inner join BDAcesso.dbo.TB_Sistemas s 
				on p.PerSisId = s.SisId 
		inner join BDAcesso.dbo.TB_Perfil_Transacao pt 
				on p.PerId = pt.PTrPerId 
	    inner join BDAcesso.dbo.TB_Transacao t 
				on pt.PTrTraId = t.TraId ".
		
sql_update_permissions() ->
	   " select distinct u.UsuId as codigo, 
					u.UsuLogin as login, 
					p.PerId as perId, 
					p.PerNome as perNome, 
					t.TraId as traId, 
					t.TraNomeFrm as traNameFrm, 
					t.TraNomeMenu as traNameMenu,
					t.TraIncluir as traInclude, 
					t.TraAlterar as traUpdate, 
					t.TraExcluir as traExclude, 
					t.TraVisualizar as traVisualize
	    from BDAcesso.dbo.TB_Usuario u inner join BDAcesso.dbo.TB_Acessos_Perfil up  
				on u.UsuId = up.APeUsuId 
		inner join BDAcesso.dbo.TB_Perfil p 
				on up.APePerId = p.PerId 
		inner join BDAcesso.dbo.TB_Sistemas s 
				on p.PerSisId = s.SisId 
		inner join BDAcesso.dbo.TB_Perfil_Transacao pt 
				on p.PerId = pt.PTrPerId 
	    inner join BDAcesso.dbo.TB_Transacao t 
				on pt.PTrTraId = t.TraId 
		where u.UsuLogin = ? ".
		












