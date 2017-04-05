-module(oauth2ems_backend).

-behavior(oauth2_backend).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").

%%% API
-export([start/0, stop/0, add_client/2, add_client/3, delete_client/1]).

-export([authenticate_user/2]).
-export([authenticate_client/2]).
-export([get_client_identity/2]).
-export([associate_access_code/3]).
-export([associate_refresh_token/3]).
-export([associate_access_token/3]).
-export([resolve_access_code/2]).
-export([resolve_refresh_token/2]).
-export([resolve_access_token/2]).
-export([revoke_access_code/2]).
-export([revoke_access_token/2]).
-export([revoke_refresh_token/2]).
-export([get_redirection_uri/2]).
-export([verify_redirection_uri/3]).
-export([verify_client_scope/3]).
-export([verify_resowner_scope/3]).
-export([verify_scope/3]).

-define(ACCESS_TOKEN_TABLE, access_tokens).
-define(ACCESS_CODE_TABLE, access_codes).
-define(REFRESH_TOKEN_TABLE, refresh_tokens).
-define(USER_TABLE, users).
-define(CLIENT_TABLE, clients).
-define(SCOPE_TABLE, scopes).


-define(TABLES, [?ACCESS_TOKEN_TABLE,
				 ?ACCESS_CODE_TABLE,
                 ?REFRESH_TOKEN_TABLE,
                 ?USER_TABLE,
                 ?CLIENT_TABLE,
                 ?SCOPE_TABLE]).

-record(client, {
          client_id     :: binary(),
          client_secret :: binary(),
          redirect_uri  :: binary()
         }).

-record(scope, {
          scope :: binary(),
          client_id :: binary()
         }).


%%%===================================================================
%%% 
%%%===================================================================

start() ->
    application:set_env(oauth2, backend, oauth2ems_backend),
    lists:foreach(fun(Table) ->
                          ets:new(Table, [named_table, public])
                  end,
                  ?TABLES),
	add_client("geral","123456", "http://localhost:2301/"),
	add_client("114740","123456", "http://localhost:2301/"),
	add_client("s6BhdRkqt3","qwer", "http://164.41.120.42:3000/callback"),
	add_scope("email","s6BhdRkqt3").

stop() ->
    lists:foreach(fun ets:delete/1, ?TABLES).
    
%%%===================================================================
%%% Teste
%%%===================================================================


add_client(Id, Secret, RedirectUri) ->
    put(?CLIENT_TABLE, Id, #client{client_id = Id,
                                   client_secret = Secret,
                                   redirect_uri = RedirectUri
                                  }).

add_client(Id, Secret) ->
    add_client(Id, Secret, undefined).

delete_client(Id) ->
    delete(?CLIENT_TABLE, Id).
    
add_scope(Scope, Client) ->
    put(?SCOPE_TABLE, Scope, #scope{scope = Scope, client_id = Client}).


%%%===================================================================
%%% OAuth2 backend functions
%%%===================================================================

authenticate_user({Login, Password}, _) ->
    case ems_user:find_by_login_and_password(Login, Password) of
        {ok, User = #user{name = Username}} ->	
			{ok, {[],{<<"user">>, Username}}};
		%% Padronizar o erro conforme o RFC 6749
        _ -> {error, notfound}
    end.

authenticate_client({ClientId, ClientSecret},_) ->
    case get(?CLIENT_TABLE, ClientId) of
        {ok, Client = #client{client_secret = CliSecret}} -> 
			case ClientSecret =:= CliSecret of
				true -> {ok, {[],Client}};
				_ -> {error, badsecret}
			end;
        _ -> {error, notfound}
    end.

get_client_identity(ClientId, _) ->
    case get(?CLIENT_TABLE, ClientId) of
        {ok, Client} -> {ok, {[],Client}};
        _ -> {error, notfound}
    end.

associate_access_code(AccessCode, Context, _AppContext) ->
    {put(?ACCESS_CODE_TABLE, AccessCode, Context), Context}.

associate_refresh_token(RefreshToken, Context, _) ->
    {put(?REFRESH_TOKEN_TABLE, RefreshToken, Context), Context}.

associate_access_token(AccessToken, Context, _) ->
    {put(?ACCESS_TOKEN_TABLE, AccessToken, Context), Context}.


resolve_access_code(AccessCode, _) ->
	case get(?ACCESS_CODE_TABLE, AccessCode) of
        {ok,Value} -> 	{ok,{[],Value}};
        Error = {error, notfound} -> Error
    end.

resolve_refresh_token(RefreshToken, _AppContext) ->
    resolve_access_token(RefreshToken, _AppContext).

resolve_access_token(AccessToken, _) ->
    case get(?ACCESS_TOKEN_TABLE, AccessToken) of
       {ok,Value} -> {ok,{[],Value}};
        Error = {error, notfound} ->  Error
    end.

revoke_access_code(AccessCode, _AppContext) ->
    delete(?ACCESS_CODE_TABLE, AccessCode),
    {ok, []}.

revoke_access_token(AccessToken, _) ->
    delete(?ACCESS_TOKEN_TABLE, AccessToken),
    {ok, []}.

revoke_refresh_token(_RefreshToken, _) ->
    {ok, []}.

get_redirection_uri(ClientId, _) ->
    case get(?CLIENT_TABLE, ClientId) of
        {ok, #client{redirect_uri = RedirectUri}} ->
            {ok, RedirectUri};
        Error = {error, notfound} ->
            Error
    end.

verify_redirection_uri(ClientId, ClientUri, _) when is_list(ClientId) ->
    case get(?CLIENT_TABLE, ClientId) of
        {ok, #client{redirect_uri = RedirUri}} when ClientUri =:= RedirUri ->
            ok;
        _Error ->
            {error, mismatch}
    end;
verify_redirection_uri(#client{redirect_uri = RedirUri}, ClientUri, _) ->
    case ClientUri =:= RedirUri of
		true -> {ok,[]};
		_Error -> {error, mismatch}
    end.
    

verify_client_scope( #client{client_id = ClientID},Scope, _) ->
	case get(?SCOPE_TABLE, Scope) of
        {ok, #scope{scope = Scope, client_id = Client}} ->     
			case ClientID =:= Client of
				true -> {ok, {[],Scope}};
				_ -> {error, invalid_client}
			end;
        Error = {error, notfound} ->  Error
    end.
verify_resowner_scope(_ResOwner, Scope, _) ->
    {ok, {[],Scope}}.

verify_scope(RegScope, _ , _) ->
    case get(?SCOPE_TABLE, RegScope) of
        {ok, #scope{scope = RegScope}} -> {ok, RegScope};
        Error = {error, notfound} ->  Error
    end.

    

%%%===================================================================
%%% Funções internas
%%%===================================================================

get(Table, Key) ->
    case ets:lookup(Table, Key) of
        [] ->
            {error, notfound};
        [{_Key, Value}] ->
            {ok, Value}
    end.

put(Table, Key, Value) ->
    ets:insert(Table, {Key, Value}),
    ok.

delete(Table, Key) ->
    ets:delete(Table, Key).
