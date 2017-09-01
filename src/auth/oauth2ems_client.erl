-module(oauth2ems_client).

-export([callback/1]).
-include("../include/ems_schema.hrl").

callback(Request) -> 
	Code = ems_request:get_querystring(<<"code">>, <<>>, Request),
	Client = {<<"q1w2e3">>,<<"123456">>},
	RedirectUri = <<"https://127.0.0.1:2302/callback">>,
	Authorization = oauth2:authorize_code_grant(Client, Code, RedirectUri, []),
    {ok,ResponseData} = issue_token_and_refresh(Authorization),
	ResponseData2 = ems_schema:prop_list_to_json(ResponseData),
	{ok, Request#request{code = 200, 
		 response_data = ResponseData2}
	}.  



issue_token_and_refresh({ok, {_, Auth}}) ->
	{ok, {_, Response}} = oauth2:issue_token_and_refresh(Auth, []),
	{ok, oauth2_response:to_proplist(Response)};
issue_token_and_refresh(Error) ->
    Error.
