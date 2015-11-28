REM build msbus with rebar
REM author Everton de Vargas Agilar <<evertonagilar@gmail.com>>
rmdir Mnesia*
tools\rebar\rebar get-deps -q
tools\rebar\rebar compile
