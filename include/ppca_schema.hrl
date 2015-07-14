%%********************************************************************
%% @title db_schema
%% @version 1.0.0
%% @doc Contém definições das tabelas para persistência.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-record(sequence, {key, index}).
-record(user, {id, nome, email}).

