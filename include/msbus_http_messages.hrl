%%********************************************************************
%% @title http_messages
%% @version 1.0.0
%% @doc Contém definições de mensagens HTTP.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

%% Mensagens de erro 
-define(MSG_SERVICO_NAO_ENCONTRADO, "Serviço ~p não encontrado.").
-define(MSG_SERVICO_NAO_DISP, "Serviço ~p listado no catálogo mas não disponível.").
-define(MSG_SERVICO_FALHOU, "Serviço ~p falhou ao atender solicitação. Motivo: ~p.").


%% Mensagens de erro HTTP no formato JSON
-define(HTTP_ERROR_400, <<"{\"error\":\"HTTP 400\",\"message\":\"Requisição inválida.\"}"/utf8>>).
-define(HTTP_ERROR_401, <<"{\"error\":\"HTTP 401\",\"message\":\"Acesso não autorizado.\"}"/utf8>>).
-define(HTTP_ERROR_404, <<"{\"error\":\"HTTP 404\",\"message\":\"Serviço não encontrado.\"}"/utf8>>).
-define(HTTP_ERROR_404_FILE_NOT_FOUND, <<"{\"error\":\"HTTP 404\",\"message\":\"Arquivo não encontrado.\"}"/utf8>>).
-define(HTTP_ERROR_415, <<"{\"error\":\"HTTP 415\",\"message\":\"O servidor aceita somente JSON.\"}"/utf8>>).
-define(HTTP_ERROR_502(Motivo), io_lib:format(<<"{\"error\":\"HTTP 502\",\"message\":\"Serviço falhou ao atender solicitação. Motivo: ~p.\"}"/utf8>>, [Motivo])).
-define(HTTP_ERROR_502, <<"{\"error\":\"HTTP 502\",\"message\":\"Serviço falhou ao atender solicitação.\"}"/utf8>>).
-define(HTTP_ERROR_503, <<"{\"error\":\"HTTP 503\",\"message\":\"Serviço listado no catálogo de serviço mas não disponível.\"}"/utf8>>).
