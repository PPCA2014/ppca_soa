-module(ems_django_service).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").

-export([execute/1]).

execute(Request) ->	
	case ems_request:load_from_file_req(Request) of
		{ok, Request2 = #request{filename = Filename, 
								 service = Service}} ->
			FileNameTemplate =  Filename ++ ".django.template",
			ModuleNameTemplate = filename:basename(FileNameTemplate),
			case load_module_template(FileNameTemplate, ModuleNameTemplate) of
				{ok, ModuleTemplate} -> 
					%Request3 = Request2#request{module_name = Filename,
				%								module = list_to_atom(FileName),
			%									function = execute},
					%case ems_dispatcher:dispatch_service_work(Request3, 
					case ems_page:render(ModuleTemplate, []) of
						{ok, Content} ->
							{ok, Request#request{code = 200, 
												 reason = ok,
												 response_data = Content}
							};
						Error2 -> Error2
					end;
				_ -> {error, Request2#request{code = 500, 
											  reason = einvalid_django_sintax,
											  response_data = ems_schema:to_json({error, einvalid_django_sintax})}
					 }
			end;
		Error -> Error
	end.
   
    
load_module_template(FileNameTemplate, ModuleNameTemplate) ->
	case code:ensure_loaded(ModuleNameTemplate) of
		{module, _} -> {ok , ModuleNameTemplate};
		_Error -> 
			case ems_page:compile_file(FileNameTemplate, ModuleNameTemplate) of
				{ok, ModuleTemplate} -> 
					ems_logger:error("ems_django_service compile django template to ~p.", [FileNameTemplate]),
					{ok , ModuleNameTemplate};
				{error, Reason} = Error ->
					ems_logger:error("ems_django_service compile invalid django template to ~p. Reason: ~p.", [FileNameTemplate, Reason]),
					Error
			end
	end.
	
