-module(ems_django_service).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").

-export([execute/1]).

execute(Request) ->	
	case ems_request:load_from_file_req(Request) of
		{ok, Request2 = #request{filename = FileName, service = Service}} = FileReq->
			case ems_util:load_erlang_module(FileName) of
				{ok, ModuleController} ->
					case ems_django:load_module_template(FileName) of
						{ok, ModuleTemplate} -> 
							Service2 = Service#service{module_name = atom_to_list(ModuleController),
													   module = ModuleController,
													   function = execute},
							case ems_dispatcher:dispatch_service_work(Request2, Service2) of
								{ok, request, #request{response_data = Args}} ->
									case ems_django:render(ModuleTemplate, Args) of
										{ok, Content} ->
											{ok, Request2#request{code = 200, 
																  reason = ok,
																  response_data = Content}
											};
										Error2 -> Error2
									end;
								Error -> Error
							end;
						{error, Reason} -> {error, Request2#request{code = 500, 
																	reason = einvalid_django_sintax,
																	response_data = ems_schema:to_json({error, Reason})}
							 }
					end;
				{error, enoent} -> FileReq;
				{error, Reason} -> 
					{error, Request2#request{code = 500, 
											 reason = einvalid_django_sintax,
											 response_data = ems_schema:to_json({error, Reason})}
							 }
			end;
		Error -> Error
	end.
   
    
	
