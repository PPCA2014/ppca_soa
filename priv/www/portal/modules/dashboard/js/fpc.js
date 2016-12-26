'use strict';



/*!
 * fpc -- Framework de primeira camada
 * Copyright 2011-2015 Everton de Vargas Agilar.
 * Project: https://github.com/eliot-framework/eliot
 */


function FpcError(message, url, params) {
    this.name = "FpcError";
    this.message = message + "\nRequest: " + url + "\nParams: "+ JSON.stringify(params) + ")";						
}
FpcError.prototype = Error.prototype;


var fpc = {
    csrftoken : "",
    lazyFields : null,
    erlangms_url: "http://localhost:2301",
    username : "everton",
    password : "123456",

    callRest : function(url, params, method){
    	if (!url.startsWith("/")){
    		var url = this.erlangms_url + url;
    	}
    	var params = (params == undefined ? {} : params);
    	var method = (method == undefined ? "GET" : method);
	    return $.ajax({
            url:  url,
            data : params,
            type: method,
            contentType: "application/x-www-form-urlencoded; charset=UTF-8",
            dataType: "json",
            crossDomain: true,
            success: function(response) {
				return response;
            },
            error: function(xhr, textStatus) {
		    	var error = xhr.responseJSON;
		    	document.body.style.cursor = "default";
				fpc.mensagem(error, "erro");
				throw new FpcError(error, url, params);
            }
		});
    },

    callRestIfNull : function(value, url, params){
    	if (value == null){
    		return this.callRest(url, params);
    	}else{
    		var ret = {
    			done : function(obj){
    				return function(fn) {
    					return fn(value);
    				}
    			}(value)
    		};
    		return ret;
    	}
    },
    
   	getJSON : function(url, params){
	    return $.ajax({
            url:  url,
            data : params,
            type: "GET",
            contentType: "application/x-www-form-urlencoded; charset=UTF-8",
            dataType: "json",
            //timeout: 6500,
            crossDomain: true,
            success: function(msg) {
				var doc = document;
				if (msg.tipo === "erro" || msg.erro != undefined){
					doc.body.style.cursor = "default"; 
					var msg_erro = fpc.mensagem(msg.message, "erro");
					throw new FpcError(JSON.stringify(msg.message), url, params);
				}
            },
            error: function(xhr, textStatus, error) {
		    	document.body.style.cursor = "default";
				fpc.mensagem(error, "erro");
				throw new FpcError(error, url, params);
            }
		});
   	},
    
    fillComboboxFromArray : function(combobox, obj){
    	if (obj instanceof Array){
    		var jCombobox = $(combobox); 
	    	for (var i=0; i < obj.length; i++) {
	    		var item = obj[i];
	    		if (item instanceof Object){
	    			var keys = Object.keys(item);
	    			jCombobox.append("<option value='"+ item[keys[0]]+ "'>" + item[keys[1]] + "</option>");
	    		}else{
	    			jCombobox.append("<option value='"+ item[0]+ "'>" + item[1] + "</option>");	
	    		}
	    		
	        }
    	}
    },
    
    setComboboxValue : function(field, value){
    	if (typeof value != "string"){
        	value = value.toString()
    	}
    	if (field != null && value != null && value != ""){ 
        	  for (var j = 0, len_field = field.length; j < len_field; j++) {
                  if (field[j].value === value) {
                    field[j].selected = true;
                    break;
                  }
              }
    	}
    },
    
    getValueFromCombobox : function (field){
        if (field != undefined){
	    	for (var i=0; i < field.length; i++) {
	            if (field[i].selected) {
	            	var result = field[i].value;    
	            	return result === "-" ? undefined : result;
	            }
	        }
        }
        return undefined;
    },    

    getValueFromRadio : function(radio, form){
    	if (radio != undefined){
	    	var tipo = typeof radio;
	    	if (tipo === "object"){
	    		var inputName = radio.name;
	    	}else if (tipo === "string"){
	    		var inputName = radio;
	    	}else{
	    		throw new Error("Argumento inválido para getValueFromRadio: deve ser um radio ou nome do radio.");
	    	}
	    	if (form != undefined){
	    		var radios = form.querySelectorAll('[name='+ inputName + ']')	
	    	}else{
	    		var radios = document.getElementsByName(inputName);
	    	}
	    	for (var i = 0, length = radios.length; i < length; i++) {
	    	    if (radios[i].checked) {
	    	        return radios[i].value;
	    	    }
	    	}
    	}
    	return undefined;
    },

    getValueFromRadioAsBoolean : function(radio, form){
    	var value = fpc.getValueFromRadio(radio, form);
    	if (value != undefined){
    		return value == true || value === "true" || value === "1" || value === "sim" || value === "yes";
    	}else{
    		return undefined;
    	}
    },

    setRadioValue : function(field, value){
    	if (field != null && value != null && value != ""){
    		var inputName = field.name;
    		var fields = field.form.querySelectorAll('[name='+ inputName + ']')	
	    	for (var i = 0, length = fields.length; i < length; i++) {
	    	    if (fields[i].value === value) {
	    	        fields[i].checked = true;
	    	        break;
	    	    }
	    	}
    	}
    },

    postUrl : function (url, params) {
       if (url != undefined){
	    	var doc = document;
	    	var form = doc.createElement('form');
	        form.action = url;
	        form.method = 'POST';
	        var input = doc.createElement('input');
	        input.type = 'hidden';
	        input.name = "csrfmiddlewaretoken";
	        input.value = fpc.csrftoken;
	        form.appendChild(input);
	        for (var i in params) {
	            if (params.hasOwnProperty(i)) {
	                var input = doc.createElement('input');
	                input.type = 'hidden';
	                input.name = i;
	                input.value = params[i];
	                form.appendChild(input);
	            }
	        }
	        doc.body.appendChild(form);
	        form.submit();
       }else{
    	   throw new Error("Erro no método postUrl. Informa a url!");
       }
    },

    getCookie : function (name) {
        var doc = document;
    	var cookieValue = null;
        if (doc.cookie && doc.cookie !== '') {
            var cookies = doc.cookie.split(';');
            for (var i = 0, len = cookies.length; i < len; i++) {
                var cookie = jQuery.trim(cookies[i]);
                // Does this cookie string begin with the name we want?
                if (cookie.substring(0, name.length + 1) == (name + '=')) {
                    cookieValue = decodeURIComponent(cookie.substring(name.length + 1));
                    break;
                }
            }
        }
        return cookieValue;
    },    

    somenteNumeros : function (field){
        $(field).on('keypress', function(event){
    		var charCode = event.charCode;
    		var keyCode = event.keyCode;
        	if (charCode === 0 && keyCode > 0){ 
    			return true;
    		}
    		if (!(charCode >= 48 && charCode <= 57)){
                return false;
            }
            return true;
        });
    },

    formatDecimal : function (field){
    	try{
    		var field_value = field.value; 
			if (field_value === ""){
				return;
			}
    		if (field_value.charAt(field_value.length-1) === ","){
        		field.value = field_value.substring(0, field_value.length-1);
        	}
        	field.value = parseFloat(field_value.replace(",", ".")).toFixed(field.dataset.decimalPlaces).replace(".", ",");
        	if (field.value.length > this.maxLength){
        		alert("Valor " + field.value + " maior que o tamanho do campo!");
        		field.value = "";
        	}
    	}catch (e){
    		// ignora qualquer erro de processamento
    	}
    },
    
    somenteCaixaAlta : function (field){
    	$(field).focusout(function() {
            this.value = this.value.toLocaleUpperCase();
        });
    },
    
    somenteDecimal : function (field){
    	 $(field).on('keypress', function(event){
        	if (event.charCode === 0 && event.keyCode > 0){ 
        			return true;
        	}
         	var charCode = event.charCode;
             if ((charCode >= 48 && charCode <= 57) || (charCode == 44)){
             	var value = field.value;
             	var idxVirgula = value.indexOf(",", 0);

                 // Digitou separador decimal
                 if (charCode == 44){
                     if (idxVirgula == -1){
                         return true;
                     }
                     else{
                         return false;
                     }
                 }

                 if (idxVirgula != -1){
                     var sCentavos = value.substring(idxVirgula+1);
                     if (sCentavos.length == this.dataset.decimalPlaces && value.selectionStart == value.length){
                         return false;
                     }
                 }

                 return true;
             }
             return false;
         });
         
         $(field).on('change', function (event){
     		if (this.value.length !== 0){
	        	fpc.formatDecimal(this);
        	}
         }); 
    },

    somenteData : function (field){
    	$(field).on('keydown', function (event){
        	try{
        		if (event.charCode === 0 && event.keyCode === 9 && field.value.length !== 0 && field.value !== "" && field.value !== "__/__/____"){
		        	fastForm.isDate(field.value);
	        	}
	        	return true;
        	}catch (e){
        		// ignora qualquer erro de processamento
        		return true;
        	}
    	});
	},
    
    isDate : function(txtDate)
    {
      var currVal = txtDate;
      if(currVal === '' || currVal.length !== 10){
        return false;
      }
       
      // Regex 
      var rxDatePattern = /^(\d{1,2})(\/|-)(\d{1,2})(\/|-)(\d{4})$/;
      var dtArray = currVal.match(rxDatePattern); // is format OK?
     
      if (dtArray === null){
         return false;
      }
      
      // Verifica para mm/dd/yyyy format.
      var dtDay = dtArray[1];
      var dtMonth= dtArray[3];
      var dtYear = dtArray[5];
     
      if (dtMonth < 1 || dtMonth > 12){
    	  alert("Atenção: A data " + currVal + " não é válida!");
          return false;
      }
      else if (dtDay < 1 || dtDay> 31){
    	  alert("Atenção: A data " + currVal + " não é válida!");
          return false;
      }
      else if ((dtMonth === 4 || dtMonth === 6 || dtMonth === 9 || dtMonth === 11) && dtDay === 31){
    	  alert("Atenção: A data " + currVal + " não é válida!");
          return false;
      }
      else if (dtMonth == 2)
      {
         var isleap = (dtYear % 4 === 0 && (dtYear % 100 !== 0 || dtYear % 400 === 0));
         if (dtDay> 29 || (dtDay === 29 && !isleap)){
        	 alert("Atenção: A data " + currVal + " não é válida!");
              return false;
         }
      }
      
      // Limite inferior
      if (dtYear < 1900){
    	  alert("Atenção: Você informou uma data muito antiga!");
      }
      
      var anoFuturo = new Date().getFullYear()+10;
      
      // Avisa se informado uma data futura
      if (dtYear > anoFuturo){
    	alert("Atenção: Você informou uma data superior a 10 anos!");
      }
      
      return true;
    },

   
    updateFields : function(form, update_fields){
    	if (form != undefined && update_fields != undefined){
	    	try{
		    	var list_fields = $.makeArray(form.querySelectorAll('[data-field]'));
		    	for (var field_update in update_fields){
		    		var value = update_fields[field_update];
		    		var is_object = typeof value  === "object";
					for (var i = 0, len = list_fields.length; i < len; i++){
						var field = list_fields[i];
						var dat = field.dataset;
						var field_name = field.dataset.field;
						if (field_name === field_update || ((field_name === "id" && field_update == "pk") || (field_name === "pk" && field_update == "id"))){
							if (is_object){
								field.value = value.desc;
								field.dataset.value = value.id;
							}else{
								switch (dat.type){
									case "radio":
										fpc.setRadioValue(field, value);
										break;
									case "combobox", "dropdown", "select":
						            	value = dat.key;
										fpc.setComboboxValue(field, value);
							            break;
							         default:
							        	field.value = value;
								}
									
							}
							field.dataset.dirty = true;
						}
					}
				}
			}catch (e){
				fpc.mensagem("Erro no método updateFields do formulário "+ form.id  + ". " + e + ".", "erro");
			}
    	}
    },
    
    configFields : function(){
		// Pesquisa todos os campos com a tag data-type 
		// mas que ainda não foram configuradas pela biblioteca fpc
		// A configuração a realizada apenas uma vez por campo
		var list_fields = $.makeArray(document.querySelectorAll('[data-type'));
		var qtd_fields = list_fields.length;
		var doc=document;
		if (qtd_fields > 0) {
			var jdoc = $(document); 
			for (var i = 0, len = qtd_fields; i < len; i++){
				var input = list_fields[i];
				if (input.type != undefined){
					var dat = input.dataset;
					var data_type = dat.type;
					if (!dat.fpc && data_type != undefined){
						input.style.backgroundColor="white";
						dat.fpc=true;
						if (data_type === "number"){
								dat.type = "number";
								this.somenteNumeros(input);
						} 
						else if (data_type === "decimal") {
							  dat.type = "decimal";
							  if (dat.decimalPlaces == undefined){
								  dat.decimalPlaces = 2;
							  }
							  this.somenteDecimal(input);
						}
						else if (data_type === "date" || data_type === "datetime"){
							  this.somenteData(input);
							  var input_ant=input.previousElementSibling;
							  var parent_input=input.parentNode;

							  //if (input_ant != undefined && input.previousElementSibling.classList.contains("form-group")){
							  if (parent_input != undefined && parent_input.classList.contains("form-group")){
								  var new_form_group=false;
								  var form_group=parent_input;
								  form_group.classList.remove("form-group");
							  }else{
								  var new_form_group=true;
								  var form_group=doc.createElement("div");
							  }

							  form_group.classList.add("input-group");
							  form_group.classList.add("date");
							  
							  // Cria a span para o botão do calendário
							  var span_input_group=doc.createElement("span");
							  span_input_group.classList.add("input-group-addon");
							  //span_input_group.classList.add("btn"); 
							  //span_input_group.classList.add("btn-default"); 
							  //span_input_group.classList.add("btn-xs"); 
							  //span_input_group.style.height = "10px";
							  //span_input_group.style.min_height = "10px";
							  var span_input_glyphicon=doc.createElement("span");
							  span_input_glyphicon.classList.add("glyphicon");
							  span_input_glyphicon.classList.add("glyphicon-calendar");
							  //span_input_glyphicon.style.height = "10px";
							  span_input_group.appendChild(span_input_glyphicon);
							  form_group.appendChild(span_input_group);
							  
							  if (new_form_group){
								form_group.appendChild(input);
								parent_input.appendChild(form_group);
							  }
							  
							  switch (data_type) {
								  case "date" :
		  							  $(input).mask("99/99/9999");
									  input.style.width="80px";
									  $(form_group).datetimepicker({
											language:  'pt-BR',
											format: 'dd/mm/yyyy',
											weekStart: 1,
											todayBtn:  1,
											autoclose: 1,
											todayHighlight: 1,
											startView: 2,
											minView: 2,
											forceParse: 0
										}); 
										break;
									case "datetime" :
										$(input).mask("99/99/9999 99:99");
									    input.style.width="110px";
										$(form_group).datetimepicker({
											language:  'pt-BR',
											format: 'dd/mm/yyyy hh:ii',
											weekStart: 1,
											todayBtn:  1,
											autoclose: 1,
											todayHighlight: 1,
											startView: 2,
											minView: 2,
											forceParse: 0,
											showMeridian: 1
										});		
										break
								}
										

						}else if (data_type === "text"){
							  if (dat.caixaAlta != undefined){
								  this.somenteCaixaAlta(input);
							  }
							  if (dat.mascara != undefined){
								  $(input).mask(dat.mascara, {placeholder: dat.mascaraPlaceholder});
							  }
						}else if (data_type === "combobox" || 
							      data_type === "dropdown" || 
								  data_type === "select"){
							  dat.type = "combobox";
						}
						  
						if (dat.noEditable != undefined){
							input.setAttribute("readonly", "readonly");
							input.style.backgroundColor="LightYellow";
						}
			
						if (dat.noInsertable != undefined){
							input.setAttribute("readonly", "readonly");
							input.style.backgroundColor="LightYellow";
						}
					  
						var label = fpc.getLabelFromField(input);
						if (label != undefined){
							if (dat.required != undefined){
								label.style.fontWeight="bold";
							}else{
								label.style.fontWeight="";
							}
						}
					}
				}
			}
		}
    },
    
    getLabelFromField : function(field){
    	var label = undefined;
    	if (field != undefined){
	    	if (field.id != undefined){
	    		label = field.form.querySelector('label[for="'+ field.id + '"]');
	    	}else if (field.name != undefined){
	    		label = field.form.querySelector('label[for="'+ field.name + '"]');
	    	}
    	}
    	return label;
    },
    
    validaForm : function (form){
    	if (form != undefined){
	        var fieldLabels = [];
	        var fieldRequeridos = $(form).find(':input[data-required]');
	        for (var i = 0, len = fieldRequeridos.length; i < len; i++){
	            var field = fieldRequeridos[i];
	            var field_value = fpc.getFieldValue(field);
	            if ((field_value === "" || field_value == undefined) && field.dataset.autoIncrement == undefined){
	            	var label = this.getLabelFromField(field);
	                if (label !== undefined && label !== null){
	                	var titulo = label.firstChild.textContent;
	                	if (fieldLabels.indexOf(titulo) == -1){
	                		fieldLabels.push(titulo);
	                		
	                		// Adiciona o marcador de erro no input
	                		$(field.parentElement.parentElement).addClass("has-error");
	                		$(field.parentElement.parentElement).addClass("has-feedback");
	                		$(field).after('<span class="glyphicon glyphicon-remove form-control-feedback"></span>')
	                	}
	                }
	            }
	        }
	
	        // limpar as mensagens anteriores se existir
	        fpc.mensagem(""); 
	
	        if (fieldLabels.length > 0){
	        	fpc.mensagem("Campos com o título em negrito são obrigatórios.", "warn");
	            if (fieldLabels.length == 1){
	            	fpc.mensagem(fieldLabels + ' não foi informado.', "erro");
	            }else{
	                fieldLabels = fieldLabels.join(", ");
	            	fpc.mensagem(fieldLabels + ' não foram informados.', "erro");
	            }
	
				// Refaz a validação após realizada edição
	    		$(form).one("change", function(event){
	    			$(this.getElementsByClassName('form-control-feedback')).remove();
	    			$(this.getElementsByClassName('has-error')).removeClass("has-error has-feedback");
	    			fpc.validaForm(this);
				});
	            
	            return false;
	        }
	        
	        return true;
    	}else{
    		return undefined;
    	}
    },    
    
    getQueryStringFromForm : function(form){
    	if (form != undefined){
	    	var fields = $(form).find("input[type='text']");
	    	var result = "";
	    	for (var i = 0, len = fields.size(); i < len; i++){
	    		var f = fields[i];
	    		if (f.value.trim() !== ""){ 
	    			result += f.id + "=" + escape(f.value) + "&";
	    		}
	    	}
	    	if (result !== ""){
	    		result = result.slice(0, result.length-1);
	    	}
	    	return result;
    	}else{
    		return undefined;
    	}
    },

    isFieldChanged : function(field){
    	if (field != undefined){
	    	var dat = field.dataset;
	    	var dfield = dat.field;
	    	if (dfield === "id" || dfield === "pk"){
	    		return true;
	    	}
			var field_type = field.type;
			if (field_type === "radio"){
				var field_value = fpc.getValueFromRadio(field.name, field.form); 
	    		if (field_value != undefined && field_value !== dat.value){
	       			return true;
	    		}
			} else if (field_type === "select-one"){
				var field_value = fpc.getValueFromCombobox(field);
	    		if (field_value != undefined && field_value !== dat.value){
	       			return true;
	    		}
			}else{
		    	if (dat.type === "lookup"){
					var dat_key = dat.key; 
		    		if (dat_key != undefined && dat.value !== dat_key){
						return true;
					}
				}else {
					var field_value = field.value;
					if (dat.type === "data" && field_value !== dat.value){
						return true;
					}else if (field_value != undefined && field_value !== dat.value){
		       			return true;
		    		}
				}
			}
			return false;
    	}else{
    		return undefined;
    	}
    },

    getFieldValue : function(field){
    	var dat = field.dataset;
		var field_type = field.type;
		if (field_type === "radio"){
			var field_value = fpc.getValueFromRadio(field.name, field.form); 
    		return field_value;
		} else if (field_type === "select-one"){
			var field_value = fpc.getValueFromCombobox(field);
    		return field_value;
		}else{
	    	if (dat.type === "lookup"){
				var dat_key = dat.key; 
	    		return dat_key;
			}else {
				var field_value = field.value;
				if (dat.type === "data" && field_value === "__/__/____"){
					return undefined;
				}
				return field_value;
			}
		}
		return false;
    },
    
    getObjectAsJson : function(form, all_fields){
    	var obj = this.getObject(form, all_fields);
    	return obj == undefined ? "{}" : JSON.stringify(obj);
    },
    
    getObject : function(form, all_fields){
    	if (form != undefined){
	    	try{
		    	var list_fields = $.makeArray(form.querySelectorAll('[data-field]'));
		    	var obj = {};
		    	var fields_dirty = [];
		    	var all_fields = all_fields != undefined && all_fields;
		    	for (var i = 0, len = list_fields.length; i < len; i++){
		    		var field = list_fields[i];
		    		// se o field estiver em outro form ou for undefined não serializar 
		    		if (field.form != form || field.type == undefined){
		    			continue; 
		    		}
		    		var dat = field.dataset;
		    		var dfield = dat.field;
		    		var dtype = dat.type;
		    		if (dfield != undefined && (fpc.isFieldChanged(field) || all_fields) && fields_dirty.indexOf(dfield) == -1) {
		    			if (dtype === "lookup"){
		    				obj[dfield] = escape(dat.key);
			    		}else {
			    			var field_type = field.type;
			    			if (field_type === "radio"){
			    				var value = fpc.getValueFromRadio(field.name, form);
			    				if (value != undefined){ 
			    					obj[dfield] = value;
			    				}
			    			} else if (field_type === "select-one"){
			    				var value = fpc.getValueFromCombobox(field);
			    				if (value != undefined){
			    					obj[dfield] = value;
			    				}
			    			}else{
			    				if (dtype === "decimal"){
			    					obj[dfield] = field.value.replace(",", ".");
			    				}else{
			    					obj[dfield] = field.value;
			    				}
			    			}
			    		}
			    		fields_dirty.push(dfield);
		    		}
		    	}
		    	if (fields_dirty.length > 0){
		    		return obj;
		    	}else{
		    		return undefined;
		    	}
	        }catch (e){
				fpc.mensagem("Erro ao serializar objeto do form "+ form.id + "(no metodo getObject). " + e + ".", "erro");
			}
    	}else{
    		return undefined;
    	}
    },
    
    serializeForm : function(form){
    	if (form != undefined){
    		try{
	    		var list_fields = $.makeArray(form.querySelectorAll('[data-field]'));
		    	var result = [];
		    	var fields_dirty = [];
		    	for (var i = 0, len = list_fields.length; i < len; i++){
		    		var field = list_fields[i];
		    		// se o field estiver em outro form não serialize 
		    		if (field.form != form){
		    			continue; 
		    		}
		    		var dat = field.dataset;
		    		var dfield = dat.field;
		    		if (dfield != undefined && fpc.isFieldChanged(field) && fields_dirty.indexOf(dfield) == -1) {
		    			result.push(dfield);
		    			result.push("=");
		    			if (dat.type === "lookup"){
			    			result.push(escape(dat.key));
			    		}else {
			    			var field_type = field.type; 
			    			if (field_type === "radio"){
			    				value = fpc.getValueFromRadio(field.name);
			    				if (value != undefined){ 
			    					result.push(escape(value));
			    				}
			    			} else if (field_type === "select-one"){
			    				value = fpc.getValueFromCombobox(field);
			    				if (value != undefined){
			    					result.push(escape(value));
			    				}
			    			}else{
			    				if (dtype === "decimal"){
			    					result.pusth(field.value.replace(",", "."));
			    				}else{
				    				result.push(escape(field.value));
			    				}
			    			}
			    		}
			    		result.push("&");
			    		fields_dirty.push(dfield);
		    		}
		    	}
		    	if (result.length > 0){
		    		result.pop();
		    	}
		    	return result.join("");
	        }catch (e){
				fpc.mensagem("Erro ao serializar objeto do form "+ form.id + "(no metodo serializeForm). " + e + ".", "erro");
			}
    	}else{
    		return "";
    	}
    },
    
    serializeFormParaPesquisa : function(form){
    	var fields = $.makeArray(form.getElementsByClassName("form-control"));
    	var result = "";
    	if (fields.length > 0){
	    	for (var i = 0, len = fields.length; i < len; i++){
	    		var f = fields[i];
	    		var dat = f.dataset;
	    		var dfield = dat.field;
	    		if (dfield != undefined){
	        		var dtype = dat.type; 
	    			var value = f.value;
	    			if (dtype != undefined && value != undefined){
			    		if (dtype === "lookup"){
			    			var dkey = dat.key;
			    			if (dkey != undefined && value.trim() !== ""){
			    				result += dfield + ":" + escape(dkey) + ".@.";
			    			}
			     		}else {
			    			if (value.trim() !== "" && value !== "-" && value !== "-1"){
				       			result += dfield + ":" + escape(value) + ".@.";
				    		}
			    		} 
	    			}
	    		}
	    	}
	    	if (result !== ""){
	    		result = result.slice(0, result.length-3);
	    	}
    	}
    	return result;
    },

    mensagem : function mensagem(msg, tipo){
		var alerta = $("#alerta");
    	if (alerta != undefined) {
			if (msg != undefined && msg !== "") {
				var msg_text = "";
				
				// Cria a tag do alerta se necessário
				if (alerta.length === 0){
					$("#sep_breadcrumb").after("<div id='alerta' style='display:none'/>");
					alerta = $("#alerta");
				}
	
				// adiciona todas as mensagens na tag alerta
				if (msg instanceof Array){
					for (var i in msg){
						var str = '<i class="glyphicon glyphicon-remove" style="color: #a94442;"/>' + msg[i];
						msg_text += "<span>"+ str + "</span>";
					}
					if (msg_text != ""){
						alerta.append(unescape(msg_text));
					}
				}else if (msg instanceof Object){
					var message = msg.message;
					if (message != undefined){
						var msg_text = '<span><i class="glyphicon glyphicon-remove" style="color: #a94442;"/>' + message + '</span>';
					}else{
						var warnings = msg.warnings; 
						if (warnings != undefined){
							for (var i in warnings){
								var str = '<i class="glyphicon glyphicon-warning-sign"/>' + warnings[i].msg;
								msg_text += "<span>"+ str + "</span>";
							}
						}
		
						var infos = msg.infos; 
						if (infos != undefined){
							for (var i in infos){
								var str = '<i class="glyphicon glyphicon-ok"/>' + infos[i].msg;
								msg_text += "<span>"+ str + "</span>";
							}
						}
		
						var errors = msg.errors; 
						if (errors != undefined){
							for (var i in errors){
								var str = '<i class="glyphicon glyphicon-remove" style="color: #a94442;"/>' + errors[i].msg;
								msg_text += "<span>"+ str + "</span>";
							}
						}
					}
					
					if (msg_text != ""){
						alerta.append(unescape(msg_text));
					}
					
				}else {
					msg = unescape(msg);
					if (tipo === "info" || tipo == undefined || tipo === "") {
						msg_text = '<i class="glyphicon glyphicon-ok"/>' + msg;
					} else if (tipo === "erro" || tipo === "error") { 
						msg_text = '<i class="glyphicon glyphicon-remove" style="color: #a94442;"/>' + msg;
					} else if (tipo === "warn" || tipo === "atencao") { 
						msg_text = '<i class="glyphicon glyphicon-warning-sign"/>' + msg;
					}
					alerta.append("<span>"+ msg_text + "</span>");
				}
				
				
				alerta.css("display", "block");
	            $('html, body').animate({
	                scrollTop: $("#painel_conteudo").offset().top-80}, 700);
				$("#f_cadastro").one("change", function(event){
					fpc.mensagem("");
				});
				
				return msg_text;
	   		}
	   		else
	   		{
				if (alerta.length > 0){
					alerta.remove();
	   			}
				return "";
	   		}
    	}
   	},
   	
   	
   	limparMensagem : function(){
		var alerta = $("#alerta");
		if (alerta != undefined && alerta.length > 0){
			alerta.remove();
		}
   	},
   	
   	parcialUpdate : function parcialUpdate(parcial_update){
   		for (i in parcial_update){
   			var obj = parcial_update[i];
   			for (var id in obj){
   				$("#"+ id).html(obj[id]);
   			}
   		}
   	},
    
   	ocultaMensagem : function ocultaMensagem(){
   		$("#alerta").css("display", "none");
   	},
   	
   	
   	updateGrid : function(obj_id, grid_dados){
		// Atualiza a linha do registro selecionado na grid sem fazer request
		var row_atual = null;
		var doc = document;
		// Se for uma edição atualiza o registro da grid com os dados atuais do registro   
		if (obj_id !== ""){
			row_atual = $("input[value='" + obj_id + "']").parent().parent();
			var idx_col = -1;
			row_atual.children("td").each(function(){ 
				if (idx_col > -1){
					$(this).text(grid_dados[idx_col]);
				}
				idx_col++;
			});
		}else{ // Se for inserção, adiciona o novo item na gride
			row_atual = doc.createElement("tr"); 
			fpcDataTable.forceRefresh = true;

			if ($("#dados").find("tbody").children().length > 0){
				if (fpcDataTable.idsNovos === ""){ 
					fpcDataTable.idsNovos = obj_id;
				}else{
					fpcDataTable.idsNovos += "," + obj_id;
				}

				if ($("#dados").find("tbody").children().first().hasClass("odd")){
					$(row_atual).addClass("even");
				}else{
					$(row_atual).addClass("odd");
				}
				$(row_atual).append('<td><input type="radio" name="f_id" onclick="fpc.selecionaRegistroConsultaEvent(this.value)" value="'+ obj_id + '"></td>');
				for (i in grid_dados){
					$(row_atual).append("<td>" + grid_dados[i] + "</td>");
				}
			
				$("#dados").find("tbody").children().first().before($(row_atual));
				$("#dados").find("tbody").children().first().find("input").click();
			}
		}
   	},
   	
    pesquisar : function (ts, is_consulta){
		var doc = document;
    	var frmFiltro = is_consulta ? doc.getElementById("filtro_consulta") : doc.getElementById("filtro");
    	var controller = fpc.findController();
    	var filtro = this.getObjectAsJson(frmFiltro);
		if (is_consulta){
				var id_dados_wrapper_jquery = "#dados_consulta_wrapper";
				var id_dados_pesquisa_jquery = "#dados_pesquisa_consulta";
				var id_filtro_pesquisa_jquery = "#filtro_pesquisa_consulta";
				var id_dados_jquery = "#dados_consulta";
				var event_selecionaRegistro = "' onclick='fpc.selecionaRegistroConsulta(this)'";
			}else{
				var id_dados_wrapper_jquery = "#dados_wrapper";
				var id_dados_pesquisa_jquery = "#dados_pesquisa";
				var id_filtro_pesquisa_jquery = "#filtro_pesquisa";
				var id_dados_jquery = "#dados";
				var event_selecionaRegistro = "' onclick='fpc.selecionaRegistro(this)'";
			}
		
		var tbl_dados = $(id_dados_jquery);
		var dadospesquisa = $(id_dados_pesquisa_jquery);
		dadospesquisa[0].dataset.id = "";
		dadospesquisa.css("display", "block");
		$(id_filtro_pesquisa_jquery).css("display", "none");

		if (!$(id_dados_wrapper_jquery).length){
			tbl_dados[0].dataset.html_orig = tbl_dados.html(); 
			fpcDataTable.idsNovos = "";
		}else{
			tbl_dados.dataTable().fnDestroy();
			var tbl = tbl_dados[0];
			dadospesquisa = tbl.parentNode;
			var html_tbl = tbl.dataset.html_orig; 
			var nova_tbl = doc.createElement("table");
			if (is_consulta){
				nova_tbl.setAttribute("id", "dados_consulta");
			}else{
				nova_tbl.setAttribute("id", "dados");
			}
			nova_tbl.dataset.html_orig = html_tbl;
			dadospesquisa.removeChild(tbl);
			dadospesquisa.appendChild(nova_tbl);
			tbl_dados = $(id_dados_jquery);
			tbl_dados.html(html_tbl);
			fpcDataTable.forceRefresh = true;
			fpcDataTable.idsNovos = "";
		}
		
		var f_state = doc.getElementById("f_state");
		var service_url = f_state.dataset.serviceUrl;
		if (service_url != "" && service_url != undefined){
			filtro = this.fireOnGetFiltroPesquisa(controller, filtro);
			service_url += "?ts="+ ts + "&filtro=" + filtro + "&fields=" + f_state.dataset.fieldsGrid;
			service_url = this.erlangms_url + service_url;
		}else{
			service_url = "/fpc.views.fpc_pesquisar?ts="+ ts + "&filtro='" + filtro + "'&filtroIds=''&isconsulta=" + is_consulta;
		}
		
		fpcDataTable.createDataTable(
				tbl_dados,
				service_url,
				is_consulta,
				function( nRow, aData, iDataIndex ) {
					if (nRow.firstChild != undefined){
						if (fpcDataTable.is_consulta){
			        		  nRow.onclick = function(){
			   				    	var divDadosPesquisaConsulta = document.getElementById("dados_pesquisa_consulta");  
			   				    	var chkSeleciona = this.firstChild.firstChild;
			   				    	var dat = divDadosPesquisaConsulta.dataset; 
			   				    	dat.id = chkSeleciona.value;
			   				    	dat.value = chkSeleciona.dataset.str;
			   				    	chkSeleciona.checked = true;
			   	   				};
			   	   			}else{
			   	   				nRow.onclick = function(){
			   	   					var divDadosPesquisa = document.getElementById("dados_pesquisa");
			   	   					var chkSeleciona = this.firstChild.firstChild;
			   	   					divDadosPesquisa.dataset.id = chkSeleciona.value;
			   	   					chkSeleciona.checked = true;
			   	   				};
			   	   			}
						}
					},
				function ( data, type, row, meta ) {
						var dat = meta.settings.aoHeader[0][meta.col].cell.dataset;
						var tipo = dat.type;
						if (meta.col == 0){
							return "<input class='fpc-option-grid' type='radio' name='f_id' value='"+ data + "'/>";
						}else{
							if (fpcDataTable.hasOnFormatCellDatatable){
								return fpc.fireOnFormatCellDataTable(dat.field, dat.type, data, meta.row, meta.col, row, fpcDataTable.controller);
							}else{
								var type_data = typeof data;
								switch (type_data){
									case "boolean": return data == true || data === "true" || data === "1" || data === "sim" || data === "yes" ? "Sim" : "Não";
									default: return data;
 								}
							}
						}
					},
					fpc.findController()
		);

		if (is_consulta){
				fpc.montaBarraBotaoConsulta("lista");
			}else{
				fpc.montaBarraBotao("lista");
			}
   					
	}
   	
};



///////////////////////  	fpcDataTable  	///////////////////////   



var fpcDataTable = {
		forceRefresh: false,
		idsNovos: "",
		selecionaPrimeiroReg: false,
		is_consulta: false,
		controller: undefined,
		hasOnFormatCellDatatable : false,
		
	    createDataTable : function(tbl, a_url, is_consulta, row, col, controller){
			fpcDataTable.is_consulta = is_consulta;
			fpcDataTable.controller = controller;
			fpcDataTable.hasOnFormatCellDatatable = controller != undefined && controller.on_format_cell_datable != undefined ? true : false;
			
			tbl.dataTable( {
				"deferRender": true,
				"processing": false,
				"serverSide": true,
	   	        "paginationType": "full_numbers",
		   	    "language": {
		   	    	  "processing":     "Pesquisando...",
		              "lengthMenu": "Exibir _MENU_ registros por página",
		              "zeroRecords": "Nenhum registro encontrado",
		              "info": "Exibindo _START_ até _END_ de _TOTAL_ registros",
		              "infoEmpty": "Exibindo 0 até 0 de 0 registros",
		              "infoFiltered": "(Filtrado de _MAX_ registros totais)",
		              "search": "Filtro ",
		              "paginate": {
		                  "first": "Primeira página",
		                  "last": "Última página",
		                  "next": "Próxima",
		                  "previous": "Anterior"
		                },
				   	   "aria": {
					        "sortAscending": "Ativando ordenação ascendente para a coluna",
					        "sortDescending": "Ativando ordenação decrescente para a coluna",
					    }
		          },
		          "ajax": $.fn.dataTable.pipeline( {
		              url: a_url,
		              pages: 5 
		          }),
		          "autoWidth": false,
		          "createdRow": row,
		          "columnDefs": [
		                         {
		                             // The `data` parameter refers to the data for the cell (defined by the
		                             // `data` option, which defaults to the column being worked with, in
		                             // this case `data: 0`.
		                             "render": col,
		                             "targets": "_all"
		                         },
		                     ]		          
		          
			});
	    },
		
};
 
 
$.fn.dataTable.pipeline = function ( opts ) {
    // Configuration options
    var conf = $.extend( {
        pages: 5, // number of pages to cache
        url: ''   // script url
    }, opts );
 
    // Private variables for storing the cache
    var cacheLower = -1;
    var cacheUpper = null;
    var cacheLastRequest = null;
    var cacheLastJson = null;
 
    return function ( request, drawCallback, settings ) {
        var ajax          = false;
        var requestStart  = request.start;
        var requestLength = request.length;
        var requestEnd    = requestStart + requestLength;
         
        if ( cacheLower < 0 || requestStart < cacheLower || requestEnd > cacheUpper  || fpcDataTable.forceRefresh) {
            // outside cached data - need to make a request
            ajax = true;
            fpcDataTable.forceRefresh = false;
        }
        else if ( JSON.stringify( request.order )   !== JSON.stringify( cacheLastRequest.order ) ||
                  JSON.stringify( request.columns ) !== JSON.stringify( cacheLastRequest.columns ) ||
                  JSON.stringify( request.search )  !== JSON.stringify( cacheLastRequest.search )
        ) {
            // properties changed (ordering, columns, searching)
            ajax = true;
        }
         
        // Store the request for checking next time around
        cacheLastRequest = $.extend( true, {}, request );
 
        if ( ajax ) {
            // Need data from the server
            if ( requestStart < cacheLower ) {
                requestStart = requestStart - (requestLength*(conf.pages-1));
 
                if ( requestStart < 0 ) {
                    requestStart = 0;
                }
            }
             
            cacheLower = requestStart;
            cacheUpper = requestStart + (requestLength * conf.pages);
 
            request.start = requestStart;
            request.length = requestLength*conf.pages;
 
            /* Altera o filtroIds se necessário */
    		if (fpcDataTable.idsNovos !== ""){
    			var pIdsIni = conf.url.indexOf("&filtroIds");
    			if (pIdsIni > 0){
    				var pIdsFim = conf.url.indexOf("&", pIdsIni+10);
    				if (pIdsFim == -1){
    					pIdsFim = conf.url.length;
    				}
    				var filtroIdsAntigo = conf.url.substring(pIdsIni, pIdsFim);
    				var filtroIdsNovo = "&filtroIds='"+ fpcDataTable.idsNovos + "'";
    				conf.url = conf.url.replace(filtroIdsAntigo, filtroIdsNovo);
    			}
    		}
            
            settings.jqXHR = $.ajax( {
                "url":      conf.url,
                "data":     request,
                "dataType": "json",
                "cache":    false,
                "success":  function ( json ) {
                    var controller = fpc.findController();
                    var hasOnFormatObject = (controller != undefined && controller.on_format_object != undefined);
                	var obj_json = new Object();
                    obj_json.draw = 1;
                    obj_json.recordsTotal = 1;
                    obj_json.recordsFiltered = 1;
                    /*for (row in json){
                    	if (hasOnFormatObject){
                    		fpc.fireOnFormatObject(json[row], controller);
                    	}	
                    	json[row][0] = "<input class='fpc-option-grid' type='radio' name='f_id' value='"+ json[row][0] + "'/>"; 
                    }*/
                    obj_json.data = json;
                    json = obj_json;
                	
                	cacheLastJson = $.extend(true, {}, json);
 
                    if ( cacheLower != requestStart ) {
                        json.data.splice( 0, requestStart-cacheLower );
                    }
                    json.data.splice( requestLength, json.data.length );
                     
                    drawCallback( json );
                }
            } );
        }
        else {
            json = $.extend( true, {}, cacheLastJson );
            json.draw = request.draw; // Update the echo for each response
            json.data.splice( 0, requestStart-cacheLower );
            json.data.splice( requestLength, json.data.length );
 
            drawCallback(json);
        }
    }
};
 

/////////////////////  	draggable  	///////////////////////    

(function($) {
    $.fn.drags = function(opt) {

        opt = $.extend({handle:"",cursor:"move"}, opt);

        if(opt.handle === "") {
            var $el = this;
        } else {
            var $el = this.find(opt.handle);
        }

        return $el.css('cursor', opt.cursor).on("mousedown", function(e) {
            if(opt.handle === "") {
                var $drag = $(this).addClass('draggable');
            } else {
                var $drag = $(this).addClass('active-handle').parent().addClass('draggable');
            }
            var z_idx = $drag.css('z-index'),
                drg_h = $drag.outerHeight(),
                drg_w = $drag.outerWidth(),
                pos_y = $drag.offset().top + drg_h - e.pageY,
                pos_x = $drag.offset().left + drg_w - e.pageX;
            $drag.css('z-index', 1000).parents().on("mousemove", function(e) {
                $('.draggable').offset({
                    top:e.pageY + pos_y - drg_h,
                    left:e.pageX + pos_x - drg_w
                }).on("mouseup", function() {
                    $(this).removeClass('draggable').css('z-index', z_idx);
                });
            });
            e.preventDefault(); // disable selection
        }).on("mouseup", function() {
            if(opt.handle === "") {
                $(this).removeClass('draggable');
            } else {
                $(this).removeClass('active-handle').parent().removeClass('draggable');
            }
        });

    }
})(jQuery);

/////////////////////  	ready  	///////////////////////

$(this).ready(function(){
	fpc.csrftoken = fpc.getCookie('csrftoken');
	$(document).ajaxSend(function(event, xhr, settings) {
        xhr.setRequestHeader("X-CSRFToken", fpc.csrftoken);
        xhr.setRequestHeader("Accept", "application/json,application/zip");
        xhr.setRequestHeader ("Authorization", "Basic " + btoa(fpc.username + ":" + fpc.password));
 	});

	// Registra uma thread para configurar os inputs a cada 1 segundo
	//setTimeout(function(){
	//	setInterval(function(){ fpc.configFields() }, 3000);
	//}, 4000);
});
