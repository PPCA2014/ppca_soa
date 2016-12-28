import { Component, ViewContainerRef, ViewEncapsulation } from '@angular/core';
import { Overlay, overlayConfigFactory } from 'angular2-modal';
import { Modal, BSModalContext } from 'angular2-modal/plugins/bootstrap';
import { CustomModalContext, CustomModal } from './exemplos_url_servico_component';
import { Http, Response, Headers, RequestOptions } from '@angular/http';
import { Observable } from 'rxjs/Observable';

import { Catalogo } from './catalogo';

interface fpc {
    mensagem: Function;
}

declare var fpc: fpc;

@Component({
    selector: 'catalogo',
    templateUrl: 'modules/catalogo/catalogo.html',
    providers: [Modal]
})

export class CatalogoController { 
	
	private catalogoUrl = "/catalog";
	private catalogoOwnerUrl = "/catalog/owner";
	public operacao : string = "pesquisa";
	public ult_operacao : string = "pesquisa";
	public errorMessage: string;
	public owner : string = "";
	public data : any;
    public filterQuery : string = "";
    public rowsOnPage : number = 10;
    public sortBy : string = "email";
    public sortOrder : string = "asc";
    public owner_list : any = null;
    public language_list : any = [{"name" : "erlang", "title" : "Linguagem Erlang"}, {"name" : "java", "title" : "Linguagem Java"}];
    public authentication_list : any = [{"name" : "", "title" : "Sem autenticação"}, {"name" : "basic", "title" : "Protocolo HTTP Basic"}, {"name" : "oauth", "title" : "Protocolo Oauth 2.0"}];
    public type_list : any = [{name : "GET", title : "Obter (verbo HTTP GET)"},
							  {name : "POST", title : "Incluir (verbo HTTP POST)"},
							  {name : "PUT", title : "Alterar (verbo HTTP PUT)"},
							  {name : "DELETE", title : "Excluir (DELETE)"}
							 ];
    public model : Catalogo = new Catalogo();

    constructor(private http: Http, public modal: Modal, vcRef: ViewContainerRef) {
		modal.overlay.defaultViewContainer = vcRef;

		// busca os owners
        this.http.get(this.catalogoOwnerUrl)
			.catch(this.handleError)
            .subscribe((data)=> {
                setTimeout(()=> {
                    this.owner_list = data.json();
                }, 1000);
            });
    }

    ngOnInit(): void {
    }

    public toInt(num: string) {
        return +num;
    }

    public sortByWordLength = (a: any) => {
        return a.city.length;
    }
    
    public voltar(){
			this.operacao = this.ult_operacao;
			this.ult_operacao = "pesquisa";
	}
    
    public pesquisar(){
		this.ult_operacao = this.operacao;
		this.operacao = "listagem";
        this.http.get(this.catalogoUrl)
            .catch(this.handleError)
            .subscribe((data)=> {
                setTimeout(()=> {
                    this.data = data.json();
                }, 1000);
            });
	}
	
	public novo(){
		this.ult_operacao = this.operacao;
		this.operacao = "edicao";
		
	}

	public openDialogExemplos() {
		return this.modal.open(CustomModal,  overlayConfigFactory({  }, BSModalContext));
	}	

	private handleError(error: Response | any) {
		  let errMsg: string;
		  if (error instanceof Response) {
				const body = error.json() || '';
				fpc.mensagem(body);
				errMsg = body;
		  } else {
				errMsg = error.message ? error.message : error.toString();
		  }
		  console.error(errMsg);
		  return Observable.throw(errMsg);
	}	
	
	private extractData(res: Response) {
		  let body = res.json();
		  return body.data || { };
	}
	
	public salvar(){
		let headers = new Headers({ 'Content-Type': 'application/json' });
		let options = new RequestOptions({ headers: headers });
		this.model.name = this.model.url;
		this.model.public = true;
		this.http.post(this.catalogoUrl, this.model, options)
						.map(this.extractData)
						.catch(this.handleError)
						.subscribe(
							 cat  => this.data.push(cat),
							 error =>  this.errorMessage = <any>error
						);
						
	}
	
	
    
}

