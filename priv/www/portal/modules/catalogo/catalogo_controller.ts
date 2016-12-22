import {Component} from '@angular/core';
import { Http, Response, Headers, RequestOptions } from '@angular/http';
import { Catalogo } from './catalogo';

 
@Component({
    selector: 'catalogo',
    templateUrl: 'modules/catalogo/catalogo.html'
})
 
export class CatalogoController { 
	
	public operacao : string = "pesquisa";
	public ult_operacao : string = "pesquisa";
	public owner : string = "";
	public data : any;
    public filterQuery : string = "";
    public rowsOnPage : number = 10;
    public sortBy : string = "email";
    public sortOrder : string = "asc";
    public lista_owners : any = null;
    public model : Catalogo = new Catalogo();

    constructor(private http: Http) {
		// busca os owners
        this.http.get("/catalog/owner")
            .subscribe((data)=> {
                setTimeout(()=> {
                    this.lista_owners = data.json();
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
        this.http.get("/catalog")
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
    
}

