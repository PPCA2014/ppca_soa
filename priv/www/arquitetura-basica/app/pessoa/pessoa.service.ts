import { Injectable } from '@angular/core';
import {Pessoa} from "./pessoa.module";
import { Http, Headers, Response } from '@angular/http';
import { Observable } from 'rxjs/Observable'
import { Router } from "@angular/router";
import {DefaultHeaders} from "../_headers/default.headers";

@Injectable()
export class PessoaService extends DefaultHeaders{

  public pessoa: Pessoa;

  constructor(private http: Http, private route: Router) {
    super();
  }

  insert(pessoa: Pessoa): Observable<Pessoa> {
    return this.http.post('http://localhost:2301/unb_aula/pessoa', pessoa, {headers:this.headers})
      .map((response: Response) => {
         console.log(response.json());
        return new Pessoa().fromJSON(response.json());
      });
  }

  find() {
    return this.http.get('http://localhost:2301/unb_aula/pessoa',{headers:this.headers})
      .map((response: Response) => response.json())
  }

  redirecionarEditar(pessoa: Pessoa){
      this.pessoa = pessoa;
      this.route.navigate(['/pessoa']);
  }

  update(pessoa: Pessoa): Observable<Pessoa> {
    return this.http.put('http://localhost:2301/unb_aula/pessoa/'+pessoa.id , pessoa,{headers:this.headers})
      .map((response: Response) => {
        console.log(response.json());
        return new Pessoa().fromJSON(response.json());
      });
  }

  delete(pessoa: Pessoa) {
    return this.http.delete('http://localhost:2301/unb_aula/pessoa/'+pessoa.id,{headers:this.headers})
      .map((response: Response) => response.json())

  }

}
