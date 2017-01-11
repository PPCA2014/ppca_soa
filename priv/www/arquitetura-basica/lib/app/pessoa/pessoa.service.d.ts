import { Pessoa } from "./pessoa.module";
import { Http } from '@angular/http';
import { Observable } from 'rxjs/Observable';
import { Router } from "@angular/router";
export declare class PessoaService {
    private http;
    private route;
    pessoa: Pessoa;
    constructor(http: Http, route: Router);
    insert(pessoa: Pessoa): Observable<Pessoa>;
    find(): Observable<any>;
    redirecionarEditar(pessoa: Pessoa): void;
    update(pessoa: Pessoa): Observable<Pessoa>;
    delete(pessoa: Pessoa): Observable<any>;
}
