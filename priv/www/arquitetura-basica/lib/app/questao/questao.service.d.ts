import { Http } from '@angular/http';
import { Observable } from 'rxjs/Observable';
import { Router } from "@angular/router";
import { DefaultHeaders } from "../_headers/default.headers";
import { Questao } from "./questao.module";
export declare class QuestaoService extends DefaultHeaders {
    private http;
    private route;
    questao: Questao;
    constructor(http: Http, route: Router);
    insert(questao: Questao): Observable<Questao>;
    findByIdPessoa(idPessoa: number): Observable<any>;
}
