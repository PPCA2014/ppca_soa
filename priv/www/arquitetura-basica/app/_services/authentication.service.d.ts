import { Http, Headers, RequestOptions } from '@angular/http';
import { Observable } from 'rxjs/Observable';
import { Router } from "@angular/router";
import 'rxjs/add/operator/map';
import { DefaultHeaders } from "../_headers/default.headers";
export declare class AuthenticationService extends DefaultHeaders {
    private http;
    private route;
    private options;
    token: string;
    headers: Headers;
    time: number;
    intervalId: any;
    constructor(http: Http, route: Router, options: RequestOptions);
    login(login: string, senha: string): Observable<boolean>;
    periodicIncrement(sessionTime: number): void;
    cancelPeriodicIncrement(): void;
    getSitemap(): Observable<any>;
    logout(): void;
}
