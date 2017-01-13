import { Http, RequestOptions } from '@angular/http';
import { Observable } from 'rxjs/Observable';
import { Router } from "@angular/router";
import 'rxjs/add/operator/map';
export declare class AuthenticationService {
    private http;
    private route;
    private options;
    token: string;
    time: number;
    intervalId: any;
    private url;
    private body;
    constructor(http: Http, route: Router, options: RequestOptions);
    login(login: string, senha: string): Observable<boolean>;
    getUrl(login: string, senha: string): Observable<string>;
    periodicIncrement(sessionTime: number): void;
    cancelPeriodicIncrement(): void;
    getSitemap(): Observable<any>;
    logout(): void;
}
