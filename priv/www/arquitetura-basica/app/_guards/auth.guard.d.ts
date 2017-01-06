import { Router, CanActivate } from '@angular/router';
import { Http } from '@angular/http';
import { AuthenticationService } from "../_services/authentication.service";
export declare class AuthGuard implements CanActivate {
    private router;
    private http;
    private authenticationService;
    constructor(router: Router, http: Http, authenticationService: AuthenticationService);
    private menu;
    canActivate(): boolean;
}
