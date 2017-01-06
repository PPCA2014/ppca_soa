import { OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { AuthenticationService } from "../_services/authentication.service";
export declare class LoginComponent implements OnInit {
    private router;
    private authenticationService;
    private model;
    private loading;
    private error;
    constructor(router: Router, authenticationService: AuthenticationService);
    ngOnInit(): void;
    login(): void;
}
