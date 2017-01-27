import { OnInit } from '@angular/core';
import { AuthenticationService } from "./_services/authentication.service";
export declare class AppComponent implements OnInit {
    private authenticationService;
    constructor(authenticationService: AuthenticationService);
    ngOnInit(): void;
    beforeunloadHandler(e: any): void;
}
