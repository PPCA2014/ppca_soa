import { Http } from '@angular/http';
import { Observable } from 'rxjs/Observable';
import 'rxjs/add/operator/map';
import { AuthenticationService } from "./authentication.service";
import { User } from "../_models/user";
export declare class UserService {
    private http;
    private authenticationService;
    constructor(http: Http, authenticationService: AuthenticationService);
    getUsers(): Observable<User[]>;
}
