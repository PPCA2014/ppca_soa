import { Injectable } from '@angular/core';
import { Router, CanActivate } from '@angular/router';
import { Http } from '@angular/http';
import { Observable } from 'rxjs/Rx';
import {AuthenticationService} from "../_services/authentication.service";

@Injectable()
export class AuthGuard implements CanActivate {

  constructor(private router: Router, private http: Http, private authenticationService: AuthenticationService) { }

  private menu: any;

  canActivate() {
    if (localStorage.getItem('currentUser')) {
      this.menu = sessionStorage.getItem('menu');
      //mudar para verificar a presença do token
      let usuario = JSON.parse(localStorage.getItem('currentUser'));
      if (usuario){
        let time = Observable.timer(300000);
        time.subscribe(resultado=> {
          this.authenticationService.logout();
        });

        return true;
      } else {
        this.router.navigate(['erro']);
        return false;
      }
    }
  }
}
