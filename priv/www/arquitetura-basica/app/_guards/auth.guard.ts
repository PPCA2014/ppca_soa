import { Injectable } from '@angular/core';
import { Router, CanActivate } from '@angular/router';
import { Http } from '@angular/http';
import {AuthenticationService} from "../_services/authentication.service";

@Injectable()
export class AuthGuard implements CanActivate {

  constructor(private router: Router, private http: Http, private authenticationService: AuthenticationService) { }

  private menu: any;

  canActivate() {
    if (localStorage.getItem('currentUser')) {
      this.menu = sessionStorage.getItem('menu');
      let usuario = JSON.parse(localStorage.getItem('currentUser'));
      //chamar uma url que irá validar se o token esta válido 
      if (usuario.access_token){
        this.authenticationService.periodicIncrement(usuario.expires_in);
        return true;
      } else {
        this.router.navigate(['erro']);
        return false;
      }
    }else {
      this.router.navigate(['erro']);
      return false;
    }
  }
}
