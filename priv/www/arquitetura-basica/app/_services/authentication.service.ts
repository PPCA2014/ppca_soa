import { Injectable } from '@angular/core';
import { Http , Headers, Response, RequestOptions } from '@angular/http';
import { Observable } from 'rxjs/Observable';
import { Router } from "@angular/router";
import 'rxjs/add/operator/map';
import {DefaultHeaders} from "../_headers/default.headers";


@Injectable()
export class AuthenticationService extends DefaultHeaders {

  public token: string;

  public headers: Headers;


  constructor(private http: Http, private route: Router, private options: RequestOptions) {
    super();
    var currentUser = JSON.parse(localStorage.getItem('currentUser'));
    this.token = currentUser && currentUser.token;
  }

  login(login: string, senha: string): Observable<boolean> {
    this.headers = new Headers();
    this.headers.append('Authorization', "Basic " + btoa(login + ":" + senha));
    this.options.headers.set('Authorization', "Basic " + btoa(login + ":" + senha));
    return this.http.post('http://localhost:8086/spring-oauth2-example/oauth/token?grant_type=password&username=admin&password=admin123', {headers:this.headers})
      .map((response: Response) => {
        let token = response.json() && response.json();
        if (token) {
          this.token = token;
          localStorage.setItem('currentUser', JSON.stringify(response.json()));
          return true;
        } else {
          return false;
        }
      });
  }

  getSitemap() {
    return this.http.get('menu.json')
      .map((res) => {
        var sitemap = res.json();
         sessionStorage.setItem('menu',JSON.stringify(sitemap));
        return sitemap;
      });
  }

  logout(): void {
    this.token = null;
    localStorage.removeItem('currentUser');
    this.route.navigate(['/']);
  }

}
