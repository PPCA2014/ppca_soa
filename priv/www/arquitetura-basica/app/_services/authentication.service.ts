import { Injectable } from '@angular/core';
import { Http , Headers, Response, RequestOptions } from '@angular/http';
import { Observable } from 'rxjs/Observable';
import { Router } from "@angular/router";
import 'rxjs/add/operator/map';



@Injectable()
export class AuthenticationService  {

  public token: string;

  public time: number = 0;
  intervalId: any = null;

  private url: string;
  private body: string;



  constructor(private http: Http, private route: Router, private options: RequestOptions) {
    var currentUser = JSON.parse(localStorage.getItem('currentUser'));
    this.token = currentUser && currentUser.token;
  }

  login(): Observable<boolean> {
    return this.http.post(this.url,this.body)
      .map((response: Response) => {
        let token = response.json() && response.json();
        if (token) {
          this.token = token;
          localStorage.setItem('currentUser', JSON.stringify(response.json()));
          let sessionTime = JSON.parse(localStorage.getItem('currentUser'));
          this.periodicIncrement(sessionTime.expires_in);
          return true;
        } else {
          return false;
        }
      });
  }

  getUrl(login:string, senha: string) {
    return this.http.get('/arquitetura-basica/url_security.json')
      .map((res) => {
        var json = res.json();
        this.url = json.url+''+json.param1+''+login+''+json.param2+''+senha;
        this.body = json.body;
        return this.url;
      });
  }

  periodicIncrement(sessionTime:number): void {
    this.cancelPeriodicIncrement();
    this.time = sessionTime * 1000;
    this.intervalId = setInterval(() => {
      if(this.time == 0){
        this.logout();
        return 0;
      }
      this.time = this.time - 1000;
      return this.time;
    }, 1000);

  };

  cancelPeriodicIncrement(): void {
    if (this.intervalId != null) {
      clearInterval(this.intervalId);
      this.intervalId = null;
      this.time = 0;
    }
  };


  getSitemap() {
    return this.http.get('/arquitetura-basica/menu.json')
      .map((res) => {
        var sitemap = res.json();
         sessionStorage.setItem('menu',JSON.stringify(sitemap));
        return sitemap;
      });
  }

  logout(): void {
    this.cancelPeriodicIncrement();
    this.token = null;
    localStorage.removeItem('currentUser');
    this.route.navigate(['']);
  }

}
