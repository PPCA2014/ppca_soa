import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import {AuthenticationService} from "../_services/authentication.service";


@Component({
  selector: 'app-login',
  templateUrl: 'app/login/login.component.html',
  styleUrls: ['app/login/login.component.css']
})
export class LoginComponent implements OnInit {

  private model: any = {};
  private loading = false;
  private error = '';


  constructor(
    private router: Router,
    private authenticationService: AuthenticationService) { }

  ngOnInit() {
    this.authenticationService.logout();
  }

  login() {
    this.loading = true;
    this.authenticationService.login(this.model.username, this.model.password)
      .subscribe(result => {
        if (result === true) {
          this.authenticationService.getSitemap().subscribe(resp=>{
            console.log('resposta do siteMap');
          });
          this.router.navigate(['/']);
        } else {
          this.error = 'Nome do usuário incorreto!';
          this.loading = false;
        }

      });
  }

}
