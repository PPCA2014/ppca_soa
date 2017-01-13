import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import {AuthenticationService} from "../_services/authentication.service";
import {NavigationComponent} from "../navigation/navigation.component";


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
    
  }

  login() {
    this.loading = true;
    this.authenticationService.getUrl(this.model.username, this.model.password).subscribe(
      result => {
        this.authenticationService.login()
          .subscribe(result => {
            if (result === true) {
              this.authenticationService.getSitemap().subscribe(resp=>{
                console.log('resposta do siteMap');
              });
              let sessionTime = JSON.parse(localStorage.getItem('currentUser'));
              this.authenticationService.periodicIncrement(sessionTime.expires_in);
              this.router.navigate(['/']);
            } else {
              this.error = 'Nome do usuário incorreto!';
              this.loading = false;
            }

          });
      });
   
  }

}
