import { Component, OnInit } from '@angular/core';
import {AuthenticationService} from "../_services/authentication.service";

@Component({
  selector: 'app-navigation',
  templateUrl: 'app/navigation/navigation.component.html',
  styleUrls: ['app/navigation/navigation.component.css']
})
export class NavigationComponent implements OnInit {

  private usuario: any;

  constructor(private authenticationService: AuthenticationService) { }

  ngOnInit() {

  }

  logout(){
    this.authenticationService.logout();
  }

  verificarUsuarioLogado(){
    if (localStorage.getItem('currentUser')){
      this.usuario = JSON.parse(localStorage.getItem('currentUser'));
      return true;
    }else {
      return false;
    }
  }


}
