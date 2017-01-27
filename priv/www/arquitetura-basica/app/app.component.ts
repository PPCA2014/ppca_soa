import { Component, HostListener, OnInit } from '@angular/core';
import {AuthenticationService} from "./_services/authentication.service";

@Component({
  selector: 'my-app',
  templateUrl: 'app/app.component.html'
})
export class AppComponent implements OnInit {

  constructor(private authenticationService: AuthenticationService){
  }

  ngOnInit() {
    if(localStorage.getItem('currentUser')) {
      let sessionTime = JSON.parse(localStorage.getItem('currentUser'));
      this.authenticationService.periodicIncrement(sessionTime.expires_in);
    }
  }

  @HostListener('window:beforeunload', ['$event'])
  beforeunloadHandler(e:any) {
    localStorage.removeItem('currentUser');
  }

}
