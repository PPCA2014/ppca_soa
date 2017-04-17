import { Component, OnInit } from '@angular/core';
import {AuthenticationService, RedirectService} from 'seguranca';

@Component({
  selector: 'my-app',
  templateUrl: 'app/app.component.html'
})
export class AppComponent implements OnInit  {

	constructor(private redirectService: RedirectService) {
	}

	 ngOnInit() {
		  localStorage.setItem('externalFile',('http://localhost:2301/sitab/config.json'));
      this.redirectService.initVerificationRedirect();
	 }


}
