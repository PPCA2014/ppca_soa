import { Component, OnInit } from '@angular/core';
import { AuthenticationService } from 'seguranca';

@Component({
  selector: 'my-app',
  templateUrl: 'app/app.component.html'
})
export class AppComponent implements OnInit  { 

	constructor() {	
	}
	
	 ngOnInit() {
		  localStorage.setItem('externalFile',('http://localhost:2301/sitab/config.json'));
	 }
	
 
}
