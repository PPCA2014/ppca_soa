import { Component, OnInit } from '@angular/core';
import {FileService} from "./_file/file.service";

@Component({
  selector: 'my-app',
  templateUrl: 'app/app.component.html'
})
export class AppComponent implements OnInit  {

	constructor( private fileService: FileService) {
	}

	 ngOnInit() {

     this.fileService.startRedirect()
       .subscribe(resultado => {
         console.log('Resultado');
       });

	 }


}
