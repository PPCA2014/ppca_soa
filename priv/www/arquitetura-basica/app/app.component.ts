import { Component, HostListener } from '@angular/core';

@Component({
  selector: 'my-app',
  templateUrl: 'app/app.component.html'
})
export class AppComponent  {

  @HostListener('window:unload', ['$event'])
  beforeunloadHandler(event:any) {
    localStorage.removeItem('currentUser');
  }

}
