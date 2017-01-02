import { Component } from '@angular/core';
import { Observable } from 'rxjs/Rx';

@Component({
  selector: 'my-app',
  templateUrl: 'app/app.component.html'
})
export class AppComponent  {

  getTimer(){
    var source = Observable.timer(200, 100)
      .timeInterval()
      .pluck('interval')
      .take(3);

    var subscription = source.subscribe(
      function (x) {
        console.log('Next: ' + x);
      },
      function (err) {
        console.log('Error: ' + err);
      },
      function () {
        console.log('Completed');
      });
  }

}
