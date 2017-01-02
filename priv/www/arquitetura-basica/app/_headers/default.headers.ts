import { Injectable } from '@angular/core';
import { Headers, BaseRequestOptions} from '@angular/http';

@Injectable()
export class DefaultHeaders extends BaseRequestOptions{
   public headers:Headers = new Headers({
    'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8'
  });

  constructor(){
    super();
  }
}
