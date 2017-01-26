import { Injectable } from '@angular/core';
import { Headers, RequestOptions, RequestOptionsArgs  } from '@angular/http';

@Injectable()
export class DefaultHeaders extends RequestOptions {

    constructor() {
      super();
    }

    merge(options?: RequestOptionsArgs): RequestOptions {
        let headers = new Headers({ 'content-type': 'application/json; charset=utf-8' });
        let usuario = JSON.parse(localStorage.getItem('currentUser'));
        if(usuario) {
          headers.append('authorization', 'Bearer ' + usuario.access_token)
        }
        options.headers = headers;
        var result = super.merge(options);
        result.merge = this.merge;
        return result;
      }
  
}
