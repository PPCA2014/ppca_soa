import { Injectable } from '@angular/core';
import { Headers, RequestOptions, RequestOptionsArgs  } from '@angular/http';

@Injectable()
export class DefaultHeaders extends RequestOptions {

    constructor() {
      super();
    }

    merge(options?: RequestOptionsArgs): RequestOptions {
        let headers = new Headers({ 'content-type': 'application/x-www-form-urlencoded; charset=UTF-8' });
        let usuario = JSON.parse(localStorage.getItem('currentUser'));
        if(usuario) {
          //headers.append('authorization', 'Bearer ' + usuario.access_token)
         // headers.append('X-CSRFToken', this.getCookie('csrftoken'));
        }
        options.headers = headers;
        var result = super.merge(options);
        result.merge = this.merge;
        return result;
      }

    getCookie(name:any) {
      let value = "; " + document.cookie;
      let parts = value.split("; " + name + "=");
      if (parts.length == 2){
        return parts.pop().split(";").shift();
      }
    }
}
