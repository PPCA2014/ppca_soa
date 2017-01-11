import { Injectable } from '@angular/core';
import { Headers, RequestOptions, RequestOptionsArgs  } from '@angular/http';

@Injectable()
export class DefaultHeaders extends RequestOptions {

    constructor() {
      super();
    }

    merge(options?: RequestOptionsArgs): RequestOptions {
        let headers = new Headers({ 'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8' });
        options.headers = headers;
        var result = super.merge(options);
        result.merge = this.merge;
        return result;
      }
}
