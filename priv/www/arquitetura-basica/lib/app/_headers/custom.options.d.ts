import { BaseRequestOptions, RequestOptions, RequestOptionsArgs } from '@angular/http';
export declare class CustomRequestOptions extends BaseRequestOptions {
    merge(options?: RequestOptionsArgs): RequestOptions;
}
