import { ModuleWithProviders, Type } from '@angular/core';
export declare class ModalModule {
    /**
     * Returns a ModalModule pre-loaded with a list of dynamically inserted components.
     * Since dynamic components are not analysed by the angular compiler they must register manually
     * using entryComponents, this is an easy way to do it.
     * @param entryComponents A list of dynamically inserted components (dialog's).
     * @returns {{ngModule: ModalModule, providers: {provide: OpaqueToken, useValue: Array<Type|any[]>, multi: boolean}[]}}
     */
    static withComponents(entryComponents: Array<Type<any> | any[]>): ModuleWithProviders;
    /**
     * Returns a NgModule for use in the root Module.
     * @param entryComponents A list of dynamically inserted components (dialog's).
     * @returns ModuleWithProviders
     */
    static forRoot(entryComponents?: Array<Type<any> | any[]>): ModuleWithProviders;
}
