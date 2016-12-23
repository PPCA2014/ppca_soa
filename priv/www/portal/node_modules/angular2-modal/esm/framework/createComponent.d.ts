import { ComponentRef, Injector, ViewContainerRef, ResolvedReflectiveProvider } from '@angular/core';
export interface CreateComponentArgs {
    component: any;
    vcRef: ViewContainerRef;
    injector?: Injector;
    bindings?: ResolvedReflectiveProvider[];
    projectableNodes?: any[][];
}
export declare function createComponent(instructions: CreateComponentArgs): ComponentRef<any>;
