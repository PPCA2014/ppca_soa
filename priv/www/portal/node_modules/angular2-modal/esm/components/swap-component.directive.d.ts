import { ComponentRef, ResolvedReflectiveProvider, ViewContainerRef, ComponentFactoryResolver, EventEmitter, TemplateRef, Injector } from '@angular/core';
export declare class SwapComponentDirective {
    private cfr;
    private vcRef;
    private tRef;
    private component;
    constructor(cfr: ComponentFactoryResolver, vcRef: ViewContainerRef, tRef: TemplateRef<Object>);
    swapCmpBindings: ResolvedReflectiveProvider[];
    swapCmpInjector: Injector;
    swapCmpProjectables: any[][];
    onCreate: EventEmitter<ComponentRef<any>>;
    swapCmp: any;
}
