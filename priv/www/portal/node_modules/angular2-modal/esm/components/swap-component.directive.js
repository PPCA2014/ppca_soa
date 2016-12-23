import { Directive, Input, Output, ReflectiveInjector, ViewContainerRef, ComponentFactoryResolver, EventEmitter, TemplateRef } from '@angular/core';
// <template [dynCmp]="myCmp" [dynCmpBindings]="myBindings"></template>
// <template [dynCmp]="ctx.component" [dynCmpBindings]="ctx.bindings" [dynCmpProjectables]="ctx.projectableNodes"></template>
export var SwapComponentDirective = (function () {
    function SwapComponentDirective(cfr, vcRef, tRef) {
        this.cfr = cfr;
        this.vcRef = vcRef;
        this.tRef = tRef;
        this.onCreate = new EventEmitter(false);
    }
    Object.defineProperty(SwapComponentDirective.prototype, "swapCmp", {
        set: function (component) {
            this.component = component;
            this.vcRef.clear();
            if (this.component) {
                var injector = this.swapCmpInjector || this.vcRef.parentInjector;
                if (Array.isArray(this.swapCmpBindings) && this.swapCmpBindings.length > 0) {
                    injector = ReflectiveInjector.fromResolvedProviders(this.swapCmpBindings, injector);
                }
                var cmpRef = this.vcRef.createComponent(this.cfr.resolveComponentFactory(component), this.vcRef.length, injector, this.swapCmpProjectables);
                cmpRef.changeDetectorRef.detectChanges();
                this.onCreate.emit(cmpRef);
            }
        },
        enumerable: true,
        configurable: true
    });
    SwapComponentDirective.decorators = [
        { type: Directive, args: [{
                    selector: '[swapCmp]'
                },] },
    ];
    /** @nocollapse */
    SwapComponentDirective.ctorParameters = [
        { type: ComponentFactoryResolver, },
        { type: ViewContainerRef, },
        { type: TemplateRef, },
    ];
    SwapComponentDirective.propDecorators = {
        'swapCmpBindings': [{ type: Input },],
        'swapCmpInjector': [{ type: Input },],
        'swapCmpProjectables': [{ type: Input },],
        'onCreate': [{ type: Output },],
        'swapCmp': [{ type: Input },],
    };
    return SwapComponentDirective;
}());
//# sourceMappingURL=swap-component.directive.js.map