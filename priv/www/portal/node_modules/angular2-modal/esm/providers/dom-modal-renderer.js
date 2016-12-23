import { Injectable, ReflectiveInjector } from '@angular/core';
import { createComponent } from '../framework/createComponent';
import { DialogRef } from '../models/dialog-ref';
import { ModalOverlay } from '../overlay/index';
export var DOMOverlayRenderer = (function () {
    function DOMOverlayRenderer() {
    }
    DOMOverlayRenderer.prototype.render = function (dialog, vcRef, injector) {
        var bindings = ReflectiveInjector.resolve([
            { provide: DialogRef, useValue: dialog }
        ]);
        var cmpRef = createComponent({
            component: ModalOverlay,
            vcRef: vcRef,
            injector: injector,
            bindings: bindings
        });
        if (dialog.inElement) {
            vcRef.element.nativeElement.appendChild(cmpRef.location.nativeElement);
        }
        else {
            document.body.appendChild(cmpRef.location.nativeElement);
        }
        return cmpRef;
    };
    DOMOverlayRenderer.decorators = [
        { type: Injectable },
    ];
    /** @nocollapse */
    DOMOverlayRenderer.ctorParameters = [];
    return DOMOverlayRenderer;
}());
//# sourceMappingURL=dom-modal-renderer.js.map