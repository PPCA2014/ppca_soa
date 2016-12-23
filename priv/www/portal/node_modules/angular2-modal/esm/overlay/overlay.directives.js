import { Directive, Input, ElementRef, ViewContainerRef } from '@angular/core';
import { DialogRef } from '../models/dialog-ref';
import { vcRefStore } from '../models/vc-ref-store';
import { Overlay } from "./overlay.service";
/**
 * A directive use to signal the overlay that the host of this directive
 * is a dialog boundary, i.e: over click outside of the element should close the modal
 * (if non blocking)
 */
export var OverlayDialogBoundary = (function () {
    function OverlayDialogBoundary(el, dialogRef) {
        if (dialogRef && el.nativeElement) {
            dialogRef.overlayRef.instance.setClickBoundary(el.nativeElement);
        }
    }
    OverlayDialogBoundary.decorators = [
        { type: Directive, args: [{
                    selector: '[overlayDialogBoundary]'
                },] },
    ];
    /** @nocollapse */
    OverlayDialogBoundary.ctorParameters = [
        { type: ElementRef, },
        { type: DialogRef, },
    ];
    return OverlayDialogBoundary;
}());
export var OverlayTarget = (function () {
    function OverlayTarget(vcRef) {
        this.vcRef = vcRef;
    }
    Object.defineProperty(OverlayTarget.prototype, "targetKey", {
        set: function (value) {
            this._targetKey = value;
            if (value) {
                vcRefStore.setVCRef(value, this.vcRef);
            }
        },
        enumerable: true,
        configurable: true
    });
    OverlayTarget.prototype.ngOnDestroy = function () {
        if (this._targetKey) {
            vcRefStore.delVCRef(this._targetKey, this.vcRef);
        }
    };
    OverlayTarget.decorators = [
        { type: Directive, args: [{
                    selector: '[overlayTarget]'
                },] },
    ];
    /** @nocollapse */
    OverlayTarget.ctorParameters = [
        { type: ViewContainerRef, },
    ];
    OverlayTarget.propDecorators = {
        'targetKey': [{ type: Input, args: ['overlayTarget',] },],
    };
    return OverlayTarget;
}());
var noop = function () { };
export var DefaultOverlayTarget = (function () {
    function DefaultOverlayTarget(overlay, vcRef) {
        this.overlay = overlay;
        overlay.defaultViewContainer = vcRef;
    }
    DefaultOverlayTarget.prototype.ngOnDestroy = function () {
        this.overlay.defaultViewContainer = undefined;
    };
    DefaultOverlayTarget.decorators = [
        { type: Directive, args: [{
                    selector: '[defaultOverlayTarget]'
                },] },
    ];
    /** @nocollapse */
    DefaultOverlayTarget.ctorParameters = [
        { type: Overlay, },
        { type: ViewContainerRef, },
    ];
    return DefaultOverlayTarget;
}());
//# sourceMappingURL=overlay.directives.js.map