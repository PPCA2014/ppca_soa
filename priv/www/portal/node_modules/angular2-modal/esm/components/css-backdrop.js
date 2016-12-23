var __extends = (this && this.__extends) || function (d, b) {
    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
    function __() { this.constructor = d; }
    d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
};
import { Component, ElementRef, ViewEncapsulation, Renderer } from '@angular/core';
import { BaseDynamicComponent } from './base-dynamic-component';
/**
 * Represents the modal backdrop shaped by CSS.
 */
export var CSSBackdrop = (function (_super) {
    __extends(CSSBackdrop, _super);
    function CSSBackdrop(el, renderer) {
        var _this = this;
        _super.call(this, el, renderer);
        this.activateAnimationListener();
        var style = {
            position: 'absolute',
            top: 0,
            left: 0,
            width: '100%',
            height: '100%'
        };
        Object.keys(style).forEach(function (k) { return _this.setStyle(k, style[k]); });
    }
    CSSBackdrop.decorators = [
        { type: Component, args: [{
                    selector: 'css-backdrop',
                    host: {
                        '[attr.class]': 'cssClass',
                        '[attr.style]': 'styleStr'
                    },
                    encapsulation: ViewEncapsulation.None,
                    template: ""
                },] },
    ];
    /** @nocollapse */
    CSSBackdrop.ctorParameters = [
        { type: ElementRef, },
        { type: Renderer, },
    ];
    return CSSBackdrop;
}(BaseDynamicComponent));
//# sourceMappingURL=css-backdrop.js.map