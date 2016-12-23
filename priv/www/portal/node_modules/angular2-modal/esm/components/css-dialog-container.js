var __extends = (this && this.__extends) || function (d, b) {
    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
    function __() { this.constructor = d; }
    d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
};
import { Component, ViewEncapsulation, ElementRef, Renderer } from '@angular/core';
import { BaseDynamicComponent } from './base-dynamic-component';
import { DialogRef } from '../models/dialog-ref';
/**
 * A component that acts as a top level container for an open modal window.
 */
export var CSSDialogContainer = (function (_super) {
    __extends(CSSDialogContainer, _super);
    function CSSDialogContainer(dialog, el, renderer) {
        _super.call(this, el, renderer);
        this.dialog = dialog;
        this.activateAnimationListener();
    }
    CSSDialogContainer.decorators = [
        { type: Component, args: [{
                    selector: 'css-dialog-container',
                    host: {
                        'tabindex': '-1',
                        'role': 'dialog'
                    },
                    encapsulation: ViewEncapsulation.None,
                    template: "<ng-content></ng-content>"
                },] },
    ];
    /** @nocollapse */
    CSSDialogContainer.ctorParameters = [
        { type: DialogRef, },
        { type: ElementRef, },
        { type: Renderer, },
    ];
    return CSSDialogContainer;
}(BaseDynamicComponent));
//# sourceMappingURL=css-dialog-container.js.map