var __extends = (this && this.__extends) || function (d, b) {
    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
    function __() { this.constructor = d; }
    d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
};
import { Injectable } from '@angular/core';
import { extend, arrayUnion } from './../framework/utils';
import { OverlayContext, OverlayContextBuilder } from './overlay-context';
export var DEFAULT_VALUES = {};
var DEFAULT_SETTERS = [
    'message'
];
export var ModalContext = (function (_super) {
    __extends(ModalContext, _super);
    function ModalContext() {
        _super.apply(this, arguments);
    }
    return ModalContext;
}(OverlayContext));
/**
 * A core context builder for a modal window instance, used to define the context upon
 * a modal choose it's behaviour.
 */
export var ModalContextBuilder = (function (_super) {
    __extends(ModalContextBuilder, _super);
    function ModalContextBuilder(defaultValues, initialSetters, baseType) {
        if (defaultValues === void 0) { defaultValues = undefined; }
        if (initialSetters === void 0) { initialSetters = undefined; }
        if (baseType === void 0) { baseType = undefined; }
        _super.call(this, extend(DEFAULT_VALUES, defaultValues || {}), arrayUnion(DEFAULT_SETTERS, initialSetters || []), baseType);
    }
    ModalContextBuilder.decorators = [
        { type: Injectable },
    ];
    /** @nocollapse */
    ModalContextBuilder.ctorParameters = [
        null,
        { type: Array, },
        null,
    ];
    return ModalContextBuilder;
}(OverlayContextBuilder));
//# sourceMappingURL=modal-context.js.map