var __extends = (this && this.__extends) || function (d, b) {
    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
    function __() { this.constructor = d; }
    d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
};
import { ModalOpenContext, ModalOpenContextBuilder, privateKey, extend, arrayUnion } from 'angular2-modal';
var DEFAULT_VALUES = {
    className: 'default',
    overlayClassName: 'vex-overlay',
    contentClassName: 'vex-content',
    closeClassName: 'vex-close'
};
var DEFAULT_SETTERS = [
    'className',
    'overlayClassName',
    'contentClassName',
    'closeClassName',
    'showCloseButton'
];
export var VEXModalContext = (function (_super) {
    __extends(VEXModalContext, _super);
    function VEXModalContext() {
        _super.apply(this, arguments);
    }
    VEXModalContext.prototype.normalize = function () {
        if (!this.className) {
            this.className = DEFAULT_VALUES.className;
        }
        if (!this.overlayClassName) {
            this.overlayClassName = DEFAULT_VALUES.overlayClassName;
        }
        if (!this.contentClassName) {
            this.contentClassName = DEFAULT_VALUES.contentClassName;
        }
        if (!this.closeClassName) {
            this.closeClassName = DEFAULT_VALUES.closeClassName;
        }
        _super.prototype.normalize.call(this);
    };
    return VEXModalContext;
}(ModalOpenContext));
export var VEXModalContextBuilder = (function (_super) {
    __extends(VEXModalContextBuilder, _super);
    function VEXModalContextBuilder(defaultValues, initialSetters, baseType) {
        if (defaultValues === void 0) { defaultValues = undefined; }
        if (initialSetters === void 0) { initialSetters = undefined; }
        if (baseType === void 0) { baseType = undefined; }
        _super.call(this, extend(DEFAULT_VALUES, defaultValues || {}), arrayUnion(DEFAULT_SETTERS, initialSetters || []), baseType || VEXModalContext // https://github.com/Microsoft/TypeScript/issues/7234
        );
    }
    /**
     *
     * @aliasFor isBlocking
     */
    VEXModalContextBuilder.prototype.overlayClosesOnClick = function (value) {
        this[privateKey('isBlocking')] = !value;
        return this;
    };
    return VEXModalContextBuilder;
}(ModalOpenContextBuilder));
//# sourceMappingURL=modal-context.js.map