import { ModalOpenContext, ModalOpenContextBuilder, FluentAssignMethod } from 'angular2-modal';
export declare type BootstrapModalSize = 'sm' | 'lg';
export declare class BSModalContext extends ModalOpenContext {
    /**
     * A Class for the modal dialog container.
     * Default: modal-dialog
     */
    dialogClass: string;
    /**
     * Size of the modal. 'lg' or 'sm' only.
     *
     * If you want to use custom sizes leave this empty and set the dialogClass property.
     * e.g: dialogClass = 'modal-dialog my-custom-dialog`
     * NOTE: No validation.
     * Default: ''
     */
    size: BootstrapModalSize;
    /**
     * When true, show a close button on the top right corner.
     */
    showClose: boolean;
    normalize(): void;
}
export declare class BSModalContextBuilder<T extends BSModalContext> extends ModalOpenContextBuilder<T> {
    /**
     * A Class for the modal dialog container.
     * Default: modal-dialog
     */
    dialogClass: FluentAssignMethod<string, this>;
    /**
     * Size of the modal. 'lg' or 'sm' only.
     *
     * If you want to use custom sizes leave this empty and set the dialogClass property.
     * e.g: dialogClass = 'modal-dialog my-custom-dialog`
     * NOTE: No validation.
     * Default: ''
     */
    size: FluentAssignMethod<BootstrapModalSize, this>;
    /**
     * When true, show a close button on the top right corner.
     */
    showClose: FluentAssignMethod<boolean, this>;
    constructor(defaultValues?: T, initialSetters?: string[], baseType?: any);
}
