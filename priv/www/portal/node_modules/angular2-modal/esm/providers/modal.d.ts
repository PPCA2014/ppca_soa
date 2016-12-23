import { ComponentRef, TemplateRef, ResolvedReflectiveProvider, Type } from '@angular/core';
import { Overlay } from '../overlay/index';
import { Class, Maybe } from '../framework/utils';
import { OverlayConfig, ContainerContent } from '../models/tokens';
import { DialogRef } from '../models/dialog-ref';
import { ModalControllingContextBuilder } from '../models/overlay-context';
export declare class UnsupportedDropInError extends Error {
    constructor(dropInName: string);
}
export declare abstract class Modal {
    overlay: Overlay;
    constructor(overlay: Overlay);
    alert(): ModalControllingContextBuilder<any>;
    prompt(): ModalControllingContextBuilder<any>;
    confirm(): ModalControllingContextBuilder<any>;
    /**
     * Opens a modal window inside an existing component.
     * @param content The content to display, either string, template ref or a component.
     * @param config Additional settings.
     * @returns {Promise<DialogRef>}
     */
    open(content: ContainerContent, config?: OverlayConfig): Promise<DialogRef<any>>;
    /**
     * A Hook that enables derived classes to add content to the overlay.
     * @param dialogRef
     * @param type
     * @param bindings
     * @returns {MaybeDialogRef<any>}
     */
    protected abstract create(dialogRef: DialogRef<any>, type: ContainerContent, bindings?: ResolvedReflectiveProvider[]): Maybe<DialogRef<any>>;
    protected createBackdrop<T>(dialogRef: DialogRef<any>, BackdropComponent: Class<T>): ComponentRef<T>;
    protected createContainer<T>(dialogRef: DialogRef<any>, ContainerComponent: Class<T>, content: string | TemplateRef<any> | Type<any>, bindings?: ResolvedReflectiveProvider[]): ComponentRef<T>;
    /**
     * A helper function for derived classes to create backdrop & container
     * @param dialogRef
     * @param backdrop
     * @param container
     * @returns { backdropRef: ComponentRef<B>, containerRef: ComponentRef<C> }
     *
     * @deprecated use createBackdrop and createContainer instead
     */
    protected createModal<B, C>(dialogRef: DialogRef<any>, backdrop: Class<B>, container: Class<C>): {
        backdropRef: ComponentRef<B>;
        containerRef: ComponentRef<C>;
    };
}
