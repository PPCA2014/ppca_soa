import { ViewContainerRef, ComponentRef } from '@angular/core';
import { DialogRef, OverlayRenderer, ModalOverlay } from 'angular2-modal';
export declare class JSNativeModalRenderer implements OverlayRenderer {
    render(dialog: DialogRef<any>, vcRef: ViewContainerRef): ComponentRef<ModalOverlay>;
}
