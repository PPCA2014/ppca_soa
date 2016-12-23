import { ViewContainerRef, ComponentRef, Injector } from '@angular/core';
import { DialogRef } from '../models/dialog-ref';
import { OverlayRenderer } from '../models/tokens';
import { ModalOverlay } from '../overlay/index';
export declare class DOMOverlayRenderer implements OverlayRenderer {
    render(dialog: DialogRef<any>, vcRef: ViewContainerRef, injector?: Injector): ComponentRef<ModalOverlay>;
}
