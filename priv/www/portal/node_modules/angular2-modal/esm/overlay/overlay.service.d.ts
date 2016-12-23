import { ViewContainerRef } from '@angular/core';
import { OverlayRenderer, OverlayConfig } from '../models/tokens';
import { DialogRef } from '../models/dialog-ref';
import { OverlayContext } from '../models/overlay-context';
export declare class Overlay {
    private _modalRenderer;
    /**
     * A Default view container ref, usually the app root container ref.
     * Make sure not to provide something that might get destroyed, it will destroy the modals too.
     * The container is used as logical view holder, elements might be moved.
     * Has to be set manually until we can find a way to get it automatically.
     */
    defaultViewContainer: ViewContainerRef;
    readonly stackLength: number;
    constructor(_modalRenderer: OverlayRenderer);
    /**
     * Check if a given DialogRef is the top most ref in the stack.
     * TODO: distinguish between body modal vs in element modal.
     * @param dialogRef
     * @returns {boolean}
     */
    isTopMost(dialogRef: DialogRef<any>): boolean;
    stackPosition(dialogRef: DialogRef<any>): number;
    groupStackLength(dialogRef: DialogRef<any>): number;
    /**
     * Creates an overlay and returns a dialog ref.
     * @param config instructions how to create the overlay
     * @param group A token to associate the new overlay with, used for reference (stacks usually)
     * @returns {DialogRef<T>[]}
     */
    open<T extends OverlayContext>(config: OverlayConfig, group?: any): DialogRef<T>[];
    private createOverlay(renderer, vcRef, config, group);
}
