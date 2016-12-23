import { ElementRef, Renderer } from '@angular/core';
import { BaseDynamicComponent } from './base-dynamic-component';
import { DialogRef } from '../models/dialog-ref';
/**
 * A component that acts as a top level container for an open modal window.
 */
export declare class CSSDialogContainer extends BaseDynamicComponent {
    dialog: DialogRef<any>;
    constructor(dialog: DialogRef<any>, el: ElementRef, renderer: Renderer);
}
