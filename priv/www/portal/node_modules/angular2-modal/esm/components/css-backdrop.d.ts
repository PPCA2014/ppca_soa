import { ElementRef, Renderer } from '@angular/core';
import { BaseDynamicComponent } from './base-dynamic-component';
/**
 * Represents the modal backdrop shaped by CSS.
 */
export declare class CSSBackdrop extends BaseDynamicComponent {
    cssClass: string;
    styleStr: string;
    constructor(el: ElementRef, renderer: Renderer);
}
