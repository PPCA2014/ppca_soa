import { ElementRef, Renderer } from '@angular/core';
import { BaseDynamicComponent, DialogRef } from 'angular2-modal';
import { MessageModalPreset } from './presets/message-modal-preset';
export declare class BSModalContainer extends BaseDynamicComponent {
    dialog: DialogRef<MessageModalPreset>;
    constructor(dialog: DialogRef<MessageModalPreset>, el: ElementRef, renderer: Renderer);
}
