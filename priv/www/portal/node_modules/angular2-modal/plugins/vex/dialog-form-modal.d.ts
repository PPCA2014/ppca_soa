import { EventEmitter } from '@angular/core';
import { DialogRef, ModalComponent } from 'angular2-modal';
import { DialogPreset } from './presets/dialog-preset';
import { DropInPreset } from './presets/dropin-preset';
export interface VEXButtonHandler {
    (cmp: ModalComponent<DialogPreset>, $event: MouseEvent): void;
}
/**
 * Interface for button definition
 */
export interface VEXButtonConfig {
    cssClass: string;
    caption: string;
    onClick: VEXButtonHandler;
}
export interface VEXButtonClickEvent {
    btn: VEXButtonConfig;
    $event: MouseEvent;
}
/**
 * A Dialog is a
 */
export declare class VEXDialogButtons {
    /**
     * A collection of button configurations, each configuration is a button to display.
     */
    buttons: VEXButtonConfig[];
    /**
     * Emitted when a button was clicked
     * @type {EventEmitter<VEXButtonClickEvent>}
     */
    onButtonClick: EventEmitter<VEXButtonClickEvent>;
    onClick(btn: any, $event: MouseEvent): void;
}
/**
 * A Dialog with customized buttons wrapped in a form.
 *
 */
export declare class DialogFormModal implements ModalComponent<DialogPreset> {
    dialog: DialogRef<DialogPreset>;
    context: DialogPreset;
    constructor(dialog: DialogRef<DialogPreset>);
    onButtonClick($event: VEXButtonClickEvent): void;
}
export declare class FormDropIn implements ModalComponent<DropInPreset> {
    dialog: DialogRef<DropInPreset>;
    context: DropInPreset;
    constructor(dialog: DialogRef<DropInPreset>);
}
