import { ResolvedReflectiveProvider } from '@angular/core';
import { Modal, FluentAssignMethod } from 'angular2-modal';
import { MessageModalPresetBuilder } from './message-modal-preset';
import { OneButtonPreset } from './one-button-preset';
export interface TwoButtonPreset extends OneButtonPreset {
    /**
     * Caption for the Cancel button.
     * Default: Cancel
     */
    cancelBtn: string;
    /**
     * A Class for the Cancel button.
     * Default: btn btn-default
     */
    cancelBtnClass: string;
}
/** Common two button preset */
export declare abstract class AbstractTwoButtonPresetBuilder extends MessageModalPresetBuilder<TwoButtonPreset> {
    okBtn: FluentAssignMethod<string, this>;
    okBtnClass: FluentAssignMethod<string, this>;
    cancelBtn: FluentAssignMethod<string, this>;
    cancelBtnClass: FluentAssignMethod<string, this>;
    constructor(modal: Modal, defaultValues?: TwoButtonPreset, initialSetters?: string[]);
    $$beforeOpen(config: TwoButtonPreset): ResolvedReflectiveProvider[];
}
/**
 * A Preset for a classic 2 button modal window.
 */
export declare class TwoButtonPresetBuilder extends AbstractTwoButtonPresetBuilder {
    constructor(modal: Modal, defaultValues?: TwoButtonPreset);
    $$beforeOpen(config: TwoButtonPreset): ResolvedReflectiveProvider[];
}
export interface PromptPreset extends TwoButtonPreset {
    showInput: boolean;
    /** Default value shown in the prompt. */
    defaultValue: string;
    /** A placeholder for the input element. */
    placeholder: string;
}
export declare class PromptPresetBuilder extends AbstractTwoButtonPresetBuilder {
    placeholder: FluentAssignMethod<string, this>;
    defaultValue: FluentAssignMethod<string, this>;
    constructor(modal: Modal, defaultValues?: PromptPreset);
    $$beforeOpen(config: PromptPreset): ResolvedReflectiveProvider[];
}
