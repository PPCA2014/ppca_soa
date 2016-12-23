import 'rxjs/add/operator/combineLatest';
import { ResolvedReflectiveProvider as RRP } from '@angular/core';
import { Maybe, ContainerContent, Overlay, DialogRef, Modal as Modal_ } from 'angular2-modal';
import { OneButtonPresetBuilder } from './../bootstrap/presets/one-button-preset';
import { TwoButtonPresetBuilder, PromptPresetBuilder } from './../bootstrap/presets/two-button-preset';
export declare class Modal extends Modal_ {
    constructor(overlay: Overlay);
    alert(): OneButtonPresetBuilder;
    prompt(): PromptPresetBuilder;
    confirm(): TwoButtonPresetBuilder;
    protected create(dialogRef: DialogRef<any>, content: ContainerContent, bindings?: RRP[]): Maybe<DialogRef<any>>;
}
