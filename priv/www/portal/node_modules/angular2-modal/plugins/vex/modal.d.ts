import 'rxjs/add/operator/first';
import 'rxjs/add/operator/combineLatest';
import { ResolvedReflectiveProvider as RRP } from '@angular/core';
import { ContainerContent, Maybe, Overlay, DialogRef, Modal as Modal_ } from 'angular2-modal';
import { DropInPresetBuilder } from './presets/dropin-preset';
export declare class Modal extends Modal_ {
    constructor(overlay: Overlay);
    alert(): DropInPresetBuilder;
    prompt(): DropInPresetBuilder;
    confirm(): DropInPresetBuilder;
    protected create(dialogRef: DialogRef<any>, content: ContainerContent, bindings?: RRP[]): Maybe<DialogRef<any>>;
}
