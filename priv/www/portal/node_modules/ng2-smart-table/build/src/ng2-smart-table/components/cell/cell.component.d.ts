import { EventEmitter, ElementRef } from '@angular/core';
import { Cell } from '../../lib/data-set/cell';
export declare class CellComponent {
    cell: Cell;
    inputClass: string;
    mode: string;
    edited: EventEmitter<any>;
    cellRef: ElementRef;
    onStopEditing(): boolean;
    ngAfterViewInit(): void;
    onEdited(event: any): boolean;
    onClick(event: any): void;
}
