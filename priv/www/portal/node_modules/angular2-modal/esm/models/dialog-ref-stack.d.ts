import { DialogRef } from './dialog-ref';
/**
 * A dumb stack implementation over an array.
 */
export declare class DialogRefStack<T> {
    private _stack;
    private _stackMap;
    readonly length: number;
    constructor();
    push(dialogRef: DialogRef<T>, group?: any): void;
    /**
     * Push a DialogRef into the stack and manage it so when it's done
     * it will automatically kick itself out of the stack.
     * @param dialogRef
     */
    pushManaged(dialogRef: DialogRef<T>, group?: any): void;
    pop(): DialogRef<T>;
    /**
     * Remove a DialogRef from the stack.
     * @param dialogRef
     */
    remove(dialogRef: DialogRef<T>): void;
    index(index: number): DialogRef<T>;
    indexOf(dialogRef: DialogRef<T>): number;
    groupOf(dialogRef: DialogRef<T>): any;
    groupBy(group: any): DialogRef<T>[];
    groupLength(group: any): number;
}
