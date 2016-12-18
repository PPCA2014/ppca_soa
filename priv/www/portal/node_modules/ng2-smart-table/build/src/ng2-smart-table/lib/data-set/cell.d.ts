import { Column } from './column';
import { DataSet } from './data-set';
import { Row } from './row';
export declare class Cell {
    protected value: any;
    protected row: Row;
    protected column: any;
    protected dataSet: DataSet;
    newValue: string;
    protected static PREPARE: (value: any) => any;
    constructor(value: any, row: Row, column: any, dataSet: DataSet);
    getValue(): any;
    getColumn(): Column;
    getRow(): Row;
}
