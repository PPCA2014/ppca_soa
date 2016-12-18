import { Cell } from './cell';
import { Column } from './column';
import { DataSet } from './data-set';
export declare class Row {
    index: number;
    protected data: any;
    protected _dataSet: DataSet;
    isSelected: boolean;
    isInEditing: boolean;
    protected cells: Array<Cell>;
    constructor(index: number, data: any, _dataSet: DataSet);
    getCell(column: Column): Cell;
    getCells(): Cell[];
    getData(): any;
    getNewData(): any;
    setData(data: any): any;
    protected process(): void;
    protected createCell(column: Column): Cell;
}
