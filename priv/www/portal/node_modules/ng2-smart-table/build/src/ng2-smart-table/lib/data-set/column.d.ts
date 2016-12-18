import { DataSet } from './data-set';
export declare class Column {
    id: string;
    protected settings: any;
    protected dataSet: DataSet;
    title: string;
    type: string;
    class: string;
    isSortable: boolean;
    isEditable: boolean;
    isFilterable: boolean;
    sortDirection: string;
    defaultSortDirection: string;
    protected compareFunction: Function;
    protected valuePrepareFunction: Function;
    protected filterFunction: Function;
    protected cellRenderFunction: Function;
    constructor(id: string, settings: any, dataSet: DataSet);
    getCompareFunction(): Function;
    getValuePrepareFunction(): Function;
    getFilterFunction(): Function;
    getCellRenderFunction(): Function;
    protected process(): void;
    protected prepareType(): string;
    protected prepareSortDirection(): string;
    protected determineType(): string;
}
