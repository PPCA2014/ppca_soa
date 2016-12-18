import { DataSource } from '../../lib/data-source/data-source';
import { Column } from '../../lib/data-set/column';
export declare class TitleComponent {
    column: Column;
    source: DataSource;
    protected currentDirection: string;
    ngOnInit(): void;
    sort(): boolean;
    changeSortDirection(): string;
}
