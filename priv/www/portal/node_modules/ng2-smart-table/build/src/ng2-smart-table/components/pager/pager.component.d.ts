import { DataSource } from '../../lib/data-source/data-source';
export declare class PagerComponent {
    perPage: number;
    source: DataSource;
    protected pages: Array<any>;
    protected page: number;
    protected count: number;
    ngOnInit(): void;
    /**
     * We change the page here depending on the action performed against data source
     * if a new element was added to the end of the table - then change the page to the last
     * if a new element was added to the beginning of the table - then to the first page
     * @param changes
     */
    processPageChange(changes: any): void;
    shouldShow(): boolean;
    paginate(page: number): boolean;
    getPage(): number;
    getPages(): Array<any>;
    getLast(): number;
    protected isPageOutOfBounce(): boolean;
    protected initPages(): void;
}
