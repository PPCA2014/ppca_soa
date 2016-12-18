"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
var core_1 = require('@angular/core');
var common_1 = require('@angular/common');
var forms_1 = require('@angular/forms');
var ng2_smart_table_directives_1 = require('./ng2-smart-table.directives');
var cell_component_1 = require('./ng2-smart-table/components/cell/cell.component');
var filter_component_1 = require('./ng2-smart-table/components/filter/filter.component');
var pager_component_1 = require('./ng2-smart-table/components/pager/pager.component');
var title_component_1 = require('./ng2-smart-table/components/title/title.component');
var Ng2SmartTableModule = (function () {
    function Ng2SmartTableModule() {
    }
    Ng2SmartTableModule = __decorate([
        core_1.NgModule({
            imports: [
                common_1.CommonModule,
                forms_1.FormsModule
            ],
            declarations: [
                cell_component_1.CellComponent,
                filter_component_1.FilterComponent,
                pager_component_1.PagerComponent,
                title_component_1.TitleComponent
            ].concat(ng2_smart_table_directives_1.NG2_SMART_TABLE_DIRECTIVES),
            exports: ng2_smart_table_directives_1.NG2_SMART_TABLE_DIRECTIVES.slice()
        }), 
        __metadata('design:paramtypes', [])
    ], Ng2SmartTableModule);
    return Ng2SmartTableModule;
}());
exports.Ng2SmartTableModule = Ng2SmartTableModule;
