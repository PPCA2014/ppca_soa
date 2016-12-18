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
var data_source_1 = require('../../lib/data-source/data-source');
var column_1 = require('../../lib/data-set/column');
var FilterComponent = (function () {
    function FilterComponent() {
        this.inputClass = '';
        this.query = '';
        this.delay = 300;
    }
    FilterComponent.prototype.ngAfterViewInit = function () {
        var _this = this;
        this.source.onChanged().subscribe(function (elements) {
            var filterConf = _this.source.getFilter();
            if (filterConf && filterConf.filters && filterConf.filters.length === 0) {
                _this.query = '';
            }
        });
    };
    FilterComponent.prototype.filter = function (event) {
        var _this = this;
        if (event.which === 13) {
            this.addFilter();
        }
        else if (event.which !== 9) {
            if (this.timeout) {
                clearTimeout(this.timeout);
            }
            this.timeout = setTimeout(function () {
                _this.addFilter();
            }, this.delay);
        }
        return false;
    };
    FilterComponent.prototype.addFilter = function () {
        this.source.addFilter({
            field: this.column.id,
            search: this.query,
            filter: this.column.getFilterFunction()
        });
    };
    __decorate([
        core_1.Input(), 
        __metadata('design:type', column_1.Column)
    ], FilterComponent.prototype, "column", void 0);
    __decorate([
        core_1.Input(), 
        __metadata('design:type', data_source_1.DataSource)
    ], FilterComponent.prototype, "source", void 0);
    __decorate([
        core_1.Input(), 
        __metadata('design:type', String)
    ], FilterComponent.prototype, "inputClass", void 0);
    FilterComponent = __decorate([
        core_1.Component({
            selector: 'ng2-smart-table-filter',
            styles: [require('./filter.scss')],
            template: "\n    <div class=\"ng2-smart-filter\" *ngIf=\"column.isFilterable\">\n      <input \n      [(ngModel)]=\"query\"\n      (keyup)=\"filter($event)\"\n      [ngClass]=\"inputClass\"\n      class=\"form-control\"\n      type=\"text\" \n      placeholder=\"{{ column.title }}\" />\n    </div>\n  "
        }), 
        __metadata('design:paramtypes', [])
    ], FilterComponent);
    return FilterComponent;
}());
exports.FilterComponent = FilterComponent;
