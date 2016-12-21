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
var http_1 = require('@angular/http');
var CatalogoController = (function () {
    function CatalogoController(http) {
        var _this = this;
        this.http = http;
        this.operacao = "pesquisa";
        this.ult_operacao = "pesquisa";
        this.owner = "";
        this.filterQuery = "";
        this.rowsOnPage = 10;
        this.sortBy = "email";
        this.sortOrder = "asc";
        this.lista_owners = null;
        this.sortByWordLength = function (a) {
            return a.city.length;
        };
        // busca os owners
        this.http.get("/catalog/owner")
            .subscribe(function (data) {
            setTimeout(function () {
                _this.lista_owners = data.json();
            }, 1000);
        });
    }
    CatalogoController.prototype.ngOnInit = function () {
    };
    CatalogoController.prototype.toInt = function (num) {
        return +num;
    };
    CatalogoController.prototype.voltar = function () {
        this.operacao = this.ult_operacao;
        this.ult_operacao = "pesquisa";
    };
    CatalogoController.prototype.pesquisar = function () {
        var _this = this;
        this.ult_operacao = this.operacao;
        this.operacao = "listagem";
        this.http.get("/catalog")
            .subscribe(function (data) {
            setTimeout(function () {
                _this.data = data.json();
            }, 1000);
        });
    };
    CatalogoController.prototype.novo = function () {
        this.ult_operacao = this.operacao;
        this.operacao = "edicao";
    };
    CatalogoController = __decorate([
        core_1.Component({
            selector: 'catalogo',
            templateUrl: 'modules/catalogo/catalogo.html'
        }), 
        __metadata('design:paramtypes', [http_1.Http])
    ], CatalogoController);
    return CatalogoController;
}());
exports.CatalogoController = CatalogoController;
//# sourceMappingURL=catalogo_controller.js.map