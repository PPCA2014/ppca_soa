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
var pessoa_service_1 = require("../pessoa.service");
var router_1 = require("@angular/router");
var ListaComponent = (function () {
    function ListaComponent(pessoaService, route) {
        this.pessoaService = pessoaService;
        this.route = route;
        this.pessoas = [];
    }
    ListaComponent.prototype.ngOnInit = function () {
        var _this = this;
        this.pessoaService.find()
            .subscribe(function (data) {
            _this.pessoas = data;
        });
    };
    ListaComponent.prototype.setPessoa = function (pessoa) {
        this.pessoa = pessoa;
    };
    ListaComponent.prototype.editar = function (pessoa) {
        this.pessoaService.redirecionarEditar(pessoa);
    };
    ListaComponent.prototype.deletar = function () {
        var _this = this;
        this.pessoaService.delete(this.pessoa).subscribe(function (res) {
            _this.pessoaService.find()
                .subscribe(function (data) {
                _this.pessoas = data;
                document.getElementById("apagar").click();
            });
        });
    };
    ListaComponent = __decorate([
        core_1.Component({
            selector: 'app-lista',
            templateUrl: 'app/pessoa/lista/lista.component.html',
            styleUrls: ['app/pessoa/lista/lista.component.css']
        }), 
        __metadata('design:paramtypes', [pessoa_service_1.PessoaService, router_1.Router])
    ], ListaComponent);
    return ListaComponent;
}());
exports.ListaComponent = ListaComponent;
//# sourceMappingURL=lista.component.js.map