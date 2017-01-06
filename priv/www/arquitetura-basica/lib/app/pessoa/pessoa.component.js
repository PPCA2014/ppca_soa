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
var pessoa_module_1 = require("./pessoa.module");
var pessoa_service_1 = require("./pessoa.service");
var router_1 = require("@angular/router");
var PessoaComponent = (function () {
    function PessoaComponent(pessoaService, route) {
        this.pessoaService = pessoaService;
        this.route = route;
        this.hidden = true;
    }
    PessoaComponent.prototype.ngOnInit = function () {
        this.model = this.pessoaService.pessoa;
        if (this.model == null) {
            this.model = new pessoa_module_1.Pessoa();
        }
        else {
            this.hidden = false;
        }
        this.pessoaService.pessoa = null;
    };
    PessoaComponent.prototype.onSubmit = function () {
        var _this = this;
        this.pessoaService.insert(this.model)
            .subscribe(function (result) {
            if (result.nome !== '') {
                _this.route.navigate(['/pessoa/lista']);
            }
            else {
                console.log('Erro ao cadastrar');
            }
        });
    };
    PessoaComponent.prototype.editar = function () {
        var _this = this;
        this.pessoaService.update(this.model)
            .subscribe(function (result) {
            _this.route.navigate(['/pessoa/lista']);
        });
    };
    PessoaComponent.prototype.newHero = function () {
        this.model = new pessoa_module_1.Pessoa();
    };
    PessoaComponent = __decorate([
        core_1.Component({
            selector: 'app-pessoa',
            templateUrl: 'app/pessoa/pessoa.component.html',
            styleUrls: ['app/pessoa/pessoa.component.css']
        }), 
        __metadata('design:paramtypes', [pessoa_service_1.PessoaService, router_1.Router])
    ], PessoaComponent);
    return PessoaComponent;
}());
exports.PessoaComponent = PessoaComponent;
//# sourceMappingURL=pessoa.component.js.map