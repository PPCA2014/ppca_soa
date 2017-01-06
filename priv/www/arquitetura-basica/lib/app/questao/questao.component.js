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
var router_1 = require("@angular/router");
var questao_module_1 = require("./questao.module");
var questao_service_1 = require("./questao.service");
var pessoa_service_1 = require("../pessoa/pessoa.service");
var QuestaoComponent = (function () {
    function QuestaoComponent(questaoService, pessoaService, route) {
        this.questaoService = questaoService;
        this.pessoaService = pessoaService;
        this.route = route;
        this.hidden = true;
    }
    QuestaoComponent.prototype.ngOnInit = function () {
        var _this = this;
        this.questao = this.questaoService.questao;
        if (this.questao == null) {
            this.questao = new questao_module_1.Questao();
        }
        else {
            this.hidden = false;
        }
        this.questaoService.questao = null;
        this.pessoaService.find()
            .subscribe(function (data) {
            _this.listaPessoa = data;
        });
    };
    QuestaoComponent.prototype.onSubmit = function () {
        var _this = this;
        this.questaoService.insert(this.questao)
            .subscribe(function (result) {
            _this.route.navigate(['questao/lista']);
        });
    };
    QuestaoComponent = __decorate([
        core_1.Component({
            selector: 'app-questao',
            templateUrl: 'app/questao/questao.component.html',
            styleUrls: ['app/questao/questao.component.css']
        }), 
        __metadata('design:paramtypes', [questao_service_1.QuestaoService, pessoa_service_1.PessoaService, router_1.Router])
    ], QuestaoComponent);
    return QuestaoComponent;
}());
exports.QuestaoComponent = QuestaoComponent;
//# sourceMappingURL=questao.component.js.map