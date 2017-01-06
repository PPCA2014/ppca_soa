"use strict";
var __extends = (this && this.__extends) || function (d, b) {
    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
    function __() { this.constructor = d; }
    d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
};
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
var router_1 = require("@angular/router");
var default_headers_1 = require("../_headers/default.headers");
var questao_module_1 = require("./questao.module");
var QuestaoService = (function (_super) {
    __extends(QuestaoService, _super);
    function QuestaoService(http, route) {
        _super.call(this);
        this.http = http;
        this.route = route;
    }
    QuestaoService.prototype.insert = function (questao) {
        return this.http.post('http://localhost:2301/unb_aula/pessoa/' + questao.idPessoa + '/questao', questao, { headers: this.headers })
            .map(function (response) {
            console.log(response.json());
            return new questao_module_1.Questao().fromJSON(response.json());
        });
    };
    QuestaoService.prototype.findByIdPessoa = function (idPessoa) {
        return this.http.get('http://localhost:2301/unb_aula/pessoa/' + idPessoa + '/questao', { headers: this.headers })
            .map(function (response) { return response.json(); });
    };
    QuestaoService = __decorate([
        core_1.Injectable(), 
        __metadata('design:paramtypes', [http_1.Http, router_1.Router])
    ], QuestaoService);
    return QuestaoService;
}(default_headers_1.DefaultHeaders));
exports.QuestaoService = QuestaoService;
//# sourceMappingURL=questao.service.js.map