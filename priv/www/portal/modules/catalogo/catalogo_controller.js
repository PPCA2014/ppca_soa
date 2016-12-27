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
var angular2_modal_1 = require('angular2-modal');
var bootstrap_1 = require('angular2-modal/plugins/bootstrap');
var exemplos_url_servico_component_1 = require('./exemplos_url_servico_component');
var http_1 = require('@angular/http');
var Observable_1 = require('rxjs/Observable');
var catalogo_1 = require('./catalogo');
var CatalogoController = (function () {
    function CatalogoController(http, modal, vcRef) {
        var _this = this;
        this.http = http;
        this.modal = modal;
        this.catalogoUrl = "/catalog";
        this.catalogoOwnerUrl = "/catalog/owner";
        this.operacao = "pesquisa";
        this.ult_operacao = "pesquisa";
        this.owner = "";
        this.filterQuery = "";
        this.rowsOnPage = 10;
        this.sortBy = "email";
        this.sortOrder = "asc";
        this.owner_list = null;
        this.language_list = [{ "name": "erlang", "title": "Linguagem Erlang" }, { "name": "java", "title": "Linguagem Java" }];
        this.authentication_list = [{ "name": "", "title": "Sem autenticação" }, { "name": "basic", "title": "Protocolo HTTP Basic" }, { "name": "oauth", "title": "Protocolo Oauth 2.0" }];
        this.model = new catalogo_1.Catalogo();
        this.sortByWordLength = function (a) {
            return a.city.length;
        };
        modal.overlay.defaultViewContainer = vcRef;
        // busca os owners
        this.http.get(this.catalogoOwnerUrl)
            .subscribe(function (data) {
            setTimeout(function () {
                _this.owner_list = data.json();
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
        this.http.get(this.catalogoUrl)
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
    CatalogoController.prototype.openDialogExemplos = function () {
        return this.modal.open(exemplos_url_servico_component_1.CustomModal, angular2_modal_1.overlayConfigFactory({}, bootstrap_1.BSModalContext));
    };
    CatalogoController.prototype.handleError = function (error) {
        // In a real world app, we might use a remote logging infrastructure
        var errMsg;
        if (error instanceof http_1.Response) {
            var body = error.json() || '';
            var err = body.error || JSON.stringify(body);
            errMsg = error.status + " - " + (error.statusText || '') + " " + err;
        }
        else {
            errMsg = error.message ? error.message : error.toString();
        }
        console.error(errMsg);
        return Observable_1.Observable.throw(errMsg);
    };
    CatalogoController.prototype.extractData = function (res) {
        var body = res.json();
        return body.data || {};
    };
    CatalogoController.prototype.salvar = function () {
        var _this = this;
        var headers = new http_1.Headers({ 'Content-Type': 'application/json' });
        var options = new http_1.RequestOptions({ headers: headers });
        this.model.name = this.model.url;
        this.model.public = true;
        this.http.post(this.catalogoUrl, this.model, options)
            .map(this.extractData)
            .catch(this.handleError)
            .subscribe(function (cat) { return _this.data.push(cat); }, function (error) { return _this.errorMessage = error; });
    };
    CatalogoController = __decorate([
        core_1.Component({
            selector: 'catalogo',
            templateUrl: 'modules/catalogo/catalogo.html',
            providers: [bootstrap_1.Modal]
        }), 
        __metadata('design:paramtypes', [http_1.Http, bootstrap_1.Modal, core_1.ViewContainerRef])
    ], CatalogoController);
    return CatalogoController;
}());
exports.CatalogoController = CatalogoController;
//# sourceMappingURL=catalogo_controller.js.map