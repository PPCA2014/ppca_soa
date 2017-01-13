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
var router_1 = require("@angular/router");
require('rxjs/add/operator/map');
var AuthenticationService = (function () {
    function AuthenticationService(http, route, options) {
        this.http = http;
        this.route = route;
        this.options = options;
        this.time = 0;
        this.intervalId = null;
        var currentUser = JSON.parse(localStorage.getItem('currentUser'));
        this.token = currentUser && currentUser.token;
    }
    AuthenticationService.prototype.login = function (login, senha) {
        var _this = this;
        this.getUrl(login, senha);
        return this.http.post(this.url, this.body)
            .map(function (response) {
            var token = response.json() && response.json();
            if (token) {
                _this.token = token;
                localStorage.setItem('currentUser', JSON.stringify(response.json()));
                var sessionTime = JSON.parse(localStorage.getItem('currentUser'));
                _this.periodicIncrement(sessionTime.expires_in);
                return true;
            }
            else {
                return false;
            }
        });
    };
    AuthenticationService.prototype.getUrl = function (login, senha) {
        var _this = this;
        return this.http.get('seguranca/url_security.json')
            .map(function (res) {
            var json = res.json();
            _this.url = json.url + '' + json.param1 + '' + login + '' + json.param2 + '' + senha;
            _this.body = json.body;
            return _this.url;
        });
    };
    AuthenticationService.prototype.periodicIncrement = function (sessionTime) {
        var _this = this;
        this.cancelPeriodicIncrement();
        this.time = sessionTime * 1000;
        this.intervalId = setInterval(function () {
            if (_this.time == 0) {
                _this.logout();
                return 0;
            }
            _this.time = _this.time - 1000;
            return _this.time;
        }, 1000);
    };
    ;
    AuthenticationService.prototype.cancelPeriodicIncrement = function () {
        if (this.intervalId != null) {
            clearInterval(this.intervalId);
            this.intervalId = null;
            this.time = 0;
        }
    };
    ;
    AuthenticationService.prototype.getSitemap = function () {
        return this.http.get('/arquitetura-basica/menu.json')
            .map(function (res) {
            var sitemap = res.json();
            sessionStorage.setItem('menu', JSON.stringify(sitemap));
            return sitemap;
        });
    };
    AuthenticationService.prototype.logout = function () {
        this.cancelPeriodicIncrement();
        this.token = null;
        localStorage.removeItem('currentUser');
        this.route.navigate(['']);
    };
    AuthenticationService = __decorate([
        core_1.Injectable(), 
        __metadata('design:paramtypes', [http_1.Http, router_1.Router, http_1.RequestOptions])
    ], AuthenticationService);
    return AuthenticationService;
}());
exports.AuthenticationService = AuthenticationService;
//# sourceMappingURL=authentication.service.js.map