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
var core_1 = require("@angular/core");
var core_2 = require("@angular/core");
var sitemap_service_1 = require("../service/sitemap_service");
var NavigatorController = (function () {
    function NavigatorController(sitemapService, componentResolver) {
        this.sitemapService = sitemapService;
        this.componentResolver = componentResolver;
        this.sitemap = { "name": "dashboard",
            "title": "Dashboard",
            "url": "/portal/dashboard",
            "image_url": "modules/dashboard/img/pedidos.png",
            "items": [] };
        this.current = [];
        this.current_page = 1;
        this.current_url = undefined;
        this.breadcrumb = null;
    }
    NavigatorController.prototype.ngOnInit = function () {
        var _this = this;
        console.log("sitemap...");
        this.sitemapService.getSitemap().subscribe(function (res) {
            _this.sitemap = res;
            _this.current = _this.sitemap;
            _this.breadcrumb = _this.get_breadcrumb(_this.current);
        });
    };
    NavigatorController.prototype.go = function (item) {
        this.current = item;
        this.breadcrumb = this.get_breadcrumb(this.current);
        if (item.url != undefined) {
            this.current_page = 3;
            this.current_url = item.url;
        }
    };
    NavigatorController.prototype.get_breadcrumb = function (item) {
        return this.make_breadcrumb(item, []);
    };
    NavigatorController.prototype.make_breadcrumb = function (item, result) {
        if (item.owner != null) {
            this.make_breadcrumb(item.owner, result);
        }
        result.push(item);
        return result;
    };
    NavigatorController.prototype.setCurrentPage = function (page) {
        this.current_page = parseInt(page);
    };
    return NavigatorController;
}());
NavigatorController = __decorate([
    core_1.Component({
        selector: 'navigator',
        providers: [sitemap_service_1.SitemapService],
        templateUrl: 'modules/dashboard/web/navigator.html'
    }),
    __metadata("design:paramtypes", [sitemap_service_1.SitemapService, typeof (_a = typeof core_2.ComponentResolver !== "undefined" && core_2.ComponentResolver) === "function" && _a || Object])
], NavigatorController);
exports.NavigatorController = NavigatorController;
var _a;
//# sourceMappingURL=navigator_controller.js.map