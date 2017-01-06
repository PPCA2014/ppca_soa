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
var platform_browser_1 = require('@angular/platform-browser');
var core_1 = require('@angular/core');
var forms_1 = require('@angular/forms');
var http_1 = require('@angular/http');
var app_component_1 = require('./app.component');
var home_component_1 = require('./home/home.component');
var navigation_component_1 = require('./navigation/navigation.component');
var login_component_1 = require('./login/login.component');
var form_component_1 = require('./form/form.component');
var app_routing_1 = require('./app.routing');
var auth_guard_1 = require("./_guards/auth.guard");
var authentication_service_1 = require("./_services/authentication.service");
var user_service_1 = require("./_services/user.service");
var pessoa_component_1 = require('./pessoa/pessoa.component');
var pessoa_service_1 = require('./pessoa/pessoa.service');
var lista_component_1 = require("./pessoa/lista/lista.component");
var erro_component_1 = require('./erro/erro.component');
var rodape_component_1 = require("./rodape/rodape.component");
var default_headers_1 = require("./_headers/default.headers");
var questao_component_1 = require("./questao/questao.component");
var questao_service_1 = require("./questao/questao.service");
var AppModule = (function () {
    function AppModule() {
    }
    AppModule = __decorate([
        core_1.NgModule({
            declarations: [
                app_component_1.AppComponent,
                home_component_1.HomeComponent,
                navigation_component_1.NavigationComponent,
                login_component_1.LoginComponent,
                form_component_1.FormComponent,
                pessoa_component_1.PessoaComponent,
                lista_component_1.ListaComponent,
                erro_component_1.ErroComponent,
                rodape_component_1.RodapeComponent,
                questao_component_1.QuestaoComponent
            ],
            imports: [
                http_1.HttpModule,
                platform_browser_1.BrowserModule,
                forms_1.FormsModule,
                app_routing_1.routing
            ],
            providers: [app_routing_1.appRoutingProviders, auth_guard_1.AuthGuard, authentication_service_1.AuthenticationService, user_service_1.UserService, pessoa_service_1.PessoaService, questao_service_1.QuestaoService,
                {
                    provide: http_1.XSRFStrategy,
                    useValue: new http_1.CookieXSRFStrategy('csrftoken', 'X-CSRF-Token')
                },
                {
                    provide: http_1.RequestOptions,
                    useClass: default_headers_1.DefaultHeaders
                }
            ],
            bootstrap: [app_component_1.AppComponent]
        }), 
        __metadata('design:paramtypes', [])
    ], AppModule);
    return AppModule;
}());
exports.AppModule = AppModule;
//# sourceMappingURL=app.module.js.map