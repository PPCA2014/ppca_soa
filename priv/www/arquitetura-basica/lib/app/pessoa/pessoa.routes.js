"use strict";
var pessoa_component_1 = require("./pessoa.component");
var auth_guard_1 = require("../_guards/auth.guard");
var lista_component_1 = require("./lista/lista.component");
exports.PessoaRoute = [
    { path: 'pessoa', component: pessoa_component_1.PessoaComponent, canActivate: [auth_guard_1.AuthGuard] },
    { path: 'pessoa/lista', component: lista_component_1.ListaComponent, canActivate: [auth_guard_1.AuthGuard] }
];
//# sourceMappingURL=pessoa.routes.js.map