"use strict";
var auth_guard_1 = require("../_guards/auth.guard");
var questao_component_1 = require("./questao.component");
exports.QuestaoRoute = [
    { path: 'questao', component: questao_component_1.QuestaoComponent, canActivate: [auth_guard_1.AuthGuard] }
];
//# sourceMappingURL=questao.routes.js.map