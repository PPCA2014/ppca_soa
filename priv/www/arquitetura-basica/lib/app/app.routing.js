"use strict";
var router_1 = require('@angular/router');
var home_routes_1 = require("./home/home.routes");
var form_routes_1 = require("./form/form.routes");
var pessoa_routes_1 = require("./pessoa/pessoa.routes");
var erro_routes_1 = require("./erro/erro.routes");
var questao_routes_1 = require("./questao/questao.routes");
var appRoutes = home_routes_1.HomeRoutes.concat(form_routes_1.FormRoute, pessoa_routes_1.PessoaRoute, erro_routes_1.ErroRoute, questao_routes_1.QuestaoRoute);
exports.appRoutingProviders = [];
exports.routing = router_1.RouterModule.forRoot(appRoutes);
//# sourceMappingURL=app.routing.js.map