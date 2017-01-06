"use strict";
var form_component_1 = require('./form.component');
var auth_guard_1 = require("../_guards/auth.guard");
exports.FormRoute = [
    { path: 'formulario', component: form_component_1.FormComponent, canActivate: [auth_guard_1.AuthGuard] }
];
//# sourceMappingURL=form.routes.js.map