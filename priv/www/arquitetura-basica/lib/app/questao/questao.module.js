"use strict";
var Questao = (function () {
    function Questao() {
    }
    Questao.prototype.fromJSON = function (json) {
        for (var propName in json)
            this[propName] = json[propName];
        return this;
    };
    return Questao;
}());
exports.Questao = Questao;
//# sourceMappingURL=questao.module.js.map