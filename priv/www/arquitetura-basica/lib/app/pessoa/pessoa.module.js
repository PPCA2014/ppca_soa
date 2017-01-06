"use strict";
var Pessoa = (function () {
    function Pessoa() {
    }
    Pessoa.prototype.fromJSON = function (json) {
        for (var propName in json)
            this[propName] = json[propName];
        return this;
    };
    return Pessoa;
}());
exports.Pessoa = Pessoa;
//# sourceMappingURL=pessoa.module.js.map