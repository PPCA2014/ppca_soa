"use strict";
var protractor_1 = require('protractor');
var Portal2Page = (function () {
    function Portal2Page() {
    }
    Portal2Page.prototype.navigateTo = function () {
        return protractor_1.browser.get('/');
    };
    Portal2Page.prototype.getParagraphText = function () {
        return protractor_1.element(protractor_1.by.css('app-root h1')).getText();
    };
    return Portal2Page;
}());
exports.Portal2Page = Portal2Page;
//# sourceMappingURL=app.po.js.map