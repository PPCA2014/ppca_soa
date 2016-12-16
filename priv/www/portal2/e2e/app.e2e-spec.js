"use strict";
var app_po_1 = require('./app.po');
describe('portal2 App', function () {
    var page;
    beforeEach(function () {
        page = new app_po_1.Portal2Page();
    });
    it('should display message saying app works', function () {
        page.navigateTo();
        expect(page.getParagraphText()).toEqual('app works!');
    });
});
//# sourceMappingURL=app.e2e-spec.js.map