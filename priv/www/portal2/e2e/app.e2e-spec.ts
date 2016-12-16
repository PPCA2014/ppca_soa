import { Portal2Page } from './app.po';

describe('portal2 App', function() {
  let page: Portal2Page;

  beforeEach(() => {
    page = new Portal2Page();
  });

  it('should display message saying app works', () => {
    page.navigateTo();
    expect(page.getParagraphText()).toEqual('app works!');
  });
});
