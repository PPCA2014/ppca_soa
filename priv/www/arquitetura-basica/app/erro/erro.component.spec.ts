/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';

import { ErroComponent } from './erro.component';

describe('ErroComponent', () => {
  let component: ErroComponent;
  let fixture: ComponentFixture<ErroComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ ErroComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ErroComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
