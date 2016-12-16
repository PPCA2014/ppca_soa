import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { HttpModule } from '@angular/http';

import { AppComponent } from './app.component';
import { NavigatorController } from '../modules/dashboard/controller/navigator_controller'
import { Sobre } from '../modules/dashboard/controller/sobre'

@NgModule({
  imports: [
    BrowserModule,
    FormsModule,
    HttpModule
  ],
  declarations: [ AppComponent, NavigatorController, Sobre ],
  bootstrap: [ AppComponent, NavigatorController ],
  providers: []
})
export class AppModule { }

