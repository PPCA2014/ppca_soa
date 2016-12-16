import { NgModule }      from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import {FormsModule} from "@angular/forms";
import { HttpModule, JsonpModule } from '@angular/http';

import { AppComponent }  from './app.component';
import { NavigatorController } from '../modules/dashboard/controller/navigator_controller'
import { Sobre } from '../modules/dashboard/controller/sobre'

@NgModule({
  imports: [
    BrowserModule,
    FormsModule,
    HttpModule
  ],
  declarations: [ AppComponent, NavigatorController, Sobre, CatalogoController ],
  bootstrap: [ AppComponent, NavigatorController, CatalogoController ]
})
export class AppModule { }
