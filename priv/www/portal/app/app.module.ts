import { NgModule }      from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import {FormsModule} from "@angular/forms";
import { HttpModule, JsonpModule } from '@angular/http';

import { AppComponent }  from './app.component';
import { NavigatorController } from '../modules/dashboard/controller/navigator_controller';
import { Sobre } from '../modules/dashboard/controller/sobre';
import { CatalogoController } from '../modules/catalogo/catalogo_controller';
import { LoginComponent } from '../modules/login/login_component';

@NgModule({
  imports: [
    BrowserModule,
    FormsModule,
    HttpModule
  ],
  declarations: [ AppComponent, NavigatorController, Sobre, CatalogoController, LoginComponent ],
  bootstrap: [ AppComponent, NavigatorController ]
})
export class AppModule { }
