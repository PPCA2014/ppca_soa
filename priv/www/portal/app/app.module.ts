import { NgModule }      from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from "@angular/forms";
import { HttpModule, JsonpModule } from '@angular/http';

import { AppComponent }  from './app.component';
import { NavigatorController } from '../modules/dashboard/controller/navigator_controller';
import { Sobre } from '../modules/dashboard/controller/sobre';
import { CatalogoController } from '../modules/catalogo/catalogo_controller';
import { LoginComponent } from '../modules/login/login_component';
import { DataTableModule } from 'angular2-datatable';

import { ModalModule } from 'angular2-modal';
import { BootstrapModalModule } from 'angular2-modal/plugins/bootstrap';
import { CustomModal } from '../modules/catalogo/exemplos_url_servico_component';




@NgModule({
  imports: [
    BrowserModule,
    FormsModule,
    HttpModule,
    DataTableModule,
    ModalModule.forRoot(),
    BootstrapModalModule],
  declarations: [ AppComponent, NavigatorController, Sobre, CatalogoController, LoginComponent, CustomModal ],
  bootstrap: [ AppComponent, NavigatorController ],

  // IMPORTANT: 
  // Since 'AdditionCalculateWindow' is never explicitly used (in a template)
  // we must tell angular about it.
  entryComponents: [ CustomModal ]
})
export class AppModule { }
