import { NgModule }      from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from "@angular/forms";
import { HttpModule } from '@angular/http';

import { NavigatorController, PagerService, Sobre } from './dashboard/main';

import { AppComponent }  from './app.component';
import { DataTableModule } from 'angular2-datatable';
import { ModalModule } from 'angular2-modal';
import { BootstrapModalModule } from 'angular2-modal/plugins/bootstrap';

import { CustomModal } from './catalogo/exemplos_url_servico.component';
import { CatalogoComponent } from './catalogo/catalogo.controller';
import { LoginComponent } from './login/login.component';
import { DataTableFilterPipe } from './dashboard/datatable/datatable_filter.pipe';



@NgModule({
  imports: [
    BrowserModule,
    FormsModule,
    HttpModule,
    DataTableModule, 
    ModalModule.forRoot(),
    BootstrapModalModule],
  declarations: [ AppComponent, NavigatorController, Sobre, CatalogoComponent, LoginComponent, CustomModal, DataTableFilterPipe ],
  bootstrap: [ AppComponent, NavigatorController ],
  providers: [ PagerService ],

  // IMPORTANT:
  // Since 'AdditionCalculateWindow' is never explicitly used (in a template)
  // we must tell angular about it.
  entryComponents: [ CustomModal ]
})
export class AppModule { }
