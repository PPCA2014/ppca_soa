import { NgModule }      from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';
import { HttpModule } from '@angular/http';

import { AppComponent }  from './app.component';
import {routing ,appRoutingProviders} from "./app.routing";
import {QuestaoService} from "./questao/questao.service";
import {QuestaoComponent} from "./questao/questao.component";
import {HomeComponent} from "./home/home.component";
import {NavigationComponent, AuthenticationService, AuthGuard, ErroComponent} from "seguranca";
import {RodapeComponent, RedirectService} from "seguranca";


@NgModule({
  imports:      [ BrowserModule, FormsModule, HttpModule, routing ],
  declarations: [ AppComponent, QuestaoComponent, HomeComponent, NavigationComponent, RodapeComponent, ErroComponent ],
  providers: [appRoutingProviders, QuestaoService, AuthenticationService, AuthGuard, RedirectService ],
  bootstrap: [ AppComponent ]
})
export class AppModule { }
