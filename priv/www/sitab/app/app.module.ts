import { NgModule }      from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';

import { AppComponent }  from './app.component';
import {routing ,appRoutingProviders} from "./app.routing";
import {QuestaoService} from "./questao/questao.service";
import {QuestaoComponent} from "./questao/questao.component";


@NgModule({
  imports:      [ BrowserModule, FormsModule, routing ],
  declarations: [ AppComponent, QuestaoComponent ],
  providers: [appRoutingProviders, QuestaoService ],
  bootstrap: [ AppComponent ]
})
export class AppModule { }
