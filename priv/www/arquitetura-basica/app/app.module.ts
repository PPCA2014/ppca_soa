import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { HttpModule, XSRFStrategy, CookieXSRFStrategy, RequestOptions } from '@angular/http';

import { AppComponent } from './app.component';
import { HomeComponent } from './home/home.component';
import { NavigationComponent } from './navigation/navigation.component';
import { LoginComponent } from './login/login.component';
import { FormComponent } from './form/form.component';
import {routing, appRoutingProviders} from './app.routing';
import {AuthGuard} from "./_guards/auth.guard";
import {AuthenticationService} from "./_services/authentication.service";
import {UserService} from "./_services/user.service";
import { PessoaComponent } from './pessoa/pessoa.component';
import { PessoaService } from './pessoa/pessoa.service';
import {ListaComponent} from "./pessoa/lista/lista.component";
import { ErroComponent } from './erro/erro.component';
import {RodapeComponent} from "./rodape/rodape.component";
import {DefaultHeaders} from "./_headers/default.headers";


@NgModule({
  declarations: [
    AppComponent,
    HomeComponent,
    NavigationComponent,
    LoginComponent,
    FormComponent,
    PessoaComponent,
    ListaComponent,
    ErroComponent,
    RodapeComponent
  ],
  imports: [
    HttpModule,
    BrowserModule,
    FormsModule,
    routing
  ],
  providers: [appRoutingProviders, AuthGuard, AuthenticationService,UserService,PessoaService,
    {
      provide: XSRFStrategy,
      useValue: new CookieXSRFStrategy('csrftoken', 'X-CSRF-Token')
    },
    {
      provide: RequestOptions,
      useClass: DefaultHeaders
    }
  ],
  bootstrap: [AppComponent]
})
export class AppModule { }
