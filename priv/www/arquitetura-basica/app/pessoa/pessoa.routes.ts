import { Routes } from '@angular/router';
import {PessoaComponent} from "./pessoa.component";
import {AuthGuard} from "../_guards/auth.guard";
import {ListaComponent} from "./lista/lista.component";

export const PessoaRoute: Routes = [
  { path: 'pessoa',  component: PessoaComponent,  canActivate: [AuthGuard] },
  { path: 'pessoa/lista', component: ListaComponent, canActivate: [AuthGuard] }
];
