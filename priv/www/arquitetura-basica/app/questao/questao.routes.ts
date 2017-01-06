import { Routes } from '@angular/router';
import {AuthGuard} from "../_guards/auth.guard";
import {QuestaoComponent} from "./questao.component";


export const QuestaoRoute: Routes = [
  { path: 'questao', component: QuestaoComponent, canActivate: [AuthGuard] }
];
