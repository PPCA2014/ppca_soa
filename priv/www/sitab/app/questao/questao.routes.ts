import { Routes } from '@angular/router';
import {QuestaoComponent} from "./questao.component";
import {AuthGuard} from 'seguranca';


export const QuestaoRoute: Routes = [
  { path: 'questao', component: QuestaoComponent, canActivate: [AuthGuard] }
];
