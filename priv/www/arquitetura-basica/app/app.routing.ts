import { Routes, RouterModule } from '@angular/router';
import {HomeRoutes} from "./home/home.routes";
import {FormRoute} from "./form/form.routes";
import {PessoaRoute} from "./pessoa/pessoa.routes";
import {ErroRoute} from "./erro/erro.routes";
import {QuestaoRoute} from "./questao/questao.routes";
import { ModuleWithProviders} from '@angular/core';


const appRoutes: Routes = [
  ...HomeRoutes,
  ...FormRoute,
  ...PessoaRoute,
  ...ErroRoute,
  ...QuestaoRoute
];

export const appRoutingProviders: any[] = [
];

export const routing = RouterModule.forRoot(appRoutes);
