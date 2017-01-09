import { Routes, RouterModule } from '@angular/router';
import {QuestaoRoute} from "./questao/questao.routes";
import {HomeRoutes} from "./home/home.routes";
import {ErroRoute} from "./erro/erro.routes";


const appRoutes: Routes = [
  ...QuestaoRoute,
  ...HomeRoutes,
  ...ErroRoute
];

export const appRoutingProviders: any[] = [
];

export const routing = RouterModule.forRoot(appRoutes);
