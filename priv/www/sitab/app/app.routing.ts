import { Routes, RouterModule } from '@angular/router';
import {QuestaoRoute} from "./questao/questao.routes";


const appRoutes: Routes = [
  ...QuestaoRoute
];

export const appRoutingProviders: any[] = [
];

export const routing = RouterModule.forRoot(appRoutes);
