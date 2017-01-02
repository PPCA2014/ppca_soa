import { Routes, RouterModule } from '@angular/router';
import {HomeRoutes} from "./home/home.routes";
import {FormRoute} from "./form/form.routes";
import {PessoaRoute} from "./pessoa/pessoa.routes";
import {ErroRoute} from "./erro/erro.routes";
import {AppComponent} from "./app.component";


const appRoutes: Routes = [
  ...HomeRoutes,
  ...FormRoute,
  ...PessoaRoute,
  ...ErroRoute
];

export const appRoutingProviders: any[] = [
];

export const routing = RouterModule.forRoot(appRoutes);
