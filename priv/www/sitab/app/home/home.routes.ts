import { Routes } from '@angular/router';

import { HomeComponent } from './home.component';
import {AuthGuard} from "seguranca";

export const HomeRoutes: Routes = [
  { path: '',  component: HomeComponent, canActivate: [AuthGuard] }
];
