import { Routes } from '@angular/router';
import { FormComponent } from './form.component';
import {AuthGuard} from "../_guards/auth.guard";

export const FormRoute: Routes = [
  { path: 'formulario',  component: FormComponent, canActivate: [AuthGuard]  }
];
