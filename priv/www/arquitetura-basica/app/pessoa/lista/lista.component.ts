import { Component, OnInit, ViewChild } from '@angular/core';
import {Pessoa} from "../pessoa.module";
import {PessoaService} from "../pessoa.service";
import { Router } from "@angular/router";

@Component({
  selector: 'app-lista',
  templateUrl: 'app/pessoa/lista/lista.component.html',
  styleUrls: ['app/pessoa/lista/lista.component.css']
})
export class ListaComponent implements OnInit {

   pessoas: Pessoa[] = [];
   pessoa: Pessoa;

  constructor(private pessoaService: PessoaService, private route: Router) { }

  ngOnInit() {
    this.pessoaService.find()
      .subscribe(
        (data: Pessoa[]) => {
          this.pessoas = data;
        }
      );
  }

  setPessoa(pessoa: Pessoa){
    this.pessoa = pessoa;
  }

  editar(pessoa: Pessoa){
     this.pessoaService.redirecionarEditar(pessoa);
  }

  deletar(){
    this.pessoaService.delete(this.pessoa).subscribe(res => {
      this.pessoaService.find()
        .subscribe(
          (data: Pessoa[]) => {
            this.pessoas = data;
            document.getElementById("apagar").click();
          }
        );
    });
  }

}
