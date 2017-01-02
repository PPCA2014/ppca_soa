import { Component, OnInit } from '@angular/core';
import {Pessoa} from "./pessoa.module";
import {PessoaService} from "./pessoa.service";
import { Router } from "@angular/router";

@Component({
  selector: 'app-pessoa',
  templateUrl: 'app/pessoa/pessoa.component.html',
  styleUrls: ['app/pessoa/pessoa.component.css']
})
export class PessoaComponent implements OnInit {

  constructor(private pessoaService: PessoaService, private route:Router) { }

  ngOnInit() {
    this.model = this.pessoaService.pessoa;
    if(this.model == null){
      this.model = new Pessoa();
    }else {
      this.hidden = false;
    }
    this.pessoaService.pessoa = null;
  }

  model: Pessoa;
  hidden:boolean = true;


  onSubmit() {
    this.pessoaService.insert(this.model)
      .subscribe(result => {
        if (result.nome !== '') {
          this.route.navigate(['/pessoa/lista']);
        } else {
          console.log('Erro ao cadastrar');
        }
      });

  }

  editar(){
    this.pessoaService.update(this.model)
      .subscribe(result => {
        this.route.navigate(['/pessoa/lista']);
      })
  }

  newHero() {
    this.model = new Pessoa();
  }

}
