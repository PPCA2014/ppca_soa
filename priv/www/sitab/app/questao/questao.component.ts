import { Component, OnInit } from '@angular/core';
import { Router } from "@angular/router";
import {Questao} from "./questao.module";
import {QuestaoService} from "./questao.service";

@Component({
  selector: 'app-questao',
  templateUrl: 'app/questao/questao.component.html',
  styleUrls: ['app/questao/questao.component.css']
})
export class QuestaoComponent implements OnInit {

  questao: Questao;
  hidden:boolean = true;
  constructor(private questaoService:QuestaoService,private route: Router) { }



  ngOnInit() {
    this.questao = this.questaoService.questao;
    if(this.questao == null){
      this.questao = new Questao();
    }else {
      this.hidden = false;
    }
    this.questaoService.questao = null;
  }

  onSubmit(){
    this.questaoService.insert(this.questao)
      .subscribe(result => {
        this.route.navigate(['questao/lista']);
      });
  }

}
