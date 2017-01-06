import { OnInit } from '@angular/core';
import { Router } from "@angular/router";
import { Questao } from "./questao.module";
import { QuestaoService } from "./questao.service";
import { PessoaService } from "../pessoa/pessoa.service";
import { Pessoa } from "../pessoa/pessoa.module";
export declare class QuestaoComponent implements OnInit {
    private questaoService;
    private pessoaService;
    private route;
    questao: Questao;
    hidden: boolean;
    constructor(questaoService: QuestaoService, pessoaService: PessoaService, route: Router);
    listaPessoa: Pessoa[];
    ngOnInit(): void;
    onSubmit(): void;
}
