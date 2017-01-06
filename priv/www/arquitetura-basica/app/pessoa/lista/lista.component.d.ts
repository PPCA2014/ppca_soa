import { OnInit } from '@angular/core';
import { Pessoa } from "../pessoa.module";
import { PessoaService } from "../pessoa.service";
import { Router } from "@angular/router";
export declare class ListaComponent implements OnInit {
    private pessoaService;
    private route;
    pessoas: Pessoa[];
    pessoa: Pessoa;
    constructor(pessoaService: PessoaService, route: Router);
    ngOnInit(): void;
    setPessoa(pessoa: Pessoa): void;
    editar(pessoa: Pessoa): void;
    deletar(): void;
}
