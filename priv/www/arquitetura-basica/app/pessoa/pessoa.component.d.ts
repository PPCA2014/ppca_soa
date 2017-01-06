import { OnInit } from '@angular/core';
import { Pessoa } from "./pessoa.module";
import { PessoaService } from "./pessoa.service";
import { Router } from "@angular/router";
export declare class PessoaComponent implements OnInit {
    private pessoaService;
    private route;
    constructor(pessoaService: PessoaService, route: Router);
    ngOnInit(): void;
    model: Pessoa;
    hidden: boolean;
    onSubmit(): void;
    editar(): void;
    newHero(): void;
}
