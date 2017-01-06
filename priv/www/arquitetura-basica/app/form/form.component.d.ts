import { OnInit } from '@angular/core';
import { Hero } from './hero';
export declare class FormComponent implements OnInit {
    constructor();
    ngOnInit(): void;
    powers: string[];
    model: Hero;
    submitted: boolean;
    onSubmit(): void;
    newHero(): void;
}
