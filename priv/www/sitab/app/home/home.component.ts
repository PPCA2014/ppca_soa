import { Component, OnInit } from '@angular/core';


@Component({
  selector: 'app-home',
  templateUrl: 'app/home/home.component.html',
  styleUrls: ['app/home/home.component.css']
})
export class HomeComponent implements OnInit {

  constructor() { }

  ngOnInit() {

  }


  getMenuSession(){
    let verify = sessionStorage.getItem('menu');
    console.log(verify);
    let usuario = localStorage.getItem('currentUser');
    console.log(usuario);
  }



}
