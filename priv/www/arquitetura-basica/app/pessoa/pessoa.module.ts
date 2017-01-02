export class Pessoa {
  constructor( ){ }

  public nome: string;
  public id: number;


  fromJSON(json:Pessoa) {
    for (var propName in json)
      this[propName] = json[propName];
    return this;
  }

}

