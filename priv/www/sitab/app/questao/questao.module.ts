export class Questao {

  public id: number;
  public titulo: string;
  public conteduto: string;
  public idPessoa: number;

  constructor( ){ }


  fromJSON(json:Questao) {
    for (var propName in json)
      this[propName] = json[propName];
    return this;
  }

}

