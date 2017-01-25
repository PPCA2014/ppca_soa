describe("academico.aluno", function() {
 
 beforeEach(function() {

 });

 
 it("Verifica se consegue obter uma lista de alunos", function() {
	var result = $.ajax({
					url:  "http://localhost:2301/academico/aluno",
					data : {},
					type: "GET",
					contentType: "application/json",
					dataType: "json",
					crossDomain: true,
					async: false
				});
	expect(result.status).toBe(200);
	if (result.status == 200){
		expect(result.responseJSON).toEqual(jasmine.any(Object));
	}
 });



 
});
