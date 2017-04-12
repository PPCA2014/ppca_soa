var Login = Login || {};

Login.LoginSistemas = (function() {
	
	function LoginSistemas() {
		this.botaoLogin = $('#logar');
		this.username = $('#username');
		this.pass = $('#pass');
		this.error = $("#error");
	}
	
	LoginSistemas.prototype.iniciar = function() {
		this.botaoLogin.on('click', onBotaoLoginClick.bind(this));
	}
	
	function onBotaoLoginClick() {
		$.ajax({
			url: 'http://127.0.0.1:2301/authorize?response_type='+getRdirectUri()['response_type']+'&client_id='+getRdirectUri()['client_id']+'&state=xyz%20&redirect_uri='+getRdirectUri()['RedirectUri']+'&username='+this.username.val()+'&password='+this.pass.val(),
			method: 'GET',
			contentType: 'application/json',
			error: onErroSalvandoEstilo.bind(this),
			success: onEstiloSalvo.bind(this)
		});
	}
	
	//erro na autenticação
	function onErroSalvandoEstilo(obj) {
		this.error.append('<div class="alert alert-danger" role="alert">Usuário ou senha invalido(s).</div>');
	}
	
	//sucesso na autenticado
	function onEstiloSalvo(estilo) {
		console.log('Autenticado com sucesso');
	}
	
	function getRdirectUri(){
		var vars = [], hash;
		var hashes = window.location.href.slice(window.location.href.indexOf('?') + 1).split('&');
		for(var i = 0; i < hashes.length; i++)
		{
			hash = hashes[i].split('=');
			vars.push(hash[0]);
			vars[hash[0]] = hash[1];
		}
		return vars;
	}
	
	return LoginSistemas;
	
}());

$(function() {
	var loginSistemas = new Login.LoginSistemas();
	loginSistemas.iniciar();
});
