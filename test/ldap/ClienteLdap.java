package br.unb.web.clientldap;

import java.util.Hashtable;

import javax.ejb.EJB;
import javax.ejb.Stateless;
import javax.naming.*;
import javax.naming.directory.*;
import javax.naming.ldap.InitialLdapContext;
import javax.naming.ldap.LdapContext;

import br.unb.web.properties.ArquivoPropriedades;

@Stateless
public class ClienteLdap {

	@EJB
	private ArquivoPropriedades arquivoPropriedades;
	
	public String validarEmailSenha(String email, String senha) throws NamingException {
		Hashtable env = new Hashtable(11);
		env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
		env.put(Context.PROVIDER_URL, "ldap://"+arquivoPropriedades.verificarAmbiente()+"/");
		env.put(Context.SECURITY_AUTHENTICATION, "simple");
		env.put(Context.SECURITY_PRINCIPAL, "cn=admin,dc=unb,dc=br");
		env.put(Context.SECURITY_CREDENTIALS, senha);
		env.put(Context.SECURITY_PRINCIPAL, "uid="+email);
		
		LdapContext ctx = new InitialLdapContext(env, null);
	   
	    NamingEnumeration answer = ctx.search("", "objectClass=*", null);

	    System.out.println(ctx.getResponseControls());
	    
	    ctx.close();
	    return ctx.toString();
	
	}
	
	public static void main(String[] args) {
		ClienteLdap ldap = new ClienteLdap();
		try {
			ldap.validarEmailSenha("geral","123456");
		} catch (NamingException e) {
			e.printStackTrace();
		}
	}
	
}
