package com.logicaldoc.webservice.soap;

import jakarta.jws.WebMethod;
import jakarta.jws.WebParam;
import jakarta.jws.WebService;

import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.webservice.doc.WSDoc;

/**
 * Auth Web Service definition interface
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
@WSDoc(description = "authentication operations")
@WebService(name = "Auth", serviceName = "Auth", targetNamespace = "http://ws.logicaldoc.com")
public interface AuthService {
	/**
	 *  Starts a new session.
	 * 
	 * @param username The username
	 * @param password The password
	 * 
	 * @return The newly created session identifier(sid)
	 * 
	 * @throws AuthenticationException the user was not authenticated
	 */
	@WebMethod(action = "login")
	@WSDoc(description = "deprecated, use loginApiKey instead.")
	public String login(@WebParam(name = "username")
	String username, @WebParam(name = "password")
	String password) throws AuthenticationException;

	/**
	 * Starts a new session.
	 * 
	 * @param apiKey The API Key
	 * 
	 * @return The newly created session identifier(sid)
	 * 
	 * @throws AuthenticationException the user was not authenticated
	 */
	@WebMethod(action = "loginApiKey")
	@WSDoc(description = "starts a new session; returns the session identifier(SID)")
	public String loginApiKey(@WebParam(name = "apiKey")
	String apiKey) throws AuthenticationException;

	/**
	 * Closes a session.
	 * 
	 * @param sid identifier of the session or an API Key
	 */
	@WebMethod(action = "logout")
	@WSDoc(description = "closes a session")
	public void logout(@WSDoc(description = "the session identifier")
	@WebParam(name = "sid")
	String sid);

	/**
	 * Checks if a SID is valid
	 * 
	 * @param sid identifier of the session or an API Key
	 * 
	 * @return if the session is valid
	 */
	@WebMethod(action = "valid")
	@WSDoc(description = "checks if a SID is valid; returns true only if the session is valid")
	public boolean valid(@WSDoc(description = "the session identifier")
	@WebParam(name = "sid")
	String sid);

	/**
	 * Renews a session
	 * 
	 * @param sid identifier of the session or an API Key
	 */
	@WebMethod(action = "renew")
	@WSDoc(description = "renews an existing session")
	public void renew(@WSDoc(description = "the session identifier")
	@WebParam(name = "sid")
	String sid);
}