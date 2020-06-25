package com.logicaldoc.webservice.soap;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebService;

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
	 * Starts a new session.
	 * 
	 * @param username The username
	 * @param password The password
	 * 
	 * @return The newly created session identifier(sid)
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WSDoc(description = "starts a new session; returns the session identifier(SID)")
	public String login(@WebParam(name = "username") String username, @WebParam(name = "password") String password)
			throws Exception;

	/**
	 * Closes a session.
	 * 
	 * @param sid The session identifier
	 */
	@WebMethod
	@WSDoc(description = "closes a session")
	public void logout(@WSDoc(description = "the session identifier") @WebParam(name = "sid") String sid);

	/**
	 * Checks if a SID is valid
	 * 
	 * @param sid identifier of the session
	 * 
	 * @return if the session is valid
	 */
	@WebMethod
	@WSDoc(description = "checks if a SID is valid; returns true only if the session is valid")
	public boolean valid(@WSDoc(description = "the session identifier") @WebParam(name = "sid") String sid);

	/**
	 * Renews a session
	 * 
	 * @param sid identifier of the session
	 */
	@WebMethod
	@WSDoc(description = "renews an existing session")
	public void renew(@WSDoc(description = "the session identifier") @WebParam(name = "sid") String sid);
}