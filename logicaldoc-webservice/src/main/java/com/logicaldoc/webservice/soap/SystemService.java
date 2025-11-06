package com.logicaldoc.webservice.soap;

import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.doc.WSDoc;
import com.logicaldoc.webservice.model.WSParameter;
import com.logicaldoc.webservice.model.WSSystemInfo;

import jakarta.jws.WebMethod;
import jakarta.jws.WebParam;
import jakarta.jws.WebResult;
import jakarta.jws.WebService;

/**
 * System Web Service definition interface
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
@WSDoc(description = "informations about the system")
@WebService(name = "System", serviceName = "System", targetNamespace = "http://ws.logicaldoc.com")
public interface SystemService {

	/**
	 * Retrieves the Installation informations
	 * 
	 * @return The value object containing the installation informations
	 * 
	 * @throws WebserviceException Error in the webservice
	 */
	@WebResult(name = "info")
	@WebMethod(action = "getInfo")
	@WSDoc(description = "retrieves the Installation informations")
	public WSSystemInfo getInfo() throws WebserviceException;

	/**
	 * Retrieves the system statistics
	 * 
	 * @param sid Session identifier
	 * 
	 * @return List of stats
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebResult(name = "parameter")
	@WebMethod(action = "getStatistics")
	@WSDoc(description = "gets a set of statisticts of the system")
	public List<WSParameter> getStatistics(@WebParam(name = "sid")
	String sid) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Retrieves the statistics of a tenenat
	 * 
	 * @param sid Session identifier
	 * @param tenantId Identifier of the tenant
	 * 
	 * @return List of stats
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebResult(name = "parameter")
	@WebMethod(action = "getStatistics")
	@WSDoc(description = "gets a set of statisticts of the system")
	public List<WSParameter> getTenantStatistics(@WebParam(name = "sid")
	String sid, @WebParam(name = "tenantId")
	long tenantId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Retrieves the languages enabled in the server.
	 * 
	 * @return Array of active languages (en, it, es ....)
	 * 
	 * @param tenantOrSid Tenant name or session identifier (optional)
	 */
	@WebResult(name = "language")
	@WebMethod(action = "getLanguages")
	@WSDoc(description = "retrieves the languages enabled in the server")
	public List<String> getLanguages(@WSDoc(description = "a session's identifier or a tenant's name")
	@WebParam(name = "tenantOrSid")
	String tenantOrSid);
}