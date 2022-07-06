package com.logicaldoc.webservice.soap;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;

import com.logicaldoc.webservice.doc.WSDoc;
import com.logicaldoc.webservice.model.WSParameter;
import com.logicaldoc.webservice.model.WSSystemInfo;

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
	 * @throws Exception error in the server application
	 */
	@WebResult(name = "info")
	@WebMethod
	@WSDoc(description = "retrieves the Installation informations")
	public WSSystemInfo getInfo() throws Exception;

	/**
	 * Retrieves the system statistics
	 * 
	 * @param sid Session identifier
	 * 
	 * @return The value object containing the statistics values
	 * 
	 * @throws Exception error in the server application
	 */
	@WebResult(name = "parameter")
	@WebMethod
	@WSDoc(description = "gets a set of statisticts of the system")
	public WSParameter[] getStatistics(@WebParam(name = "sid") String sid) throws Exception;

	/**
	 * Retrieves the languages enabled in the server.
	 * 
	 * @return Array of active languages (en, it, es ....)
	 * 
	 * @param tenantOrSid Tenant name or session identifier (optional)
	 * 
	 * @throws Exception error in the server application
	 */
	@WebResult(name = "language")
	@WSDoc(description = "retrieves the languages enabled in the server")
	public String[] getLanguages(
			@WSDoc(description = "a session's identifier or a tenant's name") @WebParam(name = "tenantOrSid") String tenantOrSid)
			throws Exception;
}