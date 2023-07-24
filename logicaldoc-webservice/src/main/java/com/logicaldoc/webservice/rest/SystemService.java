package com.logicaldoc.webservice.rest;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSParameter;
import com.logicaldoc.webservice.model.WSSystemInfo;

/**
 * System Web Service definition interface for REST.
 * 
 * Alessandro Gasparini - LogicalDOC
 * 
 * @since 8.4.2
 */
@Consumes({ MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public interface SystemService {
	

	/**
	 * Retrieves the Installation informations
	 * 
	 * @return The value object containing the installation informations
	 * 
	 * @throws WebserviceException Error in the webservice
	 */
	@GET
    @Path("/getInfo")	
	public WSSystemInfo getInfo() throws WebserviceException;

	/**
	 * Retrieves the system statistics
	 * 
	 * @return The value object containing the statistics values
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@GET
    @Path("/getStatistics")		
	public WSParameter[] getStatistics() throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Retrieves the languages enabled in the server.
	 * 
	 * @return Array of active languages (en, it, es ....)
	 */
	@GET
	@Path("/getLanguages")	
	public String[] getLanguages();	
	
}