package com.logicaldoc.webservice.rest;

import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSParameter;
import com.logicaldoc.webservice.model.WSSystemInfo;

import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.MediaType;

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
	 * @return List of stats
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@GET
	@Path("/getStatistics")
	public List<WSParameter> getStatistics() throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Retrieves the statistics of a tenant
	 * 
	 * @param tenantId identifier of the tenant
	 * 
	 * @return List of stats
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@GET
	@Path("/getTenantStatistics")
	public List<WSParameter> getTenantStatistics(@QueryParam("tenantId")
	long tenantId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Retrieves the languages enabled in the server.
	 * 
	 * @return Array of active languages (en, it, es ....)
	 */
	@GET
	@Path("/getLanguages")
	public List<String> getLanguages();

}