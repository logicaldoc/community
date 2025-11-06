package com.logicaldoc.webservice.rest;

import com.logicaldoc.webservice.model.WSCredentials;

import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.DELETE;
import jakarta.ws.rs.FormParam;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.HeaderParam;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.MediaType;

/**
 * Auth Web Service definition interface for Rest Services
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 7.5
 */
@Consumes({ MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public interface AuthService {

	@GET
    @Path("/login")
	public String login(@QueryParam("u") String username, @QueryParam("pw") String password);
	
	@GET
    @Path("/loginApiKey")
	public String loginApiKey(@HeaderParam("X-API-KEY") String apiKey);

	@POST
    @Path("/loginForm")
	@Consumes(MediaType.APPLICATION_FORM_URLENCODED)
	public String loginForm(@FormParam("username") String username, @FormParam("password") String password);

	@POST
    @Path("/login")
	@Consumes(MediaType.APPLICATION_JSON)
	public String loginPostJSON(WSCredentials wscred);	

	@DELETE
    @Path("/logout")
	public void logout(@QueryParam("sid") String sid);
	
	@GET
    @Path("/getSid")
	@Produces({ MediaType.APPLICATION_JSON, MediaType.TEXT_PLAIN })
	public String getSid();	
}