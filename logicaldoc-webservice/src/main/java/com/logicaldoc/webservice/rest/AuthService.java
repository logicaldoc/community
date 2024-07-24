package com.logicaldoc.webservice.rest;

import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import com.logicaldoc.webservice.model.WSCredentials;

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