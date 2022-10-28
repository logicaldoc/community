package com.logicaldoc.webservice.rest.endpoint;

import java.util.HashMap;

import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;

import org.apache.cxf.jaxrs.ext.MessageContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.webservice.rest.AuthService;
import com.logicaldoc.webservice.soap.endpoint.SoapAuthService;

import io.swagger.v3.oas.annotations.tags.Tag;

@Path("/")
@Tag(name = "author")
@Consumes({ MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public class RestAuthService extends SoapAuthService implements AuthService {

	private static Logger log = LoggerFactory.getLogger(RestAuthService.class);

	@Context
	public void setMessageContext(MessageContext messageContext) {
		// https://docs.oracle.com/cd/E13222_01/wls/docs92/webserv/annotations.html
		// https://jersey.java.net/documentation/latest/jaxrs-resources.html#d0e2790
		// https://jersey.java.net/apidocs-javax.jax-rs/2.0.1/javax/ws/rs/core/Context.html
		super.messageContext = messageContext;
	}

	@GET
    @Path("/login")
	public String login(@QueryParam("u") String username, @QueryParam("pw") String password) throws AuthenticationException {
		return super.login(username, password);
	}

	@POST
    @Path("/login")
	@Consumes(MediaType.APPLICATION_FORM_URLENCODED)
	public String loginPost(@FormParam("username") String username, @FormParam("password") String password)
			throws Exception {
		return super.login(username, password);
	}

	@POST
    @Path("/login")
	@Consumes(MediaType.APPLICATION_JSON)	
	public String loginPostJSON(String jsonstr) throws Exception {
		log.debug("loginPostJSON({})", jsonstr);

		ObjectMapper mapper = new ObjectMapper();
		TypeReference<HashMap<String, String>> typeRef = new TypeReference<HashMap<String, String>>() {
			// Nothing to do
		};
		HashMap<String, String> hm = mapper.readValue(jsonstr, typeRef);

		String username = hm.get("username");
		String password = hm.get("password");

		return super.login(username, password);
	}

	@DELETE
    @Path("/logout")
	public void logout(@QueryParam("sid") String sid) {
		log.debug("logout({})", sid);
		if (sid != null)
			super.logout(sid);
	}

	@GET
    @Path("/getSid")
	@Produces({ MediaType.APPLICATION_JSON, MediaType.TEXT_PLAIN })
	public String getSid() {
		return getCurrentSessionId();
	}
}
