package com.logicaldoc.webservice.rest.endpoint;

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

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.webservice.model.WSCredentials;
import com.logicaldoc.webservice.rest.AuthService;
import com.logicaldoc.webservice.soap.endpoint.SoapAuthService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

@Path("/")
@Tag(name = "authorize")
@Consumes({ MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public class RestAuthService extends SoapAuthService implements AuthService {

	private static Logger log = LoggerFactory.getLogger(RestAuthService.class);

	@GET
	@Path("/login")
	@Override
	public String login(@QueryParam("u")
	String username, @QueryParam("pw")
	String password) throws AuthenticationException {
		return super.login(username, password);
	}
	
	@POST
	@Path("/loginForm")
	@Operation(operationId = "loginForm", summary = "Login with POST", description = "Deprecated, use loginApiKey instead")
	@Consumes(MediaType.APPLICATION_FORM_URLENCODED)
	@Override
	public String loginForm(@FormParam("username")
	String username, @FormParam("password")
	String password) {
		return super.login(username, password);
	}

	@POST
	@Path("/login")
	@Operation(operationId = "loginPostJSON", summary = "Login with POST in JSON format", description = "Deprecated, use loginApiKey instead")
	@Consumes(MediaType.APPLICATION_JSON)
	@Override
	public String loginPostJSON(WSCredentials cred) {
		return super.login(cred.getUsername(), cred.getPassword());
	}
	
	@GET
	@Path("/loginApiKey")
	@Override
	public String loginApiKey(@HeaderParam("X-API-KEY")
	String apikey) {
		// The header was already processed by the SessionFilter so we must
		// check the existing session first
		String sid = SessionManager.get().getSessionId(getCurrentRequest());
		if (StringUtils.isEmpty(sid))
			return super.loginApiKey(apikey);
		else
			return sid;
	}

	@DELETE
	@Path("/logout")
	@Override
	public void logout(@QueryParam("sid")
	String sid) {
		log.debug("logout({})", sid);
		if (sid != null)
			super.logout(sid);
	}

	@GET
	@Path("/getSid")
	@Produces({ MediaType.APPLICATION_JSON, MediaType.TEXT_PLAIN })
	@Override
	public String getSid() {
		return getCurrentSessionId();
	}

	/**
	 * Renews a session
	 * 
	 * @param sid identifier of the session
	 */
	@GET
	@Path("/renew")
	@Override
	public void renew(@QueryParam("sid")
	String sid) {
		super.renew(sid);
	}

	/**
	 * Renews the current session
	 */
	@GET
	@Path("/renewcurrent")
	public void renew() {
		super.renew(getSid());
	}
}