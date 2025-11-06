package com.logicaldoc.webservice.rest.endpoint;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.webservice.model.WSCredentials;
import com.logicaldoc.webservice.rest.AuthService;
import com.logicaldoc.webservice.soap.endpoint.SoapAuthService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
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

@Path("/")
@Tag(name = "authorize")
@Consumes({ MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public class RestAuthService extends SoapAuthService implements AuthService {

	private static final Logger log = LoggerFactory.getLogger(RestAuthService.class);

	@GET
	@Path("/login")
	@Override
	public String login(@QueryParam("u") String username, @QueryParam("pw") String password) 
			throws AuthenticationException {
		return super.login(username, password);
	}

	@POST
	@Path("/loginForm")
	@Operation(operationId = "loginForm", summary = "Login with POST", description = "Deprecated, use loginApiKey instead")
	@Consumes(MediaType.APPLICATION_FORM_URLENCODED)
	@Override
	public String loginForm(@FormParam("username") String username, @FormParam("password") String password) {
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
	@Operation(summary = "Login by API Key", description = "Performs login by passing the API Key in the header. Read more at https://docs.logicaldoc.com/en/web-services-api/api-keys")
	@ApiResponses(value = { 
			@ApiResponse(responseCode = "200", description = "Successful operation. The Session ID (sid) is returned.", content = @Content(schema = @Schema(implementation = String.class, description = "The session ID (sid) created", example = "ca3c411d-b043-49a3-b151-e363dddcecef"))),
			@ApiResponse(responseCode = "500", description = "Authentication failed")})
	@Override
	public String loginApiKey(@HeaderParam("X-API-KEY") String apikey) {
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