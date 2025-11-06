package com.logicaldoc.webservice.rest.endpoint;

import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSParameter;
import com.logicaldoc.webservice.model.WSSystemInfo;
import com.logicaldoc.webservice.rest.SystemService;
import com.logicaldoc.webservice.soap.endpoint.SoapSystemService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.MediaType;

@Path("/")
@Tag(name = "system")
@Consumes({ MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public class RestSystemService extends SoapSystemService implements SystemService {

	@Override
	@GET
	@Path("/getInfo")
	@Operation(summary = "Get system information", description = "Retrieves information about the Installation")
	public WSSystemInfo getInfo() throws WebserviceException {
		return super.getInfo();
	}

	@Override
	@GET
	@Path("/getStatistics")
	@Operation(summary = "Get system statistics", description = "Retrieves the system statistics")
	public List<WSParameter> getStatistics() throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.getStatistics(sid);
	}

	@Override
	@GET
	@Path("/getTenantStatistics")
	@Operation(summary = "Get tenant statistics", description = "Retrieves the statistics of a tenant")
	public List<WSParameter> getTenantStatistics(@QueryParam("tenantId")
	long tenantId) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.getTenantStatistics(sid, tenantId);
	}

	@Override
	@GET
	@Path("/getLanguages")
	@Operation(summary = "Get enabled languages", description = "Retrieves the languages enabled in the server")
	public List<String> getLanguages() {
		String sid = validateSessionREST();
		return super.getLanguages(sid);
	}
}
