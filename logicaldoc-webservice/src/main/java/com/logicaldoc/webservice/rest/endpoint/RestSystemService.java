package com.logicaldoc.webservice.rest.endpoint;

import java.util.List;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSParameter;
import com.logicaldoc.webservice.model.WSSystemInfo;
import com.logicaldoc.webservice.rest.SystemService;
import com.logicaldoc.webservice.soap.endpoint.SoapSystemService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

@Path("/")
@Tag(name = "system")
@Consumes({ MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public class RestSystemService extends SoapSystemService implements SystemService {

	protected static Logger log = LoggerFactory.getLogger(RestSystemService.class);

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
	@Path("/getLanguages")
	@Operation(summary = "Get enabled languages", description = "Retrieves the languages enabled in the server")
	public List<String> getLanguages() {
		String sid = validateSessionREST();
		return super.getLanguages(sid);
	}
}
