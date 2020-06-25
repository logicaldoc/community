package com.logicaldoc.webservice.rest.endpoint;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.webservice.model.WSParameter;
import com.logicaldoc.webservice.model.WSSystemInfo;
import com.logicaldoc.webservice.rest.SystemService;
import com.logicaldoc.webservice.soap.endpoint.SoapSystemService;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

@Path("/")
@Api(value = "system")
@Consumes({ MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public class RestSystemService extends SoapSystemService implements SystemService {

	protected static Logger log = LoggerFactory.getLogger(RestSystemService.class);

	@Override
	@GET
	@Path("/getInfo")
	@ApiOperation(value = "Get system information", notes = "Retrieves information about the Installation")
	public WSSystemInfo getInfo() throws Exception {
		return super.getInfo();
	}

	@Override
	@GET
	@Path("/getStatistics")
	@ApiOperation(value = "Get system statistics", notes ="Retrieves the system statistics")
	public WSParameter[] getStatistics() throws Exception {
		String sid = validateSession();
		return super.getStatistics(sid);
	}

	@Override
	@GET
	@Path("/getLanguages")
	@ApiOperation(value = "Get enabled languages", notes = "Retrieves the languages enabled in the server")
	public String[] getLanguages() throws Exception {
		String sid = validateSession();
		return super.getLanguages(sid);
	}
}
