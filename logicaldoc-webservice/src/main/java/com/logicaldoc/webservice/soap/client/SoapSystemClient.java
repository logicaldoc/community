package com.logicaldoc.webservice.soap.client;

import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSParameter;
import com.logicaldoc.webservice.model.WSSystemInfo;
import com.logicaldoc.webservice.soap.SystemService;

/**
 * System Web Service client.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class SoapSystemClient extends SoapClient<SystemService> implements SystemService {

	public SoapSystemClient(String endpoint) {
		super(endpoint, SystemService.class, -1, true, -1);
	}

	@Override
	public List<WSParameter> getStatistics(String sid)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.getStatistics(sid);
	}

	@Override
	public List<String> getLanguages(String tenantOrSid) {
		return client.getLanguages(tenantOrSid);
	}

	@Override
	public WSSystemInfo getInfo() throws WebserviceException {
		return client.getInfo();
	}
}