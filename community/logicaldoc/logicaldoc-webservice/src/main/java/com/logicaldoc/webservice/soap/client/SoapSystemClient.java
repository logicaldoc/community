package com.logicaldoc.webservice.soap.client;

import java.io.IOException;

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

	public SoapSystemClient(String endpoint) throws IOException {
		super(endpoint, SystemService.class, -1, true, -1);
	}

	@Override
	public WSParameter[] getStatistics(String sid) throws Exception {
		return client.getStatistics(sid);
	}

	@Override
	public String[] getLanguages(String tenantOrSid) throws Exception {
		return client.getLanguages(tenantOrSid);
	}

	@Override
	public WSSystemInfo getInfo() throws Exception {
		return client.getInfo();
	}
}