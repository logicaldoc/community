package com.logicaldoc.webservice.rest.client;

import java.util.Arrays;

import javax.ws.rs.core.MediaType;

import org.apache.cxf.jaxrs.client.JAXRSClientFactory;
import org.apache.cxf.jaxrs.client.WebClient;
import org.apache.cxf.transport.http.HTTPConduit;
import org.apache.cxf.transports.http.configuration.HTTPClientPolicy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.jaxrs.json.JacksonJsonProvider;
import com.logicaldoc.webservice.model.WSSearchOptions;
import com.logicaldoc.webservice.model.WSSearchResult;
import com.logicaldoc.webservice.rest.SearchService;

public class RestSearchClient extends AbstractRestClient {

	protected static Logger log = LoggerFactory.getLogger(RestSearchClient.class);

	private SearchService proxy = null;

	public RestSearchClient(String endpoint, String username, String password) {
		this(endpoint, username, password, -1);
	}

	/**
	 * Constructor
	 * 
	 * @param endpoint  Connection URL
	 * @param username the username
	 * @param password the password
	 * @param timeout Timeout for the RESTful requests
	 */
	public RestSearchClient(String endpoint, String username, String password, int timeout) {
		super(endpoint, username, password, timeout);

		JacksonJsonProvider provider = new JacksonJsonProvider();
		
		if ((username == null) || (password == null)) {
			proxy = JAXRSClientFactory.create(endpoint, SearchService.class, Arrays.asList(provider));
		} else {
			proxy = JAXRSClientFactory.create(endpoint, SearchService.class, Arrays.asList(provider), username, password, null);
		}
		
		if (timeout > 0) {
			HTTPConduit conduit = WebClient.getConfig(proxy).getHttpConduit();
			HTTPClientPolicy policy = new HTTPClientPolicy();
			policy.setReceiveTimeout(timeout);
			conduit.setClient(policy);
		}
	}	
	
	public WSSearchResult find(WSSearchOptions owd) throws Exception {	
		
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);		

		return proxy.find(owd);
	}
}