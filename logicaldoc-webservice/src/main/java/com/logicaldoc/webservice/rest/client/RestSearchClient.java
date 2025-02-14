package com.logicaldoc.webservice.rest.client;

import javax.ws.rs.core.MediaType;

import org.apache.cxf.jaxrs.client.WebClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.searchengine.SearchException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSSearchOptions;
import com.logicaldoc.webservice.model.WSSearchResult;
import com.logicaldoc.webservice.rest.SearchService;

public class RestSearchClient extends AbstractRestClient<SearchService> {

	private static final Logger log = LoggerFactory.getLogger(RestSearchClient.class);

	public RestSearchClient(String endpoint, String apiKey) {
		this(endpoint, apiKey, -1);
	}

	public RestSearchClient(String endpoint, String apiKey, int timeout) {
		super(SearchService.class, endpoint, apiKey, timeout);
	}

	public WSSearchResult find(WSSearchOptions owd)
			throws AuthenticationException, PersistenceException, WebserviceException, SearchException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.find(owd);
	}
}