package com.logicaldoc.webservice.rest.client;

import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.apache.cxf.jaxrs.client.JAXRSClientFactoryBean;
import org.apache.cxf.jaxrs.client.WebClient;
import org.apache.cxf.transport.http.HTTPConduit;
import org.apache.cxf.transports.http.configuration.HTTPClientPolicy;

import com.fasterxml.jackson.jaxrs.json.JacksonJsonProvider;

/**
 * Parent for all RESTful clients
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.9
 * 
 * @param <T> The web service class
 */
public abstract class AbstractRestClient<T> {

	protected String endpoint;

	protected String apiKey;

	protected int timeout;

	protected T proxy;

	/**
	 * Constructor
	 * 
	 * @param stub The interface representing the service
	 * @param endpoint Connection URL
	 * @param apiKey the API Key
	 */
	protected AbstractRestClient(Class<T> stub, String endpoint, String apiKey) {
		this.endpoint = endpoint;
		this.apiKey = apiKey;
		prepareProxy(stub);
	}

	/**
	 * Constructor
	 * 
	 * @param stub The interface representing the service
	 * @param endpoint Connection URL
	 * @param apiKey the API Key
	 * @param timeout Timeout for the RESTful requests
	 */
	protected AbstractRestClient(Class<T> stub, String endpoint, String apiKey, int timeout) {
		this(stub, endpoint, apiKey);
		this.timeout = timeout;
		if (timeout > 0) {
			HTTPConduit conduit = WebClient.getConfig(proxy).getHttpConduit();
			HTTPClientPolicy policy = new HTTPClientPolicy();
			policy.setReceiveTimeout(timeout);
			conduit.setClient(policy);
		}
	}

	protected void prepareProxy(Class<T> stub) {
		JAXRSClientFactoryBean bean = new JAXRSClientFactoryBean();

		if (StringUtils.isNotEmpty(apiKey)) {
			bean.setHeaders(Map.of("X-API-KEY", apiKey));
			bean.setInheritHeaders(true);
		}

		bean.setResourceClass(stub);
		bean.setAddress(endpoint);
		bean.setProviders(List.of(new JacksonJsonProvider()));
		proxy = bean.create(stub);
	}

}