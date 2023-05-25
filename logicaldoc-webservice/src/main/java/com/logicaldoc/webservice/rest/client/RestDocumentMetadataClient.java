package com.logicaldoc.webservice.rest.client;

import java.util.Arrays;

import org.apache.cxf.jaxrs.client.JAXRSClientFactory;
import org.apache.cxf.jaxrs.client.WebClient;
import org.apache.cxf.transport.http.HTTPConduit;
import org.apache.cxf.transports.http.configuration.HTTPClientPolicy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.jaxrs.json.JacksonJsonProvider;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSAttributeOption;
import com.logicaldoc.webservice.model.WSAttributeSet;
import com.logicaldoc.webservice.model.WSTemplate;
import com.logicaldoc.webservice.rest.DocumentMetadataService;

public class RestDocumentMetadataClient extends AbstractRestClient {

	protected static Logger log = LoggerFactory.getLogger(RestDocumentMetadataClient.class);

	private DocumentMetadataService proxy = null;

	public RestDocumentMetadataClient(String endpoint, String username, String password) {
		this(endpoint, username, password, -1);
	}

	public RestDocumentMetadataClient(String endpoint, String username, String password, int timeout) {
		super(endpoint, username, password, timeout);

		JacksonJsonProvider provider = new JacksonJsonProvider();

		if ((username == null) || (password == null)) {
			proxy = JAXRSClientFactory.create(endpoint, DocumentMetadataService.class, Arrays.asList(provider));
		} else {
			proxy = JAXRSClientFactory.create(endpoint, DocumentMetadataService.class, Arrays.asList(provider),
					username, password, null);
		}

		if (timeout > 0) {
			HTTPConduit conduit = WebClient.getConfig(proxy).getHttpConduit();
			HTTPClientPolicy policy = new HTTPClientPolicy();
			policy.setReceiveTimeout(timeout);
			conduit.setClient(policy);
		}
	}

	public void setAttributeOptions(long setId, String attribute, WSAttributeOption[] options)
			throws WebserviceException, PersistenceException {
		proxy.setAttributeOptions(setId, attribute, options);
	}

	public void setAttributeOptionsPOST(long setId, String attribute, WSAttributeOption[] options)
			throws WebserviceException, PersistenceException {
		proxy.setAttributeOptionsPOST(setId, attribute, options);
	}

	public long storeAttributeSet(WSAttributeSet attributeSet) throws WebserviceException, PersistenceException {
		return proxy.storeAttributeSet(attributeSet);
	}

	public long storeTemplate(WSTemplate template)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		return proxy.storeTemplate(template);
	}

	public WSAttributeSet getAttributeSetById(long setId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return proxy.getAttributeSetById(setId);
	}

	public WSAttributeSet getAttributeSet(String name)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return proxy.getAttributeSet(name);
	}

	public WSTemplate getTemplate(String name)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return proxy.getTemplate(name);
	}

	public WSTemplate getTemplateById(long templateId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return proxy.getTemplateById(templateId);
	}

	public String[] getAttributeOptions(long setId, String attribute)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return proxy.getAttributeOptions(setId, attribute);
	}

	public WSAttributeSet[] listAttributeSets()
			throws AuthenticationException, WebserviceException, PersistenceException {
		return proxy.listAttributeSets();
	}

	public void deleteAttributeSet(long setId) throws WebserviceException, PersistenceException {
		proxy.deleteAttributeSet(setId);
	}

	public void deleteTemplate(long templateId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		proxy.deleteTemplate(templateId);
	}

	public WSTemplate[] listTemplates() throws AuthenticationException, WebserviceException, PersistenceException {
		return proxy.listTemplates();
	}
}