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
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.rest.TagService;

public class RestTagClient extends AbstractRestClient {

	protected static Logger log = LoggerFactory.getLogger(RestTagClient.class);

	private TagService proxy = null;

	public RestTagClient(String endpoint, String username, String password) {
		this(endpoint, username, password, -1);
	}

	public RestTagClient(String endpoint, String username, String password, int timeout) {
		super(endpoint, username, password, timeout);

		JacksonJsonProvider provider = new JacksonJsonProvider();

		if ((username == null) || (password == null)) {
			proxy = JAXRSClientFactory.create(endpoint, TagService.class, Arrays.asList(provider));
		} else {
			proxy = JAXRSClientFactory.create(endpoint, TagService.class, Arrays.asList(provider), username, password,
					null);
		}

		if (timeout > 0) {
			HTTPConduit conduit = WebClient.getConfig(proxy).getHttpConduit();
			HTTPClientPolicy policy = new HTTPClientPolicy();
			policy.setReceiveTimeout(timeout);
			conduit.setClient(policy);
		}
	}

	public void addDocumentTags(long docId, String[] tags)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_FORM_URLENCODED);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.addDocumentTags(docId, tags);
	}

	public void setDocumentTags(long docId, String[] tags) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, UnexistingResourceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_FORM_URLENCODED);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.setDocumentTags(docId, tags);
	}

	public void addFolderTags(long folderId, String[] tags)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_FORM_URLENCODED);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.addFolderTags(folderId, tags);
	}

	public void setFolderTags(long folderId, String[] tags)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_FORM_URLENCODED);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.setFolderTags(folderId, tags);
	}

	public String[] getDocumentTags(long docId) throws PermissionException, AuthenticationException,
			PersistenceException, WebserviceException, UnexistingResourceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.getDocumentTags(docId);
	}

	public String[] getTags() throws AuthenticationException, PersistenceException, WebserviceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.getTags();
	}

	public WSDocument[] findDocumentsByTag(String tag)
			throws AuthenticationException, PersistenceException, WebserviceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.findDocumentsByTag(tag);
	}

	public WSFolder[] findFoldersByTag(String tag)
			throws AuthenticationException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.findFoldersByTag(tag);
	}
}