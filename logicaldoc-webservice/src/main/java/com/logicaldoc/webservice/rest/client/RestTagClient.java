package com.logicaldoc.webservice.rest.client;

import java.util.List;

import javax.ws.rs.core.MediaType;

import org.apache.cxf.jaxrs.client.WebClient;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.rest.TagService;

public class RestTagClient extends AbstractRestClient<TagService> {

	public RestTagClient(String endpoint, String apiKey) {
		this(endpoint, apiKey, -1);
	}

	public RestTagClient(String endpoint, String apiKey, int timeout) {
		super(TagService.class, endpoint, apiKey, timeout);
	}

	public void addDocumentTags(long docId, List<String> tags) throws AuthenticationException, PermissionException,
			PersistenceException, UnexistingResourceException, WebserviceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_FORM_URLENCODED);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.addDocumentTags(docId, tags);
	}

	public void setDocumentTags(long docId, List<String> tags) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, UnexistingResourceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_FORM_URLENCODED);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.setDocumentTags(docId, tags);
	}

	public void addFolderTags(long folderId, List<String> tags)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_FORM_URLENCODED);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.addFolderTags(folderId, tags);
	}

	public void setFolderTags(long folderId, List<String> tags)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_FORM_URLENCODED);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.setFolderTags(folderId, tags);
	}

	public List<String> getDocumentTags(long docId) throws PermissionException, AuthenticationException,
			PersistenceException, WebserviceException, UnexistingResourceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.getDocumentTags(docId);
	}

	public List<String> getTags() throws AuthenticationException, PersistenceException, WebserviceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.getTags();
	}

	public List<WSDocument> findDocumentsByTag(String tag)
			throws AuthenticationException, PersistenceException, WebserviceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.findDocumentsByTag(tag);
	}

	public List<WSFolder> findFoldersByTag(String tag)
			throws AuthenticationException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.findFoldersByTag(tag);
	}
}