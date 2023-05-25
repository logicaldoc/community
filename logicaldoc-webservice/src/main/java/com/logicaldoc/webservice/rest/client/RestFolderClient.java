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
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.rest.FolderService;

public class RestFolderClient extends AbstractRestClient {

	protected static Logger log = LoggerFactory.getLogger(RestFolderClient.class);

	private FolderService proxy = null;

	public RestFolderClient(String endpoint, String username, String password) {
		this(endpoint, username, password, -1);
	}

	public RestFolderClient(String endpoint, String username, String password, int timeout) {
		super(endpoint, username, password, timeout);

		JacksonJsonProvider provider = new JacksonJsonProvider();

		if ((username == null) || (password == null)) {
			proxy = JAXRSClientFactory.create(endpoint, FolderService.class, Arrays.asList(provider));
		} else {
			proxy = JAXRSClientFactory.create(endpoint, FolderService.class, Arrays.asList(provider), username,
					password, null);
		}

		if (timeout > 0) {
			HTTPConduit conduit = WebClient.getConfig(proxy).getHttpConduit();
			HTTPClientPolicy policy = new HTTPClientPolicy();
			policy.setReceiveTimeout(timeout);
			conduit.setClient(policy);
		}
	}

	public WSFolder[] listChildren(long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.listChildren(folderId);
	}

	public WSFolder create(WSFolder folder) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.create(folder);
	}

	public WSFolder createPath(long rootFolder, String path) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_FORM_URLENCODED);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.createPath(rootFolder, path);
	}

	public WSFolder findByPath(String path) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.findByPath(path);
	}

	public WSFolder getRootFolder() throws AuthenticationException, WebserviceException, PersistenceException {
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.getRootFolder();
	}

	public WSFolder getFolder(long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.getFolder(folderId);
	}

	public long createFolder(long parentId, String folderName) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).accept(MediaType.TEXT_PLAIN);
		return proxy.createFolder(parentId, folderName);
	}

	public void delete(long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.delete(folderId);
	}

	public void update(WSFolder folder) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.update(folder);
	}
}