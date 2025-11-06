package com.logicaldoc.webservice.rest.client;

import java.util.List;

import org.apache.cxf.jaxrs.client.WebClient;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.rest.FolderService;

import jakarta.ws.rs.core.MediaType;

public class RestFolderClient extends AbstractRestClient<FolderService> {

	public RestFolderClient(String endpoint, String apiKey) {
		this(endpoint, apiKey, -1);
	}

	public RestFolderClient(String endpoint, String apiKey, int timeout) {
		super(FolderService.class, endpoint, apiKey, timeout);
	}

	public List<WSFolder> listChildren(long folderId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.listChildren(folderId);
	}

	public List<WSFolder> list(long folderId, String sort, Integer page, Integer max)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.list(folderId, sort, page, max);
	}

	public WSFolder create(WSFolder folder)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.create(folder);
	}

	public WSFolder createPath(long rootFolder, String path)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_FORM_URLENCODED);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.createPath(rootFolder, path);
	}

	public WSFolder findByPath(String path)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.findByPath(path);
	}

	public WSFolder getRootFolder() throws AuthenticationException, WebserviceException, PersistenceException {
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.getRootFolder();
	}

	public WSFolder getFolder(long folderId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.getFolder(folderId);
	}

	public long createFolder(long parentId, String folderName)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).accept(MediaType.TEXT_PLAIN);
		return proxy.createFolder(parentId, folderName);
	}

	public void delete(long folderId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.delete(folderId);
	}

	public void update(WSFolder folder)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.update(folder);
	}
}