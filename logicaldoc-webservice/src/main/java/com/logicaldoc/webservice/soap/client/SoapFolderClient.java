package com.logicaldoc.webservice.soap.client;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSAccessControlEntry;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.soap.FolderService;

/**
 * Folder Web Service client.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class SoapFolderClient extends SoapClient<FolderService> implements FolderService {

	public SoapFolderClient(String endpoint, int gzipThreshold, boolean log, int timeout) {
		super(endpoint, FolderService.class, gzipThreshold, log, timeout);
	}

	public SoapFolderClient(String endpoint) {
		super(endpoint, FolderService.class, -1, true, -1);
	}

	@Override
	public WSFolder create(String sid, WSFolder folder)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		return client.create(sid, folder);
	}

	@Override
	public long createFolder(String sid, long parentId, String name)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		return client.createFolder(sid, parentId, name);
	}

	@Override
	public void delete(String sid, long folderId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		client.delete(sid, folderId);
	}

	@Override
	public WSFolder getFolder(String sid, long folderId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		return client.getFolder(sid, folderId);
	}

	@Override
	public boolean isReadable(String sid, long folderId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.isReadable(sid, folderId);
	}

	@Override
	public void move(String sid, long folderId, long parentId)
			throws AuthenticationException, PersistenceException, WebserviceException, PermissionException {
		client.move(sid, folderId, parentId);
	}

	@Override
	public void copy(String sid, long folderId, long parentId, int foldersOnly, String securityOption)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		client.copy(sid, folderId, parentId, foldersOnly, securityOption);
	}

	@Override
	public void rename(String sid, long folderId, String name)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		client.rename(sid, folderId, name);
	}

	@Override
	public List<WSFolder> listChildren(String sid, long folderId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		final List<WSFolder> folders = client.listChildren(sid, folderId);
		if (folders != null)
			return folders;
		else
			return new ArrayList<>();
	}
	
	
	@Override
	public List<WSFolder> list(String sid, long folderId, String sort, Integer page, Integer max)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		final List<WSFolder> folders = client.list(sid, folderId, sort, page, max);
		if (folders != null)
			return folders;
		else
			return new ArrayList<>();
	}

	@Override
	public WSFolder getRootFolder(String sid)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.getRootFolder(sid);
	}

	@Override
	public WSFolder getDefaultWorkspace(String sid)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		return client.getDefaultWorkspace(sid);
	}

	@Override
	public boolean isWritable(String sid, long folderId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.isWritable(sid, folderId);
	}

	@Override
	public List<WSFolder> getPath(String sid, long folderId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		final List<WSFolder> folders = client.getPath(sid, folderId);
		if (folders != null)
			return folders;
		else
			return new ArrayList<>();
	}

	@Override
	public void update(String sid, WSFolder folder)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		client.update(sid, folder);
	}

	@Override
	public WSFolder createPath(String sid, long parentId, String path)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		return client.createPath(sid, parentId, path);
	}

	@Override
	public List<WSFolder> listWorkspaces(String sid)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.listWorkspaces(sid);
	}

	@Override
	public WSFolder findByPath(String sid, String path)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		return client.findByPath(sid, path);
	}

	@Override
	public boolean isGranted(String sid, long folderId, String permission)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.isGranted(sid, folderId, permission);
	}

	@Override
	public WSFolder createAlias(String sid, long parentId, long foldRef)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		return client.createAlias(sid, parentId, foldRef);
	}

	@Override
	public void merge(String sid, long sourceId, long targetId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		client.merge(sid, sourceId, targetId);
	}

	@Override
	public List<WSAccessControlEntry> getAccessControlList(String sid, long folderId)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		return client.getAccessControlList(sid, folderId);
	}

	@Override
	public void setAccessControlList(String sid, long folderId, List<WSAccessControlEntry> acl)
			throws PersistenceException, PermissionException, AuthenticationException, WebserviceException {
		client.setAccessControlList(sid, folderId, acl);
	}
}