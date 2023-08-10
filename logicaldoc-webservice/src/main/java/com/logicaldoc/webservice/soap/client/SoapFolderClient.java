package com.logicaldoc.webservice.soap.client;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSRight;
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
			throws AuthenticationException, PersistenceException, WebserviceException {
		client.move(sid, folderId, parentId);
	}

	@Override
	public void copy(String sid, long folderId, long parentId, int foldersOnly, String securityOption)
			throws AuthenticationException, WebserviceException, PersistenceException {
		client.copy(sid, folderId, parentId, foldersOnly, securityOption);
	}

	@Override
	public void rename(String sid, long folderId, String name)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		client.rename(sid, folderId, name);
	}

	@Override
	public WSFolder[] listChildren(String sid, long folderId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		return client.listChildren(sid, folderId);
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
	public WSFolder[] getPath(String sid, long folderId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		return client.getPath(sid, folderId);
	}

	@Override
	public WSRight[] getGrantedGroups(String sid, long folderId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.getGrantedGroups(sid, folderId);
	}

	@Override
	public WSRight[] getGrantedUsers(String sid, long folderId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.getGrantedUsers(sid, folderId);
	}

	@Override
	public void grantGroup(String sid, long folderId, long groupId, int permissions, boolean recursive)
			throws PermissionException, AuthenticationException, PersistenceException, WebserviceException {
		client.grantGroup(sid, folderId, groupId, permissions, recursive);
	}

	@Override
	public void grantUser(String sid, long folderId, long userId, int permissions, boolean recursive)
			throws PermissionException, AuthenticationException, PersistenceException, WebserviceException {
		client.grantUser(sid, folderId, userId, permissions, recursive);
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
	public WSFolder[] listWorkspaces(String sid)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.listWorkspaces(sid);
	}

	@Override
	public WSFolder findByPath(String sid, String path)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		return client.findByPath(sid, path);
	}

	@Override
	public boolean isGranted(String sid, long folderId, int permission)
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
}