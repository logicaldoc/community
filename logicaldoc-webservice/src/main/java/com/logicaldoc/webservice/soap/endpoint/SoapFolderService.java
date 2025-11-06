package com.logicaldoc.webservice.soap.endpoint;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderComparator;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderEvent;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.security.AccessControlEntry;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.webservice.AbstractService;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSAccessControlEntry;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSUtil;
import com.logicaldoc.webservice.soap.FolderService;

/**
 * Folder Web Service Implementation
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
public class SoapFolderService extends AbstractService implements FolderService {

	private static final String FOLDER = "Folder ";

	private static final String NOT_ALLOWED = "Not Allowed";

	private static final String NO_CHANGES = "No Changes";

	private static final String CANNOT_MOVE_FOLDERS_IN_THE_ROOT = "Cannot move folders in the root";

	private static final Logger log = LoggerFactory.getLogger(SoapFolderService.class);

	@Override
	public WSFolder create(String sid, WSFolder wsFolder)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);

		FolderDAO folderDao = FolderDAO.get();
		Folder parentFolder = folderDao.findById(wsFolder.getParentId());
		if (parentFolder == null)
			throw new WebserviceException(
					String.format("A parent folder with id %s was not found.", wsFolder.getParentId()));
		checkFolderPermission(Permission.ADD, user, wsFolder.getParentId());

		// Add a folder history entry
		FolderHistory transaction = new FolderHistory();
		transaction.setUser(user);
		transaction.setSessionId(sid);

		Folder folderVO = new Folder();
		folderVO.setTenantId(user.getTenantId());
		folderVO.setName(wsFolder.getName());
		folderVO.setDescription(wsFolder.getDescription());
		folderVO.setType(wsFolder.getType());
		folderVO.setPosition(wsFolder.getPosition());
		folderVO.setTemplateLocked(wsFolder.getTemplateLocked());
		folderVO.setHidden(wsFolder.getHidden());
		folderVO.setFoldRef(wsFolder.getFoldRef());
		folderVO.setStore(wsFolder.getStore());
		folderVO.setMaxVersions(wsFolder.getMaxVersions());
		folderVO.setSecurityRef(wsFolder.getSecurityRef());
		folderVO.setFoldRef(wsFolder.getFoldRef());
		folderVO.setOcrTemplateId(wsFolder.getOcrTemplateId());
		folderVO.setBarcodeTemplateId(wsFolder.getBarcodeTemplateId());
		folderVO.setColor(wsFolder.getColor());
		folderVO.setTile(wsFolder.getTile());

		folderVO.setTagsFromWords(Set.copyOf(wsFolder.getTags()));

		wsFolder.updateAttributes(folderVO);

		Folder f = folderDao.create(parentFolder, folderVO, true, transaction);

		if (f == null) {
			log.error("Folder {} not created", folderVO.getName());
			throw new WebserviceException("error");
		}

		WSFolder createdFolder = WSFolder.fromFolder(f);
		log.info("Created folder {}", createdFolder.getName());
		return createdFolder;
	}

	@Override
	public WSFolder createAlias(String sid, long parentId, long foldRef)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);

		FolderDAO folderDao = FolderDAO.get();
		Folder parentFolder = folderDao.findFolder(parentId);
		if (parentFolder == null)
			throw new WebserviceException(String.format("A parent folder with id %s was not found.", parentId));
		checkFolderPermission(Permission.ADD, user, parentFolder.getId());

		// Add a folder history entry
		FolderHistory transaction = new FolderHistory();
		transaction.setUser(user);
		transaction.setSessionId(sid);

		Folder f = folderDao.createAlias(parentFolder.getId(), foldRef, transaction);

		WSFolder createdFolder = WSFolder.fromFolder(f);
		log.info("Created folder {}", createdFolder.getName());
		return createdFolder;
	}

	@Override
	public long createFolder(String sid, long parentId, String name)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WSFolder folder = new WSFolder();
		folder.setParentId(parentId);
		folder.setName(name);
		folder.setType(Folder.TYPE_DEFAULT);
		return this.create(sid, folder).getId();
	}

	@Override
	public void delete(String sid, long folderId)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);
		FolderDAO folderDao = FolderDAO.get();
		long rootId = folderDao.findRoot(user.getTenantId()).getTenantId();
		if (folderId == rootId)
			throw new WebserviceException("Cannot delete root folder or Default workspace");

		checkFolderPermission(Permission.DELETE, user, folderId);

		// Add a folder history entry
		FolderHistory transaction = new FolderHistory();
		transaction.setUser(user);
		transaction.setEvent(FolderEvent.DELETED);
		transaction.setSessionId(sid);
		folderDao.deleteTree(folderId, transaction);
	}

	@Override
	public WSFolder getFolder(String sid, long folderId)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);
		checkFolderPermission(Permission.READ, user, folderId);
		FolderDAO folderDao = FolderDAO.get();
		Folder folder = folderDao.findById(folderId);
		if (folder == null)
			return null;

		folderDao.initialize(folder);

		return WSFolder.fromFolder(folder);
	}

	@Override
	public WSFolder findByPath(String sid, String path)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);

		FolderDAO folderDao = FolderDAO.get();
		Folder folder = folderDao.findByPathExtended(path, user.getTenantId());
		if (folder == null)
			return null;

		folderDao.initialize(folder);
		checkFolderPermission(Permission.READ, user, folder.getId());
		return WSFolder.fromFolder(folder);
	}

	@Override
	public boolean isReadable(String sid, long folderId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		try {
			checkFolderPermission(Permission.READ, user, folderId);
		} catch (Exception e) {
			return false;
		}

		return true;
	}

	@Override
	public List<WSFolder> list(String sid, long folderId, String sort, Integer page, Integer max)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);
		checkFolderPermission(Permission.READ, user, folderId);

		FolderDAO folderDao = FolderDAO.get();

		// Get the folders
		List<Folder> folders = folderDao.findChildren(folderId, user.getId());

		// Sort based on the given criteria
		if (StringUtils.isNotEmpty(sort))
			Collections.sort(folders, FolderComparator.getComparator(sort));

		// In case of pagination, extract just the wanted page
		if (max != null && page != null && max < folders.size())
			folders = folders.stream().skip((page - 1) * (long) max).limit(max).collect(Collectors.toList());

		List<WSFolder> wsFolders = new ArrayList<>();
		for (Folder folder : folders) {
			if (folder.getHidden() == 0) {
				folderDao.initialize(folder);
				wsFolders.add(WSFolder.fromFolder(folder));
			}
		}

		return wsFolders;
	}

	@Override
	public List<WSFolder> listChildren(String sid, long folderId)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		return list(sid, folderId, "name asc", null, null);
	}

	@Override
	public void move(String sid, long folderId, long parentId)
			throws PersistenceException, AuthenticationException, WebserviceException, PermissionException {
		User user = validateSession(sid);
		FolderDAO folderDao = FolderDAO.get();

		if (parentId == folderDao.findRoot(user.getTenantId()).getId()) {
			String message = CANNOT_MOVE_FOLDERS_IN_THE_ROOT;
			log.error(message);
			throw new WebserviceException(message);
		}

		Folder destParentFolder = folderDao.findById(parentId);
		Folder folderToMove = folderDao.findById(folderId);

		// Check destParentId: Must be different from the current folder
		// parentId
		if (parentId == folderToMove.getParentId())
			throw new SecurityException(NO_CHANGES);

		// Check destParentId: Must be different from the current folder Id
		// A folder cannot be children of herself
		if (parentId == folderToMove.getId())
			throw new SecurityException(NOT_ALLOWED);

		// Check delete permission on the folder parent of folderToMove
		Folder sourceParent = folderDao.findById(folderToMove.getParentId());
		boolean sourceParentDeleteEnabled = folderDao.isPermissionAllowed(Permission.DELETE, sourceParent.getId(),
				user.getId());
		if (!sourceParentDeleteEnabled)
			throw new SecurityException("No rights to delete folder");

		// Check add permission on destParentFolder
		checkFolderPermission(Permission.ADD, user, destParentFolder.getId());

		// Add a folder history entry
		FolderHistory transaction = new FolderHistory();
		transaction.setSessionId(sid);
		transaction.setUser(user);

		folderDao.move(folderToMove, destParentFolder, transaction);
	}

	@Override
	public void copy(String sid, long folderId, long targetId, int foldersOnly, String securityOption)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);
		FolderDAO folderDao = FolderDAO.get();

		if (targetId == folderDao.findRoot(user.getTenantId()).getId()) {
			String message = CANNOT_MOVE_FOLDERS_IN_THE_ROOT;
			log.error(message);
			throw new WebserviceException(message);
		}

		Folder destTargetFolder = folderDao.findById(targetId);
		Folder folderToCopy = folderDao.findById(folderId);

		// Check destParentId: Must be different from the current folder
		// parentId
		if (targetId == folderToCopy.getParentId())
			throw new SecurityException(NO_CHANGES);

		// Check destParentId: Must be different from the current folder Id
		// A folder cannot be children of herself
		if (targetId == folderToCopy.getId())
			throw new SecurityException(NOT_ALLOWED);

		// Check add permission on destParentFolder
		checkFolderPermission(Permission.ADD, user, destTargetFolder.getId());

		// Add a folder history entry
		FolderHistory transaction = new FolderHistory();
		transaction.setSessionId(sid);
		transaction.setUser(user);

		folderDao.copy(folderToCopy, destTargetFolder, null, 1 == foldersOnly, securityOption, transaction);
	}

	@Override
	public void rename(String sid, long folderId, String name)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);
		FolderDAO folderDao = FolderDAO.get();
		if (!folderDao.isPermissionAllowed(Permission.RENAME, folderId, user.getId()))
			throw new PermissionException(user.getUsername(), FOLDER + folderId, Permission.RENAME);

		long rootId = folderDao.findRoot(user.getTenantId()).getId();
		if (folderId == rootId)
			throw new WebserviceException("cannot rename the root folder");

		Folder folder = folderDao.findById(folderId);
		if (folder == null)
			throw new WebserviceException(String.format("cannot find folder %s", folderId));

		folderDao.initialize(folder);

		List<Folder> folders = folderDao.findByNameAndParentId(name, folder.getParentId());
		if (CollectionUtils.isNotEmpty(folders)) {
			throw new WebserviceException(String.format("duplicate folder name %s", name));
		} else {
			// Add a folder history entry
			FolderHistory transaction = new FolderHistory();
			transaction.setUser(user);
			transaction.setEvent(FolderEvent.RENAMED);
			transaction.setSessionId(sid);
			transaction.setFilenameOld(folder.getName());
			transaction.setPathOld(folderDao.computePathExtended(folderId));

			folder.setName(name);
			folderDao.store(folder, transaction);
		}
	}

	@Override
	public WSFolder getRootFolder(String sid)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		FolderDAO folderDao = FolderDAO.get();

		Folder folder = folderDao.findRoot(user.getTenantId());
		folderDao.initialize(folder);

		return WSFolder.fromFolder(folder);
	}

	@Override
	public WSFolder getDefaultWorkspace(String sid)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);
		FolderDAO folderDao = FolderDAO.get();
		return getFolder(sid, folderDao.findDefaultWorkspace(user.getTenantId()).getId());
	}

	@Override
	public boolean isWritable(String sid, long folderId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		FolderDAO folderDao = FolderDAO.get();
		return folderDao.isWriteAllowed(folderId, user.getId());
	}

	@Override
	public boolean isGranted(String sid, long folderId, String permission)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		try {
			checkFolderPermission(Permission.valueOf(permission), user, folderId);
		} catch (Exception e) {
			return false;
		}
		return true;
	}

	@Override
	public List<WSFolder> getPath(String sid, long folderId)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);

		checkFolderPermission(Permission.READ, user, folderId);

		List<WSFolder> path = new ArrayList<>();

		FolderDAO folderDao = FolderDAO.get();
		long rootId = folderDao.findRoot(user.getTenantId()).getId();
		if (folderId == rootId)
			path.add(getRootFolder(sid));
		else {
			// Iterate on the parents and populate the path
			List<Folder> folders = folderDao.findParents(folderId);
			for (Folder folder : folders) {
				folderDao.initialize(folder);
				WSFolder wsFolder = WSFolder.fromFolder(folder);
				path.add(wsFolder);
			}
			// Insert the target folder itself
			Folder f = folderDao.findById(folderId);
			folderDao.initialize(f);
			WSFolder wsFolder = WSFolder.fromFolder(f);
			path.add(wsFolder);
		}

		return path;
	}

	@Override
	public void setAccessControlList(String sid, long folderId, List<WSAccessControlEntry> acl)
			throws PersistenceException, PermissionException, AuthenticationException, WebserviceException {
		User sessionUser = validateSession(sid);

		FolderDAO folderDao = FolderDAO.get();
		// Check if the session user has the Security Permission of this
		// folder
		if (!folderDao.isPermissionAllowed(Permission.SECURITY, folderId, sessionUser.getId()))
			throw new PermissionException(sessionUser.getUsername(), FOLDER + folderId, Permission.SECURITY);

		Folder folder = folderDao.findById(folderId);
		folderDao.initialize(folder);
		folder.setSecurityRef(null);
		folder.getAccessControlList().clear();
		for (WSAccessControlEntry wsAcwe : acl)
			folder.addAccessControlEntry(WSUtil.toFolderAccessControlEntry(wsAcwe));

		FolderHistory history = new FolderHistory();
		history.setEvent(FolderEvent.PERMISSION);
		history.setSession(SessionManager.get().get(sid));
		folderDao.store(folder, history);

	}

	@Override
	public List<WSAccessControlEntry> getAccessControlList(String sid, long folderId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		validateSession(sid);

		List<WSAccessControlEntry> acl = new ArrayList<>();

		FolderDAO folderDao = FolderDAO.get();
		Folder folder = folderDao.findById(folderId);
		if (folder.getSecurityRef() != null)
			folder = folderDao.findById(folder.getSecurityRef());
		folderDao.initialize(folder);

		for (AccessControlEntry ace : folder.getAccessControlList())
			acl.add(WSUtil.toWSAccessControlEntry(ace));
		return acl;
	}

	@Override
	public void update(String sid, WSFolder wsFolder)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);

		long folderId = wsFolder.getId();
		String name = wsFolder.getName();

		FolderDAO folderDao = FolderDAO.get();
		if (!folderDao.isPermissionAllowed(Permission.RENAME, folderId, user.getId()))
			throw new PermissionException(user.getUsername(), FOLDER + folderId, Permission.RENAME);

		if (folderId == folderDao.findRoot(user.getTenantId()).getId())
			throw new PermissionException("cannot update the root folder");

		Folder folder = folderDao.findById(folderId);
		if (folder == null)
			throw new WebserviceException(String.format("cannot find folder %s", folderId));
		String oldName = folder.getName();

		List<Folder> folders = folderDao.findByNameAndParentId(name, folder.getParentId());
		if (CollectionUtils.isNotEmpty(folders) && folders.get(0).getId() != folder.getId()) {
			throw new WebserviceException(String.format("duplicate folder name %s", name));
		} else {
			folderDao.initialize(folder);

			folder.setName(name);
			folder.setColor(wsFolder.getColor());
			folder.setTile(wsFolder.getTile());
			folder.setDescription(wsFolder.getDescription());
			folder.setTemplateLocked(wsFolder.getTemplateLocked());
			folder.setPosition(wsFolder.getPosition());
			folder.setSecurityRef(wsFolder.getSecurityRef());
			folder.setFoldRef(wsFolder.getFoldRef());
			folder.setOcrTemplateId(wsFolder.getOcrTemplateId());
			folder.setBarcodeTemplateId(wsFolder.getBarcodeTemplateId());
			folder.setStore(wsFolder.getStore());
			folder.setMaxVersions(wsFolder.getMaxVersions());
			folder.setTagsFromWords(new HashSet<>(wsFolder.getTags()));

			wsFolder.updateAttributes(folder);

			// Add a folder history entry
			FolderHistory transaction = new FolderHistory();
			transaction.setUser(user);
			transaction.setEvent(FolderEvent.CHANGED);
			transaction.setSessionId(sid);
			
			folderDao.store(folder, transaction);
			
			if(!oldName.equals(name)) {
				FolderHistory renameHistory = new FolderHistory();
				renameHistory.setUser(user);
				renameHistory.setEvent(FolderEvent.RENAMED);
				renameHistory.setSessionId(sid);
				folderDao.saveFolderHistory(folder, renameHistory);
			}
		}
	}

	@Override
	public WSFolder createPath(String sid, long parentId, String path)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);
		checkFolderPermission(Permission.ADD, user, parentId);

		FolderDAO folderDao = FolderDAO.get();
		Folder parent = folderDao.findById(parentId);
		folderDao.initialize(parent);

		FolderHistory transaction = new FolderHistory();
		transaction.setUser(user);
		transaction.setEvent(FolderEvent.CREATED);
		transaction.setSessionId(sid);
		transaction.setTenantId(user.getTenantId());

		if (path.startsWith("/"))
			path = path.substring(1);

		/*
		 * Cannot write in the root so if the parent is the root, we have to
		 * guarantee that the first element in the path is a workspace. If not
		 * the Default one will be used.
		 */
		long rootId = folderDao.findRoot(user.getTenantId()).getTenantId();
		if (parentId == rootId) {
			Folder workspace = null;

			/*
			 * Check if the path contains the workspace specification
			 */
			for (WSFolder w : listWorkspaces(sid)) {
				if (path.startsWith(w.getName() + "/")) {
					workspace = folderDao.findById(w.getId());
					folderDao.initialize(workspace);
					break;
				}
			}

			if (workspace == null) {
				log.debug("Path {} will be created in the Default workspace", path);
				parent = folderDao.findDefaultWorkspace(user.getTenantId());
				folderDao.initialize(parent);
			}
		}

		Folder folder = folderDao.createPath(parent, path, true, transaction);

		return WSFolder.fromFolder(folder, false);
	}

	@Override
	public List<WSFolder> listWorkspaces(String sid)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);

		FolderDAO folderDao = FolderDAO.get();
		List<Folder> folders = folderDao.findByUserId(user.getId(), folderDao.findRoot(user.getTenantId()).getId());
		List<WSFolder> wsFolders = new ArrayList<>();
		for (Folder folder : folders) {
			if (folder.getType() == Folder.TYPE_WORKSPACE) {
				folderDao.initialize(folder);
				wsFolders.add(WSFolder.fromFolder(folder));
			}
		}
		return wsFolders;
	}

	@Override
	public void merge(String sid, long sourceId, long targetId)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);
		FolderDAO folderDao = FolderDAO.get();

		if (targetId == folderDao.findRoot(user.getTenantId()).getId()) {
			throw new PermissionException(CANNOT_MOVE_FOLDERS_IN_THE_ROOT);
		}

		Folder destTargetFolder = folderDao.findById(targetId);
		Folder folderToCopy = folderDao.findById(sourceId);

		// Check destParentId: Must be different from the current folder
		// parentId
		if (targetId == folderToCopy.getParentId())
			throw new SecurityException(NO_CHANGES);

		// Check destParentId: Must be different from the current folder Id
		// A folder cannot be children of herself
		if (targetId == folderToCopy.getId())
			throw new SecurityException(NOT_ALLOWED);

		// Check add permission on destParentFolder
		checkFolderPermission(Permission.ADD, user, destTargetFolder.getId());
		checkFolderPermission(Permission.WRITE, user, destTargetFolder.getId());

		checkFolderPermission(Permission.DELETE, user, sourceId);

		// Add a folder history entry
		FolderHistory transaction = new FolderHistory();
		transaction.setSessionId(sid);
		transaction.setUser(user);

		folderDao.merge(folderToCopy, destTargetFolder, transaction);
	}
}