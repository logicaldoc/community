package com.logicaldoc.webservice.soap.endpoint;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderEvent;
import com.logicaldoc.core.folder.FolderGroup;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.GroupDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.webservice.AbstractService;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSRight;
import com.logicaldoc.webservice.soap.FolderService;

/**
 * Folder Web Service Implementation
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
public class SoapFolderService extends AbstractService implements FolderService {

	protected static Logger log = LoggerFactory.getLogger(SoapFolderService.class);

	@Override
	public WSFolder create(String sid, WSFolder folder) throws Exception {
		User user = validateSession(sid);

		try {
			FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			Folder parentFolder = folderDao.findById(folder.getParentId());
			if (parentFolder == null)
				throw new Exception(String.format("A parent folder with id %s was not found.", folder.getParentId()));
			checkPermission(Permission.ADD, user, folder.getParentId());

			// Add a folder history entry
			FolderHistory transaction = new FolderHistory();
			transaction.setUser(user);
			transaction.setSessionId(sid);

			Folder folderVO = new Folder();
			folderVO.setTenantId(user.getTenantId());
			folderVO.setName(folder.getName());
			folderVO.setDescription(folder.getDescription());
			folderVO.setType(folder.getType());
			folderVO.setPosition(folder.getPosition());
			folderVO.setTemplateLocked(folder.getTemplateLocked());
			folderVO.setHidden(folder.getHidden());
			folderVO.setFoldRef(folder.getFoldRef());
			folderVO.setStorage(folder.getStorage());
			folderVO.setMaxVersions(folder.getMaxVersions());
			folderVO.setSecurityRef(folder.getSecurityRef());
			folderVO.setFoldRef(folder.getFoldRef());
			folderVO.setOcrTemplateId(folder.getOcrTemplateId());
			folderVO.setBarcodeTemplateId(folder.getBarcodeTemplateId());

			Set<String> tagsSet = new TreeSet<String>();
			if (folder.getTags() != null) {
				for (int i = 0; i < folder.getTags().length; i++) {
					tagsSet.add(folder.getTags()[i]);
				}
			}
			folderVO.setTagsFromWords(tagsSet);

			folder.updateAttributes(folderVO);

			Folder f = folderDao.create(parentFolder, folderVO, true, transaction);

			if (f == null) {
				log.error("Folder {} not created", folderVO.getName());
				throw new Exception("error");
			}

			WSFolder createdFolder = WSFolder.fromFolder(f);
			log.info("Created folder {}", createdFolder.getName());
			return createdFolder;
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new RuntimeException(t.getMessage(), t);
		}
	}

	@Override
	public WSFolder createAlias(String sid, long parentId, long foldRef) throws Exception {
		User user = validateSession(sid);

		try {
			FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			Folder parentFolder = folderDao.findFolder(parentId);
			if (parentFolder == null)
				throw new Exception(String.format("A parent folder with id %s was not found.", parentId));
			checkPermission(Permission.ADD, user, parentFolder.getId());

			// Add a folder history entry
			FolderHistory transaction = new FolderHistory();
			transaction.setUser(user);
			transaction.setSessionId(sid);

			Folder f = folderDao.createAlias(parentFolder.getId(), foldRef, transaction);

			WSFolder createdFolder = WSFolder.fromFolder(f);
			log.info("Created folder {}", createdFolder.getName());
			return createdFolder;
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new RuntimeException(t.getMessage(), t);
		}
	}

	@Override
	public long createFolder(String sid, long parentId, String name) throws Exception {
		WSFolder folder = new WSFolder();
		folder.setParentId(parentId);
		folder.setName(name);
		folder.setType(Folder.TYPE_DEFAULT);
		return this.create(sid, folder).getId();
	}

	@Override
	public void delete(String sid, long folderId) throws Exception {
		User user = validateSession(sid);
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		long rootId = folderDao.findRoot(user.getTenantId()).getTenantId();
		if (folderId == rootId || folderId == rootId)
			throw new Exception("Cannot delete root folder or Default workspace");

		checkPermission(Permission.DELETE, user, folderId);

		try {
			// Add a folder history entry
			FolderHistory transaction = new FolderHistory();
			transaction.setUser(user);
			transaction.setEvent(FolderEvent.DELETED.toString());
			transaction.setSessionId(sid);
			folderDao.deleteTree(folderId, transaction);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new RuntimeException(t.getMessage(), t);
		}
	}

	@Override
	public WSFolder getFolder(String sid, long folderId) throws Exception {
		User user = validateSession(sid);
		checkReadEnable(user, folderId);
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Folder folder = folderDao.findById(folderId);
		folderDao.initialize(folder);

		return WSFolder.fromFolder(folder);
	}

	@Override
	public WSFolder findByPath(String sid, String path) throws Exception {
		User user = validateSession(sid);

		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Folder folder = folderDao.findByPathExtended(path, user.getTenantId());
		if (folder == null)
			return null;

		folderDao.initialize(folder);
		checkReadEnable(user, folder.getId());
		return WSFolder.fromFolder(folder);
	}

	@Override
	public boolean isReadable(String sid, long folderId) throws Exception {
		User user = validateSession(sid);
		try {
			checkReadEnable(user, folderId);
		} catch (Exception e) {
			return false;
		}

		return true;
	}

	@Override
	public WSFolder[] listChildren(String sid, long folderId) throws Exception {

		User user = validateSession(sid);
		checkReadEnable(user, folderId);

		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		List<Folder> folders = folderDao.findChildren(folderId, user.getId());
		List<WSFolder> wsFolders = new ArrayList<WSFolder>();
		for (Folder folder : folders) {
			if (folder.getHidden() == 0) {
				folderDao.initialize(folder);
				wsFolders.add(WSFolder.fromFolder(folder));
			}
		}

		return wsFolders.toArray(new WSFolder[0]);
	}

	@Override
	public void move(String sid, long folderId, long parentId) throws Exception {
		User user = validateSession(sid);
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);

		if (parentId == folderDao.findRoot(user.getTenantId()).getId()) {
			log.error("Cannot move folders in the root");
			throw new Exception("Cannot move folders in the root");
		}

		Folder destParentFolder = folderDao.findById(parentId);
		Folder folderToMove = folderDao.findById(folderId);

		// Check destParentId: Must be different from the current folder
		// parentId
		if (parentId == folderToMove.getParentId())
			throw new SecurityException("No Changes");

		// Check destParentId: Must be different from the current folder Id
		// A folder cannot be children of herself
		if (parentId == folderToMove.getId())
			throw new SecurityException("Not Allowed");

		// Check delete permission on the folder parent of folderToMove
		Folder sourceParent = folderDao.findById(folderToMove.getParentId());
		boolean sourceParentDeleteEnabled = folderDao.isPermissionEnabled(Permission.DELETE, sourceParent.getId(),
				user.getId());
		if (!sourceParentDeleteEnabled)
			throw new SecurityException("No rights to delete folder");

		// Check add permission on destParentFolder
		boolean addEnabled = folderDao.isPermissionEnabled(Permission.ADD, destParentFolder.getId(), user.getId());
		if (!addEnabled)
			throw new SecurityException("Add Rights not granted on the target folder");

		// Add a folder history entry
		FolderHistory transaction = new FolderHistory();
		transaction.setSessionId(sid);
		transaction.setUser(user);

		folderDao.move(folderToMove, destParentFolder, transaction);
	}

	@Override
	public void copy(String sid, long folderId, long targetId, int foldersOnly, String securityOption) throws Exception {
		User user = validateSession(sid);
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);

		if (targetId == folderDao.findRoot(user.getTenantId()).getId()) {
			log.error("Cannot move folders in the root");
			throw new Exception("Cannot move folders in the root");
		}

		Folder destTargetFolder = folderDao.findById(targetId);
		Folder folderToCopy = folderDao.findById(folderId);

		// Check destParentId: Must be different from the current folder
		// parentId
		if (targetId == folderToCopy.getParentId())
			throw new SecurityException("No Changes");

		// Check destParentId: Must be different from the current folder Id
		// A folder cannot be children of herself
		if (targetId == folderToCopy.getId())
			throw new SecurityException("Not Allowed");

		// Check add permission on destParentFolder
		boolean addEnabled = folderDao.isPermissionEnabled(Permission.ADD, destTargetFolder.getId(), user.getId());
		if (!addEnabled)
			throw new SecurityException("Add Child rights not granted on the target folder");

		// Add a folder history entry
		FolderHistory transaction = new FolderHistory();
		transaction.setSessionId(sid);
		transaction.setUser(user);

		folderDao.copy(folderToCopy, destTargetFolder, null, 1 == foldersOnly, securityOption,
				transaction);
	}

	@Override
	public void rename(String sid, long folderId, String name) throws Exception {
		User user = validateSession(sid);
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		if (!folderDao.isPermissionEnabled(Permission.RENAME, folderId, user.getId())) {
			throw new Exception("user does't have rename permission");
		}

		long rootId = folderDao.findRoot(user.getTenantId()).getId();
		if (folderId == rootId)
			throw new Exception("cannot rename the root folder");

		if (folderId == rootId)
			throw new Exception("cannot rename the Default workspace");

		Folder folder = folderDao.findById(folderId);
		if (folder == null)
			throw new Exception(String.format("cannot find folder %s", folderId));

		folderDao.initialize(folder);

		List<Folder> folders = folderDao.findByNameAndParentId(name, folder.getParentId());
		if (folders.size() > 0 && folders.get(0).getId() != folder.getId()) {
			throw new Exception(String.format("duplicate folder name %s", name));
		} else {
			// Add a folder history entry
			FolderHistory transaction = new FolderHistory();
			transaction.setUser(user);
			transaction.setEvent(FolderEvent.RENAMED.toString());
			transaction.setSessionId(sid);
			transaction.setFilenameOld(folder.getName());
			transaction.setPathOld(folderDao.computePathExtended(folderId));

			folder.setName(name);
			folderDao.store(folder, transaction);
		}
	}

	@Override
	public WSFolder getRootFolder(String sid) throws Exception {
		User user = validateSession(sid);
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);

		Folder folder = folderDao.findRoot(user.getTenantId());
		folderDao.initialize(folder);

		return WSFolder.fromFolder(folder);
	}

	@Override
	public WSFolder getDefaultWorkspace(String sid) throws Exception {
		User user = validateSession(sid);
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		return getFolder(sid, folderDao.findDefaultWorkspace(user.getTenantId()).getId());
	}

	@Override
	public boolean isWritable(String sid, long folderId) throws Exception {
		User user = validateSession(sid);
		try {
			checkWriteEnable(user, folderId);
		} catch (Exception e) {
			return false;
		}
		return true;
	}

	@Override
	public boolean isGranted(String sid, long folderId, int permission) throws Exception {
		User user = validateSession(sid);
		try {
			checkPermission(Permission.valueOf(permission), user, folderId);
		} catch (Exception e) {
			return false;
		}
		return true;
	}

	@Override
	public WSFolder[] getPath(String sid, long folderId) throws Exception {
		User user = validateSession(sid);

		checkReadEnable(user, folderId);

		List<WSFolder> path = new ArrayList<WSFolder>();

		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
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

		return path.toArray(new WSFolder[0]);
	}

	@Override
	public void grantUser(String sid, long folderId, long userId, int permissions, boolean recursive) throws Exception {
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);

		User user = userDao.findById(userId);
		grantGroup(sid, folderId, user.getUserGroup().getId(), permissions, recursive);
	}

	@Override
	public void grantGroup(String sid, long folderId, long groupId, int permissions, boolean recursive)
			throws Exception {
		User sessionUser = validateSession(sid);

		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		// Check if the session user has the Security Permission of this folder
		if (!folderDao.isPermissionEnabled(Permission.SECURITY, folderId, sessionUser.getId()))
			throw new Exception(String.format("Security Rights not granted to the user on folder id %s", folderId));
		try {
			Folder folder = folderDao.findById(folderId);
			folderDao.initialize(folder);
			folder.setSecurityRef(null);

			FolderGroup fg = new FolderGroup();
			fg.setGroupId(groupId);
			fg.setPermissions(permissions);
			folder.addFolderGroup(fg);

			FolderHistory history = new FolderHistory();
			history.setEvent(FolderEvent.PERMISSION.toString());
			history.setSession(SessionManager.get().get(sid));
			folderDao.store(folder, history);

			if (recursive) {
				folderDao.initialize(folder);
				FolderHistory transaction = new FolderHistory();
				transaction.setUser(sessionUser);
				transaction.setSessionId(sid);
				folderDao.applyRightToTree(folder.getId(), transaction);
			}
		} catch (Throwable e) {
			log.error("Some errors occurred", e);
			throw new Exception("error", e);
		}
	}

	@Override
	public WSRight[] getGrantedUsers(String sid, long folderId) throws Exception {
		return getGranted(sid, folderId, true);
	}

	private WSRight[] getGranted(String sid, long folderId, boolean users) throws Exception {
		validateSession(sid);

		List<WSRight> rightsList = new ArrayList<WSRight>();
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		GroupDAO groupDao = (GroupDAO) Context.get().getBean(GroupDAO.class);
		try {
			Folder folder = folderDao.findById(folderId);
			if (folder.getSecurityRef() != null)
				folder = folderDao.findById(folder.getSecurityRef());
			folderDao.initialize(folder);
			for (FolderGroup mg : folder.getFolderGroups()) {
				Group group = groupDao.findById(mg.getGroupId());
				if (group.getName().startsWith("_user_") && users) {
					rightsList.add(
							new WSRight(Long.parseLong(group.getName().substring(group.getName().lastIndexOf('_') + 1)),
									mg.getPermissions()));
				} else if (!group.getName().startsWith("_user_") && !users)
					rightsList.add(new WSRight(group.getId(), mg.getPermissions()));
			}
		} catch (Exception e) {
			log.error("Some errors occurred", e);
			throw new Exception("error", e);
		}

		return (WSRight[]) rightsList.toArray(new WSRight[rightsList.size()]);
	}

	@Override
	public WSRight[] getGrantedGroups(String sid, long folderId) throws Exception {
		return getGranted(sid, folderId, false);
	}

	@Override
	public void update(String sid, WSFolder folder) throws Exception {
		User user = validateSession(sid);

		long folderId = folder.getId();
		String name = folder.getName();

		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		if (!folderDao.isPermissionEnabled(Permission.RENAME, folderId, user.getId())) {
			throw new Exception("user does't have rename permission");
		}

		if (folderId == folderDao.findRoot(user.getTenantId()).getId())
			throw new Exception("cannot update the root folder");

		Folder fld = folderDao.findById(folderId);
		if (fld == null)
			throw new Exception(String.format("cannot find folder %s", folderId));

		List<Folder> folders = folderDao.findByNameAndParentId(name, fld.getParentId());
		if (folders.size() > 0 && folders.get(0).getId() != fld.getId()) {
			throw new Exception(String.format("duplicate folder name %s", name));
		} else {
			folderDao.initialize(fld);

			fld.setName(name);
			fld.setDescription(folder.getDescription());
			fld.setTemplateLocked(folder.getTemplateLocked());
			fld.setPosition(folder.getPosition());
			fld.setSecurityRef(folder.getSecurityRef());
			fld.setFoldRef(folder.getFoldRef());
			fld.setOcrTemplateId(folder.getOcrTemplateId());
			fld.setBarcodeTemplateId(folder.getBarcodeTemplateId());
			fld.setStorage(folder.getStorage());
			fld.setMaxVersions(folder.getMaxVersions());

			Set<String> tagsSet = new TreeSet<String>();
			if (folder.getTags() != null) {
				for (int i = 0; i < folder.getTags().length; i++) {
					tagsSet.add(folder.getTags()[i]);
				}
			}
			fld.setTagsFromWords(tagsSet);

			folder.updateAttributes(fld);

			// Add a folder history entry
			FolderHistory transaction = new FolderHistory();
			transaction.setUser(user);
			transaction.setEvent(FolderEvent.RENAMED.toString());
			transaction.setSessionId(sid);
			folderDao.store(fld, transaction);
		}
	}

	@Override
	public WSFolder createPath(String sid, long parentId, String path) throws Exception {
		User user = validateSession(sid);
		checkPermission(Permission.ADD, user, parentId);

		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Folder parent = folderDao.findById(parentId);
		folderDao.initialize(parent);

		FolderHistory transaction = new FolderHistory();
		transaction.setUser(user);
		transaction.setEvent(FolderEvent.CREATED.toString());
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
	public WSFolder[] listWorkspaces(String sid) throws Exception {
		User user = validateSession(sid);

		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		List<Folder> folders = folderDao.findByUserId(user.getId(), folderDao.findRoot(user.getTenantId()).getId());
		List<WSFolder> wsFolders = new ArrayList<WSFolder>();
		for (Folder folder : folders) {
			if (folder.getType() == Folder.TYPE_WORKSPACE) {
				folderDao.initialize(folder);
				wsFolders.add(WSFolder.fromFolder(folder));
			}
		}
		return wsFolders.toArray(new WSFolder[0]);
	}

	@Override
	public void merge(String sid, long sourceId, long targetId) throws Exception {
		User user = validateSession(sid);
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);

		if (targetId == folderDao.findRoot(user.getTenantId()).getId()) {
			log.error("Cannot move folders in the root");
			throw new Exception("Cannot move folders in the root");
		}

		Folder destTargetFolder = folderDao.findById(targetId);
		Folder folderToCopy = folderDao.findById(sourceId);

		// Check destParentId: Must be different from the current folder
		// parentId
		if (targetId == folderToCopy.getParentId())
			throw new SecurityException("No Changes");

		// Check destParentId: Must be different from the current folder Id
		// A folder cannot be children of herself
		if (targetId == folderToCopy.getId())
			throw new SecurityException("Not Allowed");

		// Check add permission on destParentFolder
		boolean addEnabled = folderDao.isPermissionEnabled(Permission.ADD, destTargetFolder.getId(), user.getId());
		if (!addEnabled)
			throw new SecurityException("Add Child rights not granted on the target folder");

		boolean writeEnabled = folderDao.isPermissionEnabled(Permission.WRITE, destTargetFolder.getId(), user.getId());
		if (!writeEnabled)
			throw new SecurityException("Write rights not granted on the target folder");

		boolean delEnabled = folderDao.isPermissionEnabled(Permission.DELETE, sourceId, user.getId());
		if (!delEnabled)
			throw new SecurityException("Delete rights not granted on the source folder");

		// Add a folder history entry
		FolderHistory transaction = new FolderHistory();
		transaction.setSessionId(sid);
		transaction.setUser(user);

		folderDao.merge(folderToCopy, destTargetFolder, transaction);
	}
}