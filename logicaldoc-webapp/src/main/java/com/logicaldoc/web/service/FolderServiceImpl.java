package com.logicaldoc.web.service;

import java.security.AccessControlException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderEvent;
import com.logicaldoc.core.folder.FolderGroup;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.sequence.SequenceDAO;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIRight;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.frontend.client.clipboard.Clipboard;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.data.FoldersDataServlet;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * Implementation of the FolderService
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class FolderServiceImpl extends RemoteServiceServlet implements FolderService {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(FolderServiceImpl.class);

	@Override
	public GUIFolder inheritRights(long folderId, long rightsFolderId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);

			/*
			 * Just apply the current security settings to the whole subtree
			 */
			FolderHistory transaction = new FolderHistory();
			transaction.setSession(session);

			fdao.updateSecurityRef(folderId, rightsFolderId, transaction);

			return getFolder(session, folderId);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
		return null;
	}

	@Override
	public void applyRights(GUIFolder folder, boolean subtree) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			Folder f = fdao.findById(folder.getId());
			fdao.initialize(f);

			if (subtree) {
				/*
				 * Just apply the current security settings to the whole subtree
				 */
				Runnable runnable = new Runnable() {

					@Override
					public void run() {
						FolderHistory history = new FolderHistory();
						history.setSession(session);
						history.setEvent(FolderEvent.PERMISSION.toString());
						try {
							fdao.applyRightToTree(folder.getId(), history);
						} catch (PersistenceException e) {
							log.error(e.getMessage(), e);
						}
					}

				};

				ServiceUtil.executeLongRunningOperation("Apply Rights to Tree", runnable, session);
			} else {
				saveRules(session, f, session.getUserId(), folder.getRights());
			}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void applyMetadata(long parentId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			Runnable runnable = new Runnable() {

				@Override
				public void run() {
					FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
					FolderHistory transaction = new FolderHistory();
					transaction.setSession(session);

					try {
						fdao.applyMetadataToTree(parentId, transaction);
					} catch (PersistenceException e) {
						throw new RuntimeException(e.getMessage(), e);
					}
				}

			};

			ServiceUtil.executeLongRunningOperation("Apply Folder Metadata", runnable, session);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void delete(final long[] folderIds) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		try {
			for (int i = 0; i < folderIds.length; i++)
				delete(session, folderIds[i]);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	private void delete(Session session, final long folderId) throws Exception {
		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		if (!dao.isPermissionEnabled(Permission.DELETE, folderId, session.getUserId()))
			throw new ServerException("Permission DELETE not granted");

		// Add a folder history entry
		FolderHistory transaction = new FolderHistory();
		transaction.setSession(session);
		transaction.setEvent(FolderEvent.DELETED.toString());
		dao.deleteTree(folderId, PersistentObject.DELETED_CODE_DEFAULT, transaction);
	}

	public static GUIFolder fromFolder(Folder folder, boolean computePath) throws PersistenceException {
		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		dao.initialize(folder);

		GUIFolder f = new GUIFolder();
		f.setId(folder.getId());
		f.setName(folder.getId() != Constants.DOCUMENTS_FOLDERID ? folder.getName() : "/");
		f.setParentId(folder.getParentId());
		f.setDescription(folder.getDescription());
		f.setCreation(folder.getCreation());
		f.setCreator(folder.getCreator());
		f.setCreatorId(folder.getCreatorId());
		f.setType(folder.getType());
		f.setPosition(folder.getPosition());
		f.setQuotaDocs(folder.getQuotaDocs());
		f.setQuotaSize(folder.getQuotaSize());
		f.setFoldRef(folder.getFoldRef());
		f.setStorage(folder.getStorage());
		f.setMaxVersions(folder.getMaxVersions());
		f.setColor(folder.getColor());
		f.setGrid(folder.getGrid());
		f.setQuotaThreshold(folder.getQuotaThreshold());
		f.setQuotaAlertRecipients(folder.getQuotaAlertRecipientsAsList().toArray(new String[0]));
		f.setOcrTemplateId(folder.getOcrTemplateId());
		f.setBarcodeTemplateId(folder.getBarcodeTemplateId());
		if (computePath)
			f.setPathExtended(dao.computePathExtended(folder.getId()));

		if (f.isWorkspace()) {
			SequenceDAO seqDao = (SequenceDAO) Context.get().getBean(SequenceDAO.class);
			f.setDocumentsTotal(seqDao.getCurrentValue("wsdocs", folder.getId(), folder.getTenantId()));
			f.setSizeTotal(seqDao.getCurrentValue("wssize", folder.getId(), folder.getTenantId()));
		}

		if (folder.getSecurityRef() != null) {
			GUIFolder secRef = new GUIFolder();
			secRef.setId(folder.getSecurityRef());
			if (computePath)
				secRef.setPathExtended(dao.computePathExtended(folder.getSecurityRef()));
			f.setSecurityRef(secRef);
		}

		if (folder.getTemplate() != null) {
			f.setTemplateId(folder.getTemplate().getId());
			f.setTemplate(folder.getTemplate().getName());
			f.setTemplateLocked(folder.getTemplateLocked());
			GUIAttribute[] attributes = ServiceUtil.prepareGUIAttributes(folder.getTemplate(), folder);
			f.setAttributes(attributes);
		}

		if (folder.getTags() != null && folder.getTags().size() > 0)
			f.setTags(folder.getTagsAsWords().toArray(new String[folder.getTags().size()]));
		else
			f.setTags(new String[0]);

		f.setColor(folder.getColor());
		return f;
	}

	@Override
	public int[] computeStats(long folderId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			return new int[] { countDocs(folderId), countChildren(folderId) };
		} catch (Throwable t) {
			return (int[]) ServiceUtil.throwServerException(session, log, t);
		}
	}

	private static int countDocs(long folderId) {
		int count = 0;
		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);

		try {
			count = dao.queryForInt("select count(ld_id) from ld_document where ld_deleted=0 and ld_folderid="
					+ folderId + " and not ld_status=" + AbstractDocument.DOC_ARCHIVED);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
		}
		return count;
	}

	private static int countChildren(long folderId) {
		int count = 0;
		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);

		try {
			count = dao.queryForInt(
					"select count(ld_id) from ld_folder where not ld_id=ld_parentid and ld_deleted=0 and ld_parentid="
							+ folderId);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
		}

		return count;
	}

	public static GUIFolder getFolder(Session session, long folderId) throws ServerException {
		return getFolder(session, folderId, false);
	}

	public static GUIFolder getFolder(Session session, long folderId, boolean computePath) throws ServerException {
		if (session != null)
			ServiceUtil.validateSession(session.getSid());
		try {
			FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);

			if (session != null && !dao.isReadEnabled(folderId, session.getUserId()))
				return null;

			Folder folder = null;
			Folder test = dao.findById(folderId);
			if (test == null)
				return null;

			dao.initialize(test);
			GUIFolder guiFolder = null;

			// Check if it is an alias
			if (test.getFoldRef() != null) {
				folder = dao.findById(test.getFoldRef());
				dao.initialize(folder);
				guiFolder = fromFolder(folder, computePath);

				// The alias rewrite some properties
				guiFolder.setName(test.getName());
				guiFolder.setDescription(test.getDescription());
				guiFolder.setColor(test.getColor());
				guiFolder.setPosition(test.getPosition());
				guiFolder.setFoldRef(test.getFoldRef());
				guiFolder.setId(test.getId());
				if (test.getSecurityRef() != null)
					guiFolder.setSecurityRef(getFolder(session, test.getSecurityRef()));
				else
					guiFolder.setSecurityRef(null);
				guiFolder.setType(Folder.TYPE_ALIAS);
			} else {
				folder = test;
				guiFolder = fromFolder(folder, computePath);
			}

			if (session != null) {
				Set<Permission> permissions = dao.getEnabledPermissions(folderId, session.getUserId());
				List<String> permissionsList = new ArrayList<String>();
				for (Permission permission : permissions)
					permissionsList.add(permission.toString());
				guiFolder.setPermissions(permissionsList.toArray(new String[permissionsList.size()]));
			}

			Folder securityRef = folder;
			if (test.getSecurityRef() != null)
				securityRef = dao.findById(test.getSecurityRef());
			dao.initialize(securityRef);

			int i = 0;
			GUIRight[] rights = new GUIRight[(securityRef != null && securityRef.getFolderGroups() != null)
					? securityRef.getFolderGroups().size()
					: 0];
			if (securityRef != null && securityRef.getFolderGroups() != null)
				for (FolderGroup fg : securityRef.getFolderGroups()) {
					GUIRight right = new GUIRight();
					right.setEntityId(fg.getGroupId());
					right.setAdd(fg.getAdd() == 1 ? true : false);
					right.setWrite(fg.getWrite() == 1 ? true : false);
					right.setSecurity(fg.getSecurity() == 1 ? true : false);
					right.setImmutable(fg.getImmutable() == 1 ? true : false);
					right.setDelete(fg.getDelete() == 1 ? true : false);
					right.setRename(fg.getRename() == 1 ? true : false);
					right.setImport(fg.getImport() == 1 ? true : false);
					right.setExport(fg.getExport() == 1 ? true : false);
					right.setSign(fg.getSign() == 1 ? true : false);
					right.setArchive(fg.getArchive() == 1 ? true : false);
					right.setWorkflow(fg.getWorkflow() == 1 ? true : false);
					right.setDownload(fg.getDownload() == 1 ? true : false);
					right.setCalendar(fg.getCalendar() == 1 ? true : false);
					right.setSubscription(fg.getSubscription() == 1 ? true : false);
					right.setPassword(fg.getPassword() == 1 ? true : false);
					right.setMove(fg.getMove() == 1 ? true : false);
					right.setEmail(fg.getEmail() == 1 ? true : false);
					right.setAutomation(fg.getAutomation() == 1 ? true : false);
					right.setStorage(fg.getStorage() == 1 ? true : false);

					rights[i] = right;
					i++;
				}
			guiFolder.setRights(rights);
			return guiFolder;
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
		}

		return null;
	}

	@Override
	public GUIFolder getFolder(long folderId, boolean computePath, boolean computeDocs, boolean computeSubfolders)
			throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);

			GUIFolder folder = getFolder(session, folderId);
			if (folder == null)
				return null;
			if (computeDocs)
				folder.setDocumentCount(countDocs(folder.getId()));
			if (computeSubfolders)
				folder.setSubfolderCount(countChildren(folder.getId()));

			if (computePath) {
				String pathExtended = dao.computePathExtended(folderId);

				StringTokenizer st = new StringTokenizer(pathExtended, "/", false);
				int elements = st.countTokens();
				GUIFolder[] path = new GUIFolder[elements];
				Folder parent = dao.findRoot(session.getTenantId());
				List<Folder> list = new ArrayList<Folder>();
				int j = 0;
				while (st.hasMoreTokens()) {
					String text = st.nextToken();
					list = dao.findByName(parent, text, null, true);
					if (list.isEmpty())
						return null;

					if (parent.getId() == Folder.ROOTID || parent.getId() == parent.getParentId()) {
						GUIFolder f = new GUIFolder(parent.getId());
						f.setName("/");
						f.setParentId(parent.getId());
						if (computeSubfolders)
							f.setSubfolderCount(countChildren(f.getId()));
						path[j] = f;
					} else
						path[j] = getFolder(parent.getId(), false, false, computeSubfolders);
					parent = list.get(0);
					j++;
				}

				folder.setPath(path);
			}

			return folder;
		} catch (Throwable t) {
			return (GUIFolder) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void copyFolders(long[] folderIds, long targetId, boolean foldersOnly, String securityOption,
			GUIFolder model) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			for (int i = 0; i < folderIds.length; i++) {
				copyFolder(session, folderIds[i], targetId, foldersOnly, securityOption, model);
			}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	private void copyFolder(Session session, long folderId, long targetId, boolean foldersOnly, String securityOption,
			GUIFolder model) throws Exception {
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Folder folderToCopy = folderDao.findById(folderId);

		Folder destParentFolder = folderDao.findFolder(targetId);

		// Check destParentId MUST BE <> 0 (initial value)
		if (targetId == 0 || folderDao.isInPath(folderToCopy.getId(), destParentFolder.getId())) {
			return;
		}

		folderDao.initialize(folderToCopy);

		// Check destParentId: Must be different from the current folder
		// parentId
		if (destParentFolder.getId() == folderToCopy.getParentId())
			throw new SecurityException("No Changes");

		// Check destParentId: Must be different from the current folderId
		// A folder cannot be children of herself
		if (destParentFolder.getId() == folderToCopy.getId())
			throw new SecurityException("Not Allowed");

		// Check addChild permission on destParentFolder
		boolean addchildEnabled = folderDao.isPermissionEnabled(Permission.ADD, destParentFolder.getId(),
				session.getUserId());
		if (!addchildEnabled)
			throw new SecurityException("Add Child body not granted to this user in the target folder");

		// Add a folder history entry
		FolderHistory transaction = new FolderHistory();
		transaction.setSession(session);

		String modelName = (model == null ? null : model.getName());
		Folder createdFolder = folderDao.copy(folderToCopy, destParentFolder, modelName, foldersOnly, securityOption,
				transaction);
		if (model != null) {
			model.setId(createdFolder.getId());
			folderDao.initialize(createdFolder);
			save(model);
		}
	}

	@Override
	public void move(long[] folderIds, long targetId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			for (long folderId : folderIds) {
				move(session, folderId, targetId);
			}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	private void move(Session session, long folderId, long targetId) throws Exception {
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);

		Folder folderToMove = folderDao.findById(folderId);

		// Check destParentId MUST BE <> 0 (initial value)
		if (targetId == 0 || folderDao.isInPath(folderToMove.getId(), targetId)) {
			return;
		}

		Folder destParentFolder = folderDao.findFolder(targetId);

		// Check destParentId: Must be different from the current folder
		// parentId
		if (targetId == folderToMove.getParentId())
			throw new SecurityException("No Changes");

		// Check destParentId: Must be different from the current folderId
		// A folder cannot be children of herself
		if (targetId == folderToMove.getId())
			throw new SecurityException("Not Allowed");

		// Check move permission on the folder parent of folderToMove
		Folder sourceParent = folderDao.findById(folderToMove.getParentId());
		boolean sourceParentMoveEnabled = folderDao.isPermissionEnabled(Permission.MOVE, sourceParent.getId(),
				session.getUserId());
		if (!sourceParentMoveEnabled)
			throw new SecurityException(String.format("User %s has not the MOVE permission on folder %s",
					session.getUsername(), sourceParent.getName()));

		// Check addChild permission on destParentFolder
		boolean addchildEnabled = folderDao.isPermissionEnabled(Permission.ADD, destParentFolder.getId(),
				session.getUserId());
		if (!addchildEnabled)
			throw new SecurityException(String.format("User %s has not the ADD CHILD permission on folder %s",
					session.getUsername(), sourceParent.getName()));

		// Add a folder history entry
		FolderHistory transaction = new FolderHistory();
		transaction.setSession(session);

		folderDao.move(folderToMove, destParentFolder, transaction);
	}

	@Override
	public void rename(long folderId, String name) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		try {
			List<Folder> folders = dao.findByNameAndParentId(name, dao.findById(folderId).getParentId());
			if (folders.size() > 0 && folders.get(0).getId() != folderId) {
				return;
			}
			// To avoid a 'org.hibernate.StaleObjectStateException', we
			// must retrieve the folder from database.
			Folder folder = dao.findById(folderId);
			dao.initialize(folder);

			// Add a folder history entry
			FolderHistory history = new FolderHistory();
			history.setFilenameOld(folder.getName());
			history.setPathOld(dao.computePathExtended(folderId));
			history.setEvent(FolderEvent.RENAMED.toString());
			history.setSession(session);

			folder.setName(name.trim());
			dao.store(folder, history);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public GUIFolder save(GUIFolder guiFolder) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		try {
			Folder folder = folderDao.findById(guiFolder.getId());
			folderDao.initialize(folder);

			FolderHistory transaction = new FolderHistory();
			transaction.setSession(session);

			String folderName = guiFolder.getName().replace("/", "");

			if (guiFolder.getFoldRef() != null) {
				// The user is editing an alias
				folder.setDescription(guiFolder.getDescription());
				folder.setPosition(guiFolder.getPosition());
				folder.setColor(guiFolder.getColor());
				folder.setName(folderName);
				folderDao.store(folder);

				folder = folderDao.findById(folder.getFoldRef());
				folderDao.initialize(folder);
			} else {
				// The user is editing a real folder
				folder.setColor(guiFolder.getColor());
				folder.setDescription(guiFolder.getDescription());
				folder.setPosition(guiFolder.getPosition());
				folder.setType(guiFolder.getType());
				folder.setStorage(guiFolder.getStorage());

				if (folder.isWorkspace()) {
					folder.setMaxVersions(guiFolder.getMaxVersions());
				}

				if (folder.getName().trim().equals(folderName)) {
					folder.setName(folderName.trim());
					transaction.setEvent(FolderEvent.CHANGED.toString());
				} else {
					folder.setName(folderName.trim());
					transaction.setEvent(FolderEvent.RENAMED.toString());
				}
			}

			folder.setTemplateLocked(guiFolder.getTemplateLocked());
			folder.setQuotaDocs(guiFolder.getQuotaDocs());
			folder.setQuotaSize(guiFolder.getQuotaSize());
			folder.setQuotaThreshold(guiFolder.getQuotaThreshold());
			folder.setQuotaAlertRecipients(guiFolder.getQuotaAlertRecipientsAsString());
			folder.setGrid(guiFolder.getGrid());
			folder.setOcrTemplateId(guiFolder.getOcrTemplateId());
			folder.setBarcodeTemplateId(guiFolder.getBarcodeTemplateId());

			updateExtendedAttributes(folder, guiFolder);

			if (guiFolder.getTags() != null && guiFolder.getTags().length > 0)
				folder.setTagsFromWords(new HashSet<String>(Arrays.asList(guiFolder.getTags())));
			else
				folder.getTags().clear();

			folderDao.store(folder, transaction);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}

		return getFolder(session, guiFolder.getId());
	}

	@Override
	public GUIFolder create(GUIFolder newFolder, boolean inheritSecurity) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		try {
			String folderName = newFolder.getName().replace("/", "");

			FolderHistory transaction = new FolderHistory();
			transaction.setSession(session);
			transaction.setEvent(FolderEvent.CREATED.toString());

			Folder folderVO = new Folder();
			folderVO.setName(folderName);
			folderVO.setType(newFolder.getType());
			folderVO.setTenantId(session.getTenantId());

			Folder root = folderDao.findRoot(session.getTenantId());

			if (newFolder.getType() == Folder.TYPE_WORKSPACE)
				newFolder.setParentId(root.getId());

			Folder parent = folderDao.findById(newFolder.getParentId());
			if (parent.getFoldRef() != null)
				folderVO.setParentId(parent.getFoldRef());

			Folder f = null;
			if (newFolder.getType() == Folder.TYPE_WORKSPACE)
				f = folderDao.create(root, folderVO, inheritSecurity, transaction);
			else
				f = folderDao.create(folderDao.findById(newFolder.getParentId()), folderVO, inheritSecurity,
						transaction);

			if (f == null)
				throw new Exception("Folder not stored");

			return getFolder(session, f.getId());
		} catch (Throwable t) {
			return (GUIFolder) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public GUIFolder createAlias(long parentId, long foldRef) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);

			// Prepare the transaction
			FolderHistory transaction = new FolderHistory();
			transaction.setSession(session);
			transaction.setEvent(FolderEvent.CREATED.toString());

			// Finally create the alias
			Folder f = folderDao.createAlias(parentId, foldRef, transaction);

			return getFolder(session, f.getId());
		} catch (Throwable t) {
			return (GUIFolder) ServiceUtil.throwServerException(session, log, t);
		}
	}

	private boolean saveRules(Session session, Folder folder, long userId, GUIRight[] rights) throws Exception {
		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);

		boolean sqlerrors = false;
		try {
			log.info("Applying {} rights to folder {}", (rights != null ? rights.length : 0), folder.getId());

			folder.setSecurityRef(null);
			sqlerrors = false;
			Set<FolderGroup> grps = new HashSet<FolderGroup>();
			for (GUIRight right : rights) {
				boolean isAdmin = right.getEntityId() == 1;
				FolderGroup fg = null;
				if (right.isRead()) {
					fg = new FolderGroup();
					fg.setGroupId(right.getEntityId());
				}

				if (fg == null)
					continue;

				grps.add(fg);

				if (isAdmin || right.isPrint())
					fg.setPrint(1);
				else
					fg.setPrint(0);

				if (isAdmin || right.isWrite())
					fg.setWrite(1);
				else
					fg.setWrite(0);

				if (isAdmin || right.isAdd())
					fg.setAdd(1);
				else
					fg.setAdd(0);

				if (isAdmin || right.isSecurity())
					fg.setSecurity(1);
				else
					fg.setSecurity(0);

				if (isAdmin || right.isImmutable())
					fg.setImmutable(1);
				else
					fg.setImmutable(0);

				if (isAdmin || right.isDelete())
					fg.setDelete(1);
				else
					fg.setDelete(0);

				if (isAdmin || right.isRename())
					fg.setRename(1);
				else
					fg.setRename(0);

				if (isAdmin || right.isImport())
					fg.setImport(1);
				else
					fg.setImport(0);

				if (isAdmin || right.isExport())
					fg.setExport(1);
				else
					fg.setExport(0);

				if (isAdmin || right.isArchive())
					fg.setArchive(1);
				else
					fg.setArchive(0);

				if (isAdmin || right.isWorkflow())
					fg.setWorkflow(1);
				else
					fg.setWorkflow(0);

				if (isAdmin || right.isSign())
					fg.setSign(1);
				else
					fg.setSign(0);

				if (isAdmin || right.isDownload())
					fg.setDownload(1);
				else
					fg.setDownload(0);

				if (isAdmin || right.isCalendar())
					fg.setCalendar(1);
				else
					fg.setCalendar(0);

				if (isAdmin || right.isSubscription())
					fg.setSubscription(1);
				else
					fg.setSubscription(0);

				if (isAdmin || right.isPassword())
					fg.setPassword(1);
				else
					fg.setPassword(0);

				if (isAdmin || right.isMove())
					fg.setMove(1);
				else
					fg.setMove(0);

				if (isAdmin || right.isEmail())
					fg.setEmail(1);
				else
					fg.setEmail(0);

				if (isAdmin || right.isAutomation())
					fg.setAutomation(1);
				else
					fg.setAutomation(0);

				if (isAdmin || right.isStorage())
					fg.setStorage(1);
				else
					fg.setStorage(0);
			}

			folder.getFolderGroups().clear();
			folder.getFolderGroups().addAll(grps);

			// Add a folder history entry
			FolderHistory history = new FolderHistory();
			history.setEvent(FolderEvent.PERMISSION.toString());
			history.setSession(session);
			fdao.store(folder, history);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
		}
		return !sqlerrors;
	}

	@Override
	public void paste(long[] docIds, long folderId, String action) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);

			Folder folder = fdao.findFolder(folderId);

			if (!fdao.isWriteEnabled(folder.getId(), session.getUserId()))
				throw new AccessControlException("Cannot write in folder " + folder.getName());

			if (action.equals(Clipboard.CUT))
				cut(session, docIds, folder.getId());
			else if (action.equals(Clipboard.COPY))
				copy(session, docIds, folder.getId());
		} catch (Throwable t) {
			log.error("Exception moving documents: " + t.getMessage(), t);
			ServiceUtil.throwServerException(session, null, t);
		}
	}

	private void cut(Session session, long[] docIds, long folderId) throws ServerException {
		DocumentManager docManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

		try {
			Folder selectedFolderFolder = folderDao.findById(folderId);

			for (long id : docIds) {
				Document doc = docDao.findById(id);

				// The MOVE permission must be granted in the source folder
				if (!folderDao.isPermissionEnabled(Permission.MOVE, doc.getFolder().getId(), session.getUserId()))
					throw new AccessControlException(String.format("User %s has not the MOVE permission on folder %s",
							session.getUsername(), doc.getFolder().getName()));

				// Create the document history event
				DocumentHistory transaction = new DocumentHistory();
				transaction.setSession(session);

				// Check if the selected document is a shortcut
				if (doc.getDocRef() != null) {
					if (doc.getFolder().getId() != selectedFolderFolder.getId()) {
						transaction.setEvent(DocumentEvent.SHORTCUT_MOVED.toString());
						docManager.moveToFolder(doc, selectedFolderFolder, transaction);
					} else
						continue;
				}

				// The document must be not immutable
				if (doc.getImmutable() == 1 && !transaction.getUser().isMemberOf(Group.GROUP_ADMIN)) {
					continue;
				}

				// The document must be not locked
				if (doc.getStatus() != AbstractDocument.DOC_UNLOCKED
						|| doc.getExportStatus() != AbstractDocument.EXPORT_UNLOCKED) {
					continue;
				}

				docManager.moveToFolder(doc, selectedFolderFolder, transaction);
			}
		} catch (AccessControlException t) {
			ServiceUtil.throwServerException(session, log, t);
		} catch (Throwable t) {
			log.error("Exception moving documents: " + t.getMessage(), t);
			ServiceUtil.throwServerException(session, null, t);
		}
	}

	private void copy(Session session, long[] docIds, long folderId) throws ServerException {
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		DocumentManager docManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		try {
			Folder selectedFolderFolder = folderDao.findById(folderId);
			for (long id : docIds) {
				Document doc = docDao.findById(id);
				// Create the document history event
				DocumentHistory transaction = new DocumentHistory();
				transaction.setSession(session);
				transaction.setEvent(DocumentEvent.STORED.toString());
				transaction.setComment("");

				if (doc.getDocRef() == null) {
					docManager.copyToFolder(doc, selectedFolderFolder, transaction);
				} else {
					if (doc.getFolder().getId() != selectedFolderFolder.getId()) {
						transaction.setEvent(DocumentEvent.SHORTCUT_STORED.toString());
						docManager.copyToFolder(doc, selectedFolderFolder, transaction);
					}
				}
			}
		} catch (AccessControlException t) {
			ServiceUtil.throwServerException(session, log, t);
		} catch (Throwable e) {
			log.error("Exception copying documents: " + e.getMessage(), e);
			ServiceUtil.throwServerException(session, null, e);
		}
	}

	@Override
	public void pasteAsAlias(long[] docIds, long folderId, String type) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		DocumentManager docManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		try {
			Folder selectedFolderFolder = folderDao.findFolder(folderId);

			for (long id : docIds) {
				Document doc = docDao.findById(id);
				// Create the document history event
				DocumentHistory transaction = new DocumentHistory();
				transaction.setSession(session);
				transaction.setEvent(DocumentEvent.SHORTCUT_STORED.toString());
				transaction.setComment("");

				if (doc.getFolder().getId() != selectedFolderFolder.getId())
					docManager.createAlias(doc, selectedFolderFolder, StringUtils.isNotEmpty(type) ? type : null,
							transaction);
			}
		} catch (AccessControlException t) {
			ServiceUtil.throwServerException(session, log, t);
		} catch (Throwable t) {
			log.error("Exception copying documents alias: " + t.getMessage(), t);
			ServiceUtil.throwServerException(session, null, t);
		}
	}

	@Override
	public GUIValue[] loadTemplates() throws ServerException {
		return new GUIValue[0];
	}

	@Override
	public void saveTemplates(GUIValue[] templates) throws ServerException {
		// Nothing to do
	}

	@Override
	public void applyTemplate(long folderId, long templateId, boolean inheritSecurity) throws ServerException {
		// Nothing to do
	}

	/**
	 * Updates the extended attributes of a folder on the basis of the user's
	 * input
	 * 
	 * @param folder The folder to update
	 * @param f The model to use
	 * 
	 * @throws PersistenceException error in the database
	 */
	private void updateExtendedAttributes(Folder folder, GUIFolder f) throws PersistenceException {
		if (f.getTemplateId() != null) {
			TemplateDAO templateDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
			Template template = templateDao.findById(f.getTemplateId());
			templateDao.initialize(template);

			folder.setTemplate(template);
			folder.setTemplateLocked(f.getTemplateLocked());
			folder.getAttributes().clear();

			if (f.getAttributes() != null && f.getAttributes().length > 0) {
				for (GUIAttribute attr : f.getAttributes()) {
					Attribute templateAttribute = template.getAttributes()
							.get(attr.getParent() != null ? attr.getParent() : attr.getName());
					// This control is necessary because, changing
					// the template, the values of the old template
					// attributes keys remains on the form value
					// manager,
					// so the GUIFolder contains also the old
					// template attributes keys that must be
					// skipped.
					if (templateAttribute == null)
						continue;

					Attribute extAttr = new Attribute();
					int templateType = templateAttribute.getType();
					int extAttrType = attr.getType();

					if (templateType != extAttrType) {
						// This check is useful to avoid errors
						// related to the old template
						// attributes keys that remains on the form
						// value manager
						if (attr.getValue().toString().trim().isEmpty() && templateType != 0) {
							if (templateType == Attribute.TYPE_INT || templateType == Attribute.TYPE_BOOLEAN) {
								extAttr.setIntValue(null);
							} else if (templateType == Attribute.TYPE_DOUBLE) {
								extAttr.setDoubleValue(null);
							} else if (templateType == Attribute.TYPE_DATE) {
								extAttr.setDateValue(null);
							}
						} else if (templateType == GUIAttribute.TYPE_DOUBLE) {
							extAttr.setValue(Double.parseDouble(attr.getValue().toString()));
						} else if (templateType == GUIAttribute.TYPE_INT) {
							extAttr.setValue(Long.parseLong(attr.getValue().toString()));
						} else if (templateType == GUIAttribute.TYPE_BOOLEAN) {
							extAttr.setValue(attr.getBooleanValue());
							extAttr.setType(Attribute.TYPE_BOOLEAN);
						} else if (templateType == GUIAttribute.TYPE_USER || templateType == GUIAttribute.TYPE_FOLDER) {
							extAttr.setIntValue(attr.getIntValue());
							extAttr.setStringValue(attr.getStringValue());
						}
					} else {
						if (templateType == Attribute.TYPE_INT) {
							if (attr.getValue() != null)
								extAttr.setIntValue((Long) attr.getValue());
							else
								extAttr.setIntValue(null);
						} else if (templateType == Attribute.TYPE_BOOLEAN) {
							if (attr.getBooleanValue() != null)
								extAttr.setValue(attr.getBooleanValue());
							else
								extAttr.setBooleanValue(null);
						} else if (templateType == Attribute.TYPE_DOUBLE) {
							if (attr.getValue() != null)
								extAttr.setDoubleValue((Double) attr.getValue());
							else
								extAttr.setDoubleValue(null);
						} else if (templateType == Attribute.TYPE_DATE) {
							if (attr.getValue() != null)
								extAttr.setDateValue((Date) attr.getValue());
							else
								extAttr.setDateValue(null);
						} else if (templateType == Attribute.TYPE_STRING) {
							if (attr.getValue() != null)
								extAttr.setStringValue((String) attr.getValue());
							else
								extAttr.setStringValue(null);
						} else if (templateType == Attribute.TYPE_USER || templateType == Attribute.TYPE_FOLDER) {
							if (attr.getValue() != null) {
								extAttr.setStringValue((String) attr.getStringValue());
								extAttr.setIntValue((Long) attr.getIntValue());
							} else {
								extAttr.setStringValue(null);
								extAttr.setIntValue(null);
							}
						}
					}

					extAttr.setParent(attr.getParent());
					extAttr.setDependsOn(attr.getDependsOn());
					extAttr.setLabel(templateAttribute.getLabel());
					extAttr.setType(templateType);
					extAttr.setPosition(templateAttribute.getPosition());
					extAttr.setMandatory(templateAttribute.getMandatory());
					extAttr.setHidden(templateAttribute.getHidden());
					extAttr.setMultiple(templateAttribute.getMultiple());

					folder.getAttributes().put(attr.getName(), extAttr);
				}
			}
		} else {
			folder.setTemplate(null);
			folder.getAttributes().clear();
		}
	}

	@Override
	public void restore(Long[] folderIds, long parentId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);

			for (Long foldId : folderIds) {
				if (foldId == null)
					continue;
				FolderHistory transaction = new FolderHistory();
				transaction.setSession(session);
				dao.restore(foldId, parentId, transaction);
			}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void deleteFromTrash(Long[] ids) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		if (ids == null || ids.length < 1)
			return;

		try {
			String idsStr = Arrays.asList(ids).toString().replace('[', '(').replace(']', ')');
			FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			dao.bulkUpdate("set ld_deleted=2 where ld_id in " + idsStr, (Map<String, Object>) null);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void applyTags(long parentId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			Runnable runnable = new Runnable() {

				@Override
				public void run() {
					FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
					FolderHistory transaction = new FolderHistory();
					transaction.setSession(session);
					try {
						fdao.applyTagsToTree(parentId, transaction);
					} catch (PersistenceException e) {
						throw new RuntimeException(e.getMessage(), e);
					}
				}

			};

			ServiceUtil.executeLongRunningOperation("Apply Tags", runnable, session);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void setFolderPagination(long folderId, Integer startRecord, Integer pageSize) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		if (pageSize != null && startRecord != null) {
			session.getDictionary().put(FoldersDataServlet.FOLDER_PAGE_SIZE + ":" + folderId, pageSize);
			session.getDictionary().put(FoldersDataServlet.FOLDER_START_RECORD + ":" + folderId, startRecord);
		} else {
			session.getDictionary().remove(FoldersDataServlet.FOLDER_PAGE_SIZE + ":" + folderId);
			session.getDictionary().remove(FoldersDataServlet.FOLDER_START_RECORD + ":" + folderId);
		}
	}

	@Override
	public void applyGridLayout(long folderId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			Runnable runnable = new Runnable() {

				@Override
				public void run() {
					/*
					 * Just apply the current security settings to the whole
					 * subtree
					 */
					FolderHistory history = new FolderHistory();
					history.setSession(session);
					FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
					try {
						fdao.applyGridToTree(folderId, history);
					} catch (PersistenceException e) {
						log.error(e.getMessage(), e);
					}
				}

			};

			ServiceUtil.executeLongRunningOperation("Apply Grid Layout", runnable, session);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void applyOCR(long parentId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			Runnable runnable = new Runnable() {

				@Override
				public void run() {
					FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
					FolderHistory transaction = new FolderHistory();
					transaction.setSession(session);
					try {
						fdao.applyOCRToTree(parentId, transaction);
					} catch (PersistenceException e) {
						throw new RuntimeException(e.getMessage(), e);
					}
				}

			};

			ServiceUtil.executeLongRunningOperation("Apply OCR", runnable, session);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void applyStorage(long parentId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			Runnable runnable = new Runnable() {

				@Override
				public void run() {
					try {
						ServiceUtil.checkPermission(Permission.STORAGE, session.getUser(), parentId);
						FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
						FolderHistory transaction = new FolderHistory();
						transaction.setSession(session);
						fdao.applyStorageToTree(parentId, transaction);
					} catch (Exception e) {
						throw new RuntimeException(e.getMessage(), e);
					}
				}

			};

			ServiceUtil.executeLongRunningOperation("Apply Storage", runnable, session);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void merge(long[] folderIds, long targetId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);

		try {
			if (!folderDao.isPermissionEnabled(Permission.ADD, targetId, session.getUserId()))
				throw new ServerException("Add Child rights not granted on the target folder");
			if (!folderDao.isPermissionEnabled(Permission.WRITE, targetId, session.getUserId()))
				throw new ServerException("Write rights not granted on the target folder");
			if (!folderDao.isPermissionEnabled(Permission.DELETE, targetId, session.getUserId()))
				throw new ServerException("Delete rights not granted on the target folder");
			
			Runnable runnable = new Runnable() {

				@Override
				public void run() {
					try {
						FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
						Folder target = fDao.findFolder(targetId);
						fDao.initialize(target);

						FolderHistory transaction = new FolderHistory();
						transaction.setSession(session);

						for (long sourceId : folderIds) {
							Folder source = fDao.findById(sourceId);
							fDao.merge(source, target, transaction);
						}
					} catch (Exception e) {
						throw new RuntimeException(e.getMessage(), e);
					}
				}
			};

			ServiceUtil.executeLongRunningOperation("Merge", runnable, session);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}
}