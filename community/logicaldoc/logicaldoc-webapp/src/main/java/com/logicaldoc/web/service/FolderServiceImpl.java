package com.logicaldoc.web.service;

import java.security.AccessControlException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.document.History;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderEvent;
import com.logicaldoc.core.folder.FolderGroup;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
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
import com.logicaldoc.web.util.ServiceUtil;

/**
 * Implementation of the FolderService
 * 
 * @author Matteo Caruso - Logical Objects
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

			if (!fdao.updateSecurityRef(folderId, rightsFolderId, transaction))
				throw new Exception("Error updating the database");

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
				FolderHistory history = new FolderHistory();
				history.setSession(session);
				history.setEvent(FolderEvent.PERMISSION.toString());

				fdao.applyRithtToTree(folder.getId(), history);
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
			FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			FolderHistory transaction = new FolderHistory();
			transaction.setSession(session);
			fdao.applyMetadataToTree(parentId, transaction);
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

	public static GUIFolder fromFolder(Folder folder){
		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		
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
		f.setQuotaThreshold(folder.getQuotaThreshold());
		f.setQuotaAlertRecipients(folder.getQuotaAlertRecipientsAsList().toArray(new String[0]));
		f.setPathExtended(dao.computePathExtended(folder.getId()));
		
		if (f.isWorkspace()) {
			SequenceDAO seqDao = (SequenceDAO) Context.get().getBean(SequenceDAO.class);
			f.setDocumentsTotal(seqDao.getCurrentValue("wsdocs", folder.getId(), folder.getTenantId()));
			f.setSizeTotal(seqDao.getCurrentValue("wssize", folder.getId(), folder.getTenantId()));
		}

		if (folder.getSecurityRef() != null) {
			GUIFolder secRef = new GUIFolder();
			secRef.setId(folder.getSecurityRef());
			secRef.setPathExtended(dao.computePathExtended(folder.getSecurityRef()));
			f.setSecurityRef(secRef);
		}


		dao.initialize(folder);
			
	    if (folder.getTemplate() != null) {			
			f.setTemplateId(folder.getTemplate().getId());
			f.setTemplate(folder.getTemplate().getName());
			f.setTemplateLocked(folder.getTemplateLocked());
			GUIAttribute[] attributes = prepareGUIAttributes(folder.getTemplate(), folder);
			f.setAttributes(attributes);
		}

		if (folder.getTags() != null && folder.getTags().size() > 0)
			f.setTags(folder.getTagsAsWords().toArray(new String[folder.getTags().size()]));
		else
			f.setTags(new String[0]);
		
		/*
		 * Count the children
		 */
		f.setDocumentCount(dao
				.queryForInt("select count(ld_id) from ld_document where ld_deleted=0 and ld_folderid="
						+ folder.getId() + " and not ld_status=" + AbstractDocument.DOC_ARCHIVED));
		f.setSubfolderCount(dao
				.queryForInt("select count(ld_id) from ld_folder where not ld_id=ld_parentid and ld_deleted=0 and ld_parentid="
						+ folder.getId()));
		
		return f;
	}
	
	
	public static GUIFolder getFolder(Session session, long folderId) throws ServerException {
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

			// Check if it is an alias
			if (test.getFoldRef() != null) {
				folder = dao.findById(test.getFoldRef());
				// The alias rewrite some properties
				folder.setName(test.getName());
				folder.setDescription(test.getDescription());
				folder.setPosition(test.getPosition());
				folder.setFoldRef(test.getId());
				folder.setType(Folder.TYPE_ALIAS);
				dao.initialize(folder);
			} else {
				folder = test;
			}

			GUIFolder f = fromFolder(folder);

			if (session != null) {
				Set<Permission> permissions = dao.getEnabledPermissions(folder.getId(), session.getUserId());
				List<String> permissionsList = new ArrayList<String>();
				for (Permission permission : permissions)
					permissionsList.add(permission.toString());
				f.setPermissions(permissionsList.toArray(new String[permissionsList.size()]));
			}

			Folder ref = folder;
			if (folder.getSecurityRef() != null)
				ref = dao.findById(folder.getSecurityRef());

			int i = 0;
			GUIRight[] rights = new GUIRight[(ref != null && ref.getFolderGroups() != null) ? ref.getFolderGroups()
					.size() : 0];
			if (ref != null && ref.getFolderGroups() != null)
				for (FolderGroup fg : ref.getFolderGroups()) {
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

					rights[i] = right;
					i++;
				}
			f.setRights(rights);

			return f;
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
		}

		return null;
	}

	@Override
	public GUIFolder getFolder(long folderId, boolean computePath) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);

			GUIFolder folder = getFolder(session, folderId);
			if (folder == null)
				return null;

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
						path[j] = f;
					} else
						path[j] = getFolder(parent.getId(), false);
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
	public void copyFolders(long[] folderIds, long targetId, boolean foldersOnly, boolean inheritSecurity)
			throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			for (int i = 0; i < folderIds.length; i++) {
				copyFolder(session, folderIds[i], targetId, foldersOnly, inheritSecurity);
			}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	private void copyFolder(Session session, long folderId, long targetId, boolean foldersOnly, boolean inheritSecurity)
			throws Exception {
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

		folderDao.copy(folderToCopy, destParentFolder, foldersOnly, inheritSecurity, transaction);
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

		// Check delete permission on the folder parent of folderToMove
		Folder sourceParent = folderDao.findById(folderToMove.getParentId());
		boolean sourceParentDeleteEnabled = folderDao.isPermissionEnabled(Permission.DELETE, sourceParent.getId(),
				session.getUserId());
		if (!sourceParentDeleteEnabled)
			throw new SecurityException("No rights to delete folder");

		// Check addChild permission on destParentFolder
		boolean addchildEnabled = folderDao.isPermissionEnabled(Permission.ADD, destParentFolder.getId(),
				session.getUserId());
		if (!addchildEnabled)
			throw new SecurityException("AddChild Rights not granted to this user");

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
	public GUIFolder save(GUIFolder folder) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		try {
			Folder f = folderDao.findById(folder.getId());
			folderDao.initialize(f);

			FolderHistory transaction = new FolderHistory();
			transaction.setSession(session);

			String folderName = folder.getName().replace("/", "");

			if (folder.getFoldRef() != null) {
				// The user is editing an alias
				Folder alias = folderDao.findById(folder.getFoldRef());
				folderDao.initialize(alias);
				alias.setDescription(folder.getDescription());
				alias.setPosition(folder.getPosition());
				alias.setName(folderName);
				folderDao.store(alias);
			} else {
				// The user is editing a real folder
				f.setDescription(folder.getDescription());
				f.setPosition(folder.getPosition());
				f.setType(folder.getType());

				if (f.isWorkspace()) {
					f.setStorage(folder.getStorage());
					f.setMaxVersions(folder.getMaxVersions());
				}

				if (f.getName().trim().equals(folderName)) {
					f.setName(folderName.trim());
					transaction.setEvent(FolderEvent.CHANGED.toString());
				} else {
					f.setName(folderName.trim());
					transaction.setEvent(FolderEvent.RENAMED.toString());
				}
			}

			f.setTemplateLocked(folder.getTemplateLocked());
			f.setQuotaDocs(folder.getQuotaDocs());
			f.setQuotaSize(folder.getQuotaSize());
			f.setQuotaThreshold(folder.getQuotaThreshold());
			f.setQuotaAlertRecipients(folder.getQuotaAlertRecipientsAsString());
			f.setColor(folder.getColor());

			updateExtendedAttributes(f, folder);

			if (folder.getTags() != null && folder.getTags().length > 0)
				f.setTagsFromWords(new HashSet<String>(Arrays.asList(folder.getTags())));
			else
				f.getTags().clear();

			folderDao.store(f, transaction);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}

		return getFolder(session, folder.getId());
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
			log.info("Applying " + (rights != null ? rights.length : 0) + " rights to folder " + folder.getId());

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

		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);

		Folder folder = fdao.findFolder(folderId);

		if (!fdao.isWriteEnabled(folder.getId(), session.getUserId()))
			throw new AccessControlException("Cannot write in folder " + folder.getName());

		if (action.equals(Clipboard.CUT))
			cut(session, docIds, folder.getId());
		else if (action.equals(Clipboard.COPY))
			copy(session, docIds, folder.getId());
	}

	private void cut(Session session, long[] docIds, long folderId) throws ServerException {
		DocumentManager docManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Folder selectedFolderFolder = folderDao.findById(folderId);

		try {
			for (long id : docIds) {
				Document doc = docDao.findById(id);

				// The delete permission must be granted in the source folder
				if (!folderDao.isPermissionEnabled(Permission.DELETE, doc.getFolder().getId(), session.getUserId()))
					throw new AccessControlException(String.format(
							"User %s has not the DELETE permission on folder %s", session.getUsername(), doc
									.getFolder().getName()));

				// Create the document history event
				History transaction = new History();
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
				if (doc.getImmutable() == 1 && !transaction.getUser().isMemberOf("admin")) {
					continue;
				}

				// The document must be not locked
				if (doc.getStatus() != Document.DOC_UNLOCKED || doc.getExportStatus() != Document.EXPORT_UNLOCKED) {
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
		Folder selectedFolderFolder = folderDao.findById(folderId);
		DocumentManager docManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		try {
			for (long id : docIds) {
				Document doc = docDao.findById(id);
				// Create the document history event
				History transaction = new History();
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
		Folder selectedFolderFolder = folderDao.findFolder(folderId);
		DocumentManager docManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		try {
			for (long id : docIds) {
				Document doc = docDao.findById(id);
				// Create the document history event
				History transaction = new History();
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

	}

	@Override
	public void applyTemplate(long folderId, long templateId, boolean inheritSecurity) throws ServerException {

	}

	/**
	 * Updates the extended attributes of a folder on the basis of the user's
	 * input
	 * 
	 * @param folder The folder to update
	 * @param f The model to use
	 */
	private void updateExtendedAttributes(Folder folder, GUIFolder f) {
		if (f.getTemplateId() != null) {
			TemplateDAO templateDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
			Template template = templateDao.findById(f.getTemplateId());
			folder.setTemplate(template);
			folder.setTemplateLocked(f.getTemplateLocked());
			folder.getAttributes().clear();

			if (f.getAttributes() != null && f.getAttributes().length > 0) {
				for (GUIAttribute attr : f.getAttributes()) {
					Attribute templateAttribute = template.getAttributes().get(attr.getName());
					// This control is necessary because, changing
					// the template, the values of the old template
					// attributes keys remains on the form value
					// manager,
					// so the GUIDocument contains also the old
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
						} else if (templateType == GUIAttribute.TYPE_USER) {
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
						} else if (templateType == Attribute.TYPE_USER) {
							if (attr.getValue() != null) {
								extAttr.setStringValue((String) attr.getStringValue());
								extAttr.setIntValue((Long) attr.getIntValue());
							} else {
								extAttr.setStringValue(null);
								extAttr.setIntValue(null);
							}
						}
					}

					extAttr.setLabel(attr.getLabel());
					extAttr.setType(templateType);
					extAttr.setPosition(attr.getPosition());
					extAttr.setMandatory(attr.isMandatory() ? 1 : 0);

					folder.getAttributes().put(attr.getName(), extAttr);
				}
			}
		} else {
			folder.setTemplate(null);
			folder.getAttributes().clear();
		}
	}

	private static GUIAttribute[] prepareGUIAttributes(Template template, Folder folder) {
		try {
			GUIAttribute[] attributes = new GUIAttribute[template.getAttributeNames().size()];
			int i = 0;
			for (String attrName : template.getAttributeNames()) {
				Attribute extAttr = template.getAttributes().get(attrName);
				GUIAttribute att = new GUIAttribute();
				att.setName(attrName);
				att.setPosition(extAttr.getPosition());
				att.setLabel(extAttr.getLabel());
				att.setMandatory(extAttr.getMandatory() == 1);
				att.setEditor(extAttr.getEditor());

				// If the case, populate the options
				if (att.getEditor() == Attribute.EDITOR_LISTBOX) {
					String buf = (String) extAttr.getStringValue();
					List<String> list = new ArrayList<String>();
					if (buf != null) {
						StringTokenizer st = new StringTokenizer(buf, ",");
						while (st.hasMoreElements()) {
							String val = (String) st.nextElement();
							if (!list.contains(val))
								list.add(val);
						}
					}
					att.setOptions(list.toArray(new String[0]));
				}

				if (folder != null) {
					if (folder.getValue(attrName) != null)
						att.setValue(folder.getValue(attrName));
				} else
					att.setValue(extAttr.getValue());

				if (att.getValue() instanceof Date)
					att.setValue(ServiceUtil.convertToDate((Date) att.getValue()));

				att.setType(extAttr.getType());

				attributes[i] = att;
				i++;
			}
			return attributes;
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			return null;
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
			dao.bulkUpdate("set ld_deleted=2 where ld_id in " + idsStr, null);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void applyTags(long parentId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			FolderHistory transaction = new FolderHistory();
			transaction.setSession(session);
			fdao.applyTagsToTree(parentId, transaction);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}
}