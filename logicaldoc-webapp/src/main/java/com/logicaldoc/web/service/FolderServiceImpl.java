package com.logicaldoc.web.service;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Base64;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.support.rowset.SqlRowSet;

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
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.AccessControlEntry;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.sequence.SequenceDAO;
import com.logicaldoc.gui.common.client.AccessDeniedException;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.frontend.client.clipboard.Clipboard;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.web.data.FoldersDataServlet;

/**
 * Implementation of the FolderService
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class FolderServiceImpl extends AbstractRemoteService implements FolderService {

	private static final String FOLDER = "Folder ";

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(FolderServiceImpl.class);

	@Override
	public GUIFolder inheritACL(long folderId, long rightsFolderId) throws ServerException {
		Session session = validateSession();

		/*
		 * Just apply the current security settings to the whole subtree
		 */
		FolderHistory transaction = new FolderHistory();
		transaction.setSession(session);

		try {
			FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			fdao.updateSecurityRef(folderId, rightsFolderId, transaction);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}

		return getFolder(session, folderId);
	}

	@Override
	public void saveACL(GUIFolder folder, boolean subtree) throws ServerException {
		Session session = validateSession();

		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		try {
			Folder f = fdao.findById(folder.getId());
			fdao.initialize(f);

			if (subtree) {
				/*
				 * Just apply the current security settings to the whole subtree
				 */
				executeLongRunningOperation("Apply Rights to Tree", () -> {
					FolderHistory history = new FolderHistory();
					history.setSession(session);
					history.setEvent(FolderEvent.PERMISSION.toString());
					try {
						fdao.applySecurityToTree(folder.getId(), history);
					} catch (PersistenceException e) {
						log.error(e.getMessage(), e);
					}
				}, session);
			} else {
				saveACL(session, f, folder.getRights());
			}
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void applyMetadata(long parentId) throws ServerException {
		Session session = validateSession();

		executeLongRunningOperation("Apply Folder Metadata", () -> {
			FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			FolderHistory transaction = new FolderHistory();
			transaction.setSession(session);

			try {
				fdao.applyMetadataToTree(parentId, transaction);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
		}, session);
	}

	@Override
	public void delete(final long[] folderIds) throws ServerException {
		Session session = validateSession();
		for (int i = 0; i < folderIds.length; i++)
			try {
				delete(session, folderIds[i]);
			} catch (PersistenceException | PermissionException e) {
				throwServerException(session, log, e);
			}
	}

	private void delete(Session session, final long folderId) throws PermissionException, PersistenceException {
		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		if (!dao.isPermissionEnabled(Permission.DELETE, folderId, session.getUserId()))
			throw new PermissionException(session.getUsername(), FOLDER + folderId, Permission.DELETE);

		// Add a folder history entry
		FolderHistory transaction = new FolderHistory();
		transaction.setSession(session);
		transaction.setEvent(FolderEvent.DELETED.toString());
		dao.deleteTree(folderId, PersistentObject.DELETED_CODE_DEFAULT, transaction);
	}

	public GUIFolder fromFolder(Folder folder, boolean computePath) throws PersistenceException {
		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		dao.initialize(folder);

		GUIFolder guiFolder = new GUIFolder();
		guiFolder.setId(folder.getId());
		guiFolder.setName(folder.getId() != Constants.DOCUMENTS_FOLDERID ? folder.getName() : "/");
		guiFolder.setParentId(folder.getParentId());
		guiFolder.setDescription(folder.getDescription());
		guiFolder.setCreation(folder.getCreation());
		guiFolder.setCreator(folder.getCreator());
		guiFolder.setCreatorId(folder.getCreatorId());
		guiFolder.setType(folder.getType());
		guiFolder.setPosition(folder.getPosition());
		guiFolder.setQuotaDocs(folder.getQuotaDocs());
		guiFolder.setQuotaSize(folder.getQuotaSize());
		guiFolder.setFoldRef(folder.getFoldRef());
		guiFolder.setStorage(folder.getStorage());
		guiFolder.setMaxVersions(folder.getMaxVersions());
		guiFolder.setColor(folder.getColor());
		guiFolder.setTile(folder.getTile());
		guiFolder.setGrid(folder.getGrid());
		guiFolder.setQuotaThreshold(folder.getQuotaThreshold());
		guiFolder.setQuotaAlertRecipients(folder.getQuotaAlertRecipientsAsList().toArray(new String[0]));
		guiFolder.setOcrTemplateId(folder.getOcrTemplateId());
		guiFolder.setBarcodeTemplateId(folder.getBarcodeTemplateId());
		if (computePath)
			guiFolder.setPathExtended(dao.computePathExtended(folder.getId()));

		if (guiFolder.isWorkspace()) {
			SequenceDAO seqDao = (SequenceDAO) Context.get().getBean(SequenceDAO.class);
			guiFolder.setDocumentsTotal(seqDao.getCurrentValue("wsdocs", folder.getId(), folder.getTenantId()));
			guiFolder.setSizeTotal(seqDao.getCurrentValue("wssize", folder.getId(), folder.getTenantId()));
		}

		if (folder.getSecurityRef() != null) {
			GUIFolder secRef = new GUIFolder();
			secRef.setId(folder.getSecurityRef());
			if (computePath)
				secRef.setPathExtended(dao.computePathExtended(folder.getSecurityRef()));
			guiFolder.setSecurityRef(secRef);
		}

		if (folder.getTemplate() != null) {
			guiFolder.setTemplateId(folder.getTemplate().getId());
			guiFolder.setTemplate(folder.getTemplate().getName());
			guiFolder.setTemplateLocked(folder.getTemplateLocked());
			GUIAttribute[] attributes = prepareGUIAttributes(folder.getTemplate(), folder);
			guiFolder.setAttributes(attributes);
		}

		if (folder.getTags() != null && !folder.getTags().isEmpty())
			guiFolder.setTags(folder.getTagsAsWords().toArray(new String[folder.getTags().size()]));
		else
			guiFolder.setTags(new String[0]);

		guiFolder.setColor(folder.getColor());
		return guiFolder;
	}

	@Override
	public long[] computeStats(long folderId) throws ServerException {
		Session session = validateSession();
		try {
			long[] docs = countDocsInTree(folderId);
			return new long[] { docs[0], countSubfoldersInTree(folderId), docs[1] };
		} catch (PersistenceException e) {
			return (long[]) throwServerException(session, log, e);
		}
	}

	/**
	 * Counts the documents inside a tree and also calculate the total size
	 * 
	 * @param folderId identifier of the tree's root
	 * 
	 * @return first element is the documents count, second element is the total
	 *         size
	 * 
	 * @throws PersistenceException Error in the database layer
	 */
	private static long[] countDocsInTree(long folderId) throws PersistenceException {
		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Folder root = dao.findById(folderId);
		String pathPrefix = root.getPath();

		SqlRowSet resultSet = dao.queryForRowSet(
				"select count(D.ld_id), sum(D.ld_filesize) from ld_document D, ld_folder F where D.ld_deleted=0 and F.ld_deleted=0 and D.ld_folderid=F.ld_id and (F.ld_id="
						+ folderId + " or F.ld_path like '" + pathPrefix + "/%') " + " and not ld_status="
						+ AbstractDocument.DOC_ARCHIVED,
				null);
		long[] stats = new long[] { 0L, 0L };
		if (resultSet.next()) {
			stats[0] = resultSet.getLong(1);
			stats[1] = resultSet.getLong(2);
		}
		return stats;
	}

	private static long countSubfoldersInTree(long folderId) throws PersistenceException {
		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Folder root = dao.findById(folderId);
		String pathPrefix = root.getPath();

		return dao.queryForLong("select count(ld_id) from ld_folder where ld_deleted=0 and (ld_parentid=" + folderId
				+ " or ld_path like '" + pathPrefix + "/%')");
	}

	private static int countDirectDocs(long folderId) throws PersistenceException {
		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		return dao.queryForInt("select count(ld_id) from ld_document where ld_deleted=0 and ld_folderid=" + folderId
				+ " and not ld_status=" + AbstractDocument.DOC_ARCHIVED);
	}

	private static int countDirectSubfolders(long folderId) throws PersistenceException {
		int count = 0;
		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		count = dao.queryForInt(
				"select count(ld_id) from ld_folder where not ld_id=ld_parentid and ld_deleted=0 and ld_parentid="
						+ folderId);
		return count;
	}

	public GUIFolder getFolder(Session session, long folderId) throws ServerException {
		return getFolder(session, folderId, false);
	}

	public GUIFolder getFolder(Session session, long folderId, boolean computePath) throws ServerException {
		if (session != null)
			validateSession(session.getSid());

		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);

		try {
			if (session != null)
				checkPermission(Permission.READ, session.getUser(), folderId);

			Folder folder = null;
			Folder test = dao.findById(folderId);
			if (test == null)
				throw new ServerException("Unexisting folder " + folderId);

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
				guiFolder.setTile(test.getTile());
				guiFolder.setPosition(test.getPosition());
				guiFolder.setFoldRef(test.getFoldRef());
				guiFolder.setId(test.getId());
				guiFolder.setType(Folder.TYPE_ALIAS);

				setSecurityRef(session, test, guiFolder);
			} else {
				folder = test;
				guiFolder = fromFolder(folder, computePath);
			}

			setAllowedPermissions(session, folderId, guiFolder);

			Folder securityRef = folder;
			if (test.getSecurityRef() != null)
				securityRef = dao.findById(test.getSecurityRef());
			dao.initialize(securityRef);

			setACL(securityRef, guiFolder);
			return guiFolder;
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		return null;
	}

	private void setSecurityRef(Session session, Folder test, GUIFolder guiFolder) throws ServerException {
		if (test.getSecurityRef() != null)
			guiFolder.setSecurityRef(getFolder(session, test.getSecurityRef()));
		else
			guiFolder.setSecurityRef(null);
	}

	private static void setACL(Folder securityRef, GUIFolder guiFolder) {
		int i = 0;
		GUIAccessControlEntry[] rights = new GUIAccessControlEntry[(securityRef != null
				&& securityRef.getAccessControlList() != null) ? securityRef.getAccessControlList().size() : 0];
		if (securityRef != null && securityRef.getAccessControlList() != null)
			for (AccessControlEntry ace : securityRef.getAccessControlList()) {
				GUIAccessControlEntry guiAce = new GUIAccessControlEntry();
				guiAce.setEntityId(ace.getGroupId());
				guiAce.setAdd(ace.getAdd() == 1);
				guiAce.setWrite(ace.getWrite() == 1);
				guiAce.setSecurity(ace.getSecurity() == 1);
				guiAce.setImmutable(ace.getImmutable() == 1);
				guiAce.setDelete(ace.getDelete() == 1);
				guiAce.setRename(ace.getRename() == 1);
				guiAce.setImport(ace.getImport() == 1);
				guiAce.setExport(ace.getExport() == 1);
				guiAce.setSign(ace.getSign() == 1);
				guiAce.setArchive(ace.getArchive() == 1);
				guiAce.setWorkflow(ace.getWorkflow() == 1);
				guiAce.setDownload(ace.getDownload() == 1);
				guiAce.setCalendar(ace.getCalendar() == 1);
				guiAce.setSubscription(ace.getSubscription() == 1);
				guiAce.setPassword(ace.getPassword() == 1);
				guiAce.setMove(ace.getMove() == 1);
				guiAce.setEmail(ace.getEmail() == 1);
				guiAce.setAutomation(ace.getAutomation() == 1);
				guiAce.setStorage(ace.getStorage() == 1);

				rights[i] = guiAce;
				i++;
			}
		guiFolder.setRights(rights);
	}

	private static void setAllowedPermissions(Session session, long folderId, GUIFolder guiFolder)
			throws PersistenceException {
		if (session != null) {
			FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			Set<Permission> permissions = dao.getEnabledPermissions(folderId, session.getUserId());
			guiFolder.setAllowedPermissions(new GUIAccessControlEntry(
					permissions.stream().map(p -> p.name().toLowerCase()).toList().toArray(new String[0])));
		}
	}

	@Override
	public GUIFolder getFolder(long folderId, boolean computePath, boolean computeDocs, boolean computeSubfolders)
			throws ServerException {
		Session session = validateSession();

		GUIFolder folder = getFolder(session, folderId);
		if (folder == null)
			return null;

		try {
			if (computeDocs)
				folder.setDocumentCount(countDirectDocs(folder.getId()));
			if (computeSubfolders)
				folder.setSubfolderCount(countDirectSubfolders(folder.getId()));
			if (computePath)
				folder.setPath(computePath(folderId, session.getTenantId(), computeSubfolders));
			return folder;
		} catch (PersistenceException e) {
			return (GUIFolder) throwServerException(session, log, e);
		}
	}

	private GUIFolder[] computePath(long folderId, long tenantId, boolean computeSubfolders)
			throws PersistenceException, ServerException {
		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		String pathExtended = dao.computePathExtended(folderId);

		StringTokenizer st = new StringTokenizer(pathExtended, "/", false);
		int elements = st.countTokens();
		GUIFolder[] path = new GUIFolder[elements];
		Folder parent = dao.findRoot(tenantId);
		int j = 0;
		while (st.hasMoreTokens()) {
			String text = st.nextToken();
			List<Folder> list = dao.findByName(parent, text, null, true);
			if (list.isEmpty())
				return new GUIFolder[0];

			if (parent.getId() == Folder.ROOTID || parent.getId() == parent.getParentId()) {
				GUIFolder f = new GUIFolder(parent.getId());
				f.setName("/");
				f.setParentId(parent.getId());
				if (computeSubfolders)
					f.setSubfolderCount(countDirectSubfolders(f.getId()));
				path[j] = f;
			} else
				path[j] = getFolder(parent.getId(), false, false, computeSubfolders);
			parent = list.get(0);
			j++;
		}

		return path;
	}

	@Override
	public void copyFolders(long[] folderIds, long targetId, boolean foldersOnly, String securityOption,
			GUIFolder model) throws ServerException {
		Session session = validateSession();

		try {
			for (int i = 0; i < folderIds.length; i++) {
				copyFolder(session, folderIds[i], targetId, foldersOnly, securityOption, model);
			}
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	private void copyFolder(Session session, long folderId, long targetId, boolean foldersOnly, String securityOption,
			GUIFolder model) throws PersistenceException, ServerException {
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
		Session session = validateSession();

		try {
			for (long folderId : folderIds) {
				move(session, folderId, targetId);
			}
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	private void move(Session session, long folderId, long targetId) throws PersistenceException {
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
		Session session = validateSession();

		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);

		try {
			List<Folder> folders = dao.findByNameAndParentId(name, dao.findById(folderId).getParentId());
			if (!folders.isEmpty() && folders.get(0).getId() != folderId) {
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
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public GUIFolder save(GUIFolder guiFolder) throws ServerException {
		Session session = validateSession();

		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);

		try {
			Folder folder = folderDao.findById(guiFolder.getId());
			folderDao.initialize(folder);

			FolderHistory saveTransaction = new FolderHistory();
			saveTransaction.setSession(session);

			FolderHistory renameTransaction = null;

			String folderName = guiFolder.getName().replace("/", "");

			if (guiFolder.getFoldRef() != null) {
				// The user is editing an alias
				folder.setName(folderName);
				folder.setPosition(guiFolder.getPosition());
				folder.setColor(guiFolder.getColor());
				folderDao.store(folder);

				folder = folderDao.findById(folder.getFoldRef());
				folderDao.initialize(folder);
			} else {
				// The user is editing a real folder
				folder.setType(guiFolder.getType());
				folder.setStorage(guiFolder.getStorage());
				folder.setPosition(guiFolder.getPosition());
				folder.setColor(guiFolder.getColor());

				if (folder.isWorkspace()) {
					folder.setMaxVersions(guiFolder.getMaxVersions());
				}

				saveTransaction.setEvent(FolderEvent.CHANGED.toString());

				if (!folder.getName().trim().equals(folderName)) {
					folder.setName(folderName.trim());

					renameTransaction = new FolderHistory();
					renameTransaction.setEvent(FolderEvent.RENAMED.toString());
					renameTransaction.setFilenameOld(folder.getName());
					renameTransaction.setPathOld(folderDao.computePathExtended(folder.getId()));
					renameTransaction.setSession(session);
					renameTransaction.setNotifyEvent(true);
				}

				folder.setName(folderName);
			}

			folder.setDescription(guiFolder.getDescription());
			folder.setTile(guiFolder.getTile());
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
				folder.setTagsFromWords(new HashSet<>(Arrays.asList(guiFolder.getTags())));
			else
				folder.getTags().clear();

			folderDao.store(folder, saveTransaction);
			if (renameTransaction != null)
				folderDao.saveFolderHistory(folder, renameTransaction);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}

		return getFolder(session, guiFolder.getId());
	}

	@Override
	public GUIFolder create(GUIFolder newFolder, boolean inheritSecurity) throws ServerException {
		Session session = validateSession();

		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);

		String folderName = newFolder.getName().replace("/", "");

		FolderHistory transaction = new FolderHistory();
		transaction.setSession(session);
		transaction.setEvent(FolderEvent.CREATED.toString());

		Folder folderVO = new Folder();
		folderVO.setName(folderName);
		folderVO.setType(newFolder.getType());
		folderVO.setTenantId(session.getTenantId());

		Folder root = null;
		try {
			root = folderDao.findRoot(session.getTenantId());
		} catch (PersistenceException e) {
			return (GUIFolder) throwServerException(session, log, e);
		}

		if (newFolder.getType() == Folder.TYPE_WORKSPACE)
			newFolder.setParentId(root.getId());

		Folder f = null;
		try {
			Folder parent = folderDao.findById(newFolder.getParentId());
			if (parent.getFoldRef() != null)
				folderVO.setParentId(parent.getFoldRef());

			if (newFolder.getType() == Folder.TYPE_WORKSPACE)
				f = folderDao.create(root, folderVO, inheritSecurity, transaction);
			else
				f = folderDao.create(folderDao.findById(newFolder.getParentId()), folderVO, inheritSecurity,
						transaction);
		} catch (PersistenceException e) {
			return (GUIFolder) throwServerException(session, log, e);
		}

		if (f == null)
			throw new ServerException("Folder not stored");

		return getFolder(session, f.getId());

	}

	@Override
	public GUIFolder createAlias(long parentId, long foldRef) throws ServerException {
		Session session = validateSession();

		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);

		// Prepare the transaction
		FolderHistory transaction = new FolderHistory();
		transaction.setSession(session);
		transaction.setEvent(FolderEvent.CREATED.toString());

		// Finally create the alias
		Folder f;
		try {
			f = folderDao.createAlias(parentId, foldRef, transaction);
		} catch (PersistenceException e) {
			return (GUIFolder) throwServerException(session, log, e);
		}

		return getFolder(session, f.getId());

	}

	private int booleanToInt(boolean bool) {
		return bool ? 1 : 0;
	}

	private boolean saveACL(Session session, Folder folder, GUIAccessControlEntry[] rights)
			throws PersistenceException {
		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);

		boolean sqlerrors = false;
		log.info("Applying {} rights to folder {}", (rights != null ? rights.length : 0), folder.getId());

		folder.setSecurityRef(null);
		sqlerrors = false;
		Set<AccessControlEntry> grps = new HashSet<>();
		for (GUIAccessControlEntry right : rights) {
			AccessControlEntry fg = new AccessControlEntry();
			fg.setGroupId(right.getEntityId());
			grps.add(fg);

			fg.setRead(booleanToInt(right.isRead()));
			fg.setPrint(booleanToInt(right.isPrint()));
			fg.setWrite(booleanToInt(right.isWrite()));
			fg.setAdd(booleanToInt(right.isAdd()));
			fg.setSecurity(booleanToInt(right.isSecurity()));
			fg.setImmutable(booleanToInt(right.isImmutable()));
			fg.setDelete(booleanToInt(right.isDelete()));
			fg.setRename(booleanToInt(right.isRename()));
			fg.setImport(booleanToInt(right.isImport()));
			fg.setExport(booleanToInt(right.isExport()));
			fg.setArchive(booleanToInt(right.isArchive()));
			fg.setWorkflow(booleanToInt(right.isWorkflow()));
			fg.setSign(booleanToInt(right.isSign()));
			fg.setDownload(booleanToInt(right.isDownload()));
			fg.setCalendar(booleanToInt(right.isCalendar()));
			fg.setSubscription(booleanToInt(right.isSubscription()));
			fg.setPassword(booleanToInt(right.isPassword()));
			fg.setMove(booleanToInt(right.isMove()));
			fg.setEmail(booleanToInt(right.isEmail()));
			fg.setAutomation(booleanToInt(right.isAutomation()));
			fg.setStorage(booleanToInt(right.isStorage()));
			fg.setReadingreq(booleanToInt(right.isReadingreq()));
		}

		folder.getAccessControlList().clear();
		folder.getAccessControlList().addAll(grps);

		// Add a folder history entry
		FolderHistory history = new FolderHistory();
		history.setEvent(FolderEvent.PERMISSION.toString());
		history.setSession(session);
		fdao.store(folder, history);

		return !sqlerrors;
	}

	@Override
	public void paste(long[] docIds, long folderId, String action, boolean links, boolean notes, boolean security)
			throws ServerException {
		Session session = validateSession();

		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);

		Folder folder;
		try {
			folder = fdao.findFolder(folderId);

			if (!fdao.isWriteEnabled(folder.getId(), session.getUserId()))
				throw new ServerException("Cannot write in folder " + folder.getName());

			if (action.equals(Clipboard.CUT))
				cut(session, docIds, folder.getId());
			else if (action.equals(Clipboard.COPY))
				copy(session, docIds, folder.getId(), links, notes, security);
		} catch (PersistenceException e) {
			log.error("Exception moving documents: {}", e.getMessage(), e);
			throwServerException(session, null, e);
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
				checkPermission(Permission.MOVE, session.getUser(), doc.getFolder().getId());

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
				checkImmutable(doc);

				// The document must be not locked
				checkLocked(doc);

				docManager.moveToFolder(doc, selectedFolderFolder, transaction);
			}
		} catch (PermissionException | PersistenceException e) {
			log.error("Exception moving documents: {}", e.getMessage(), e);
			throwServerException(session, null, e);
		}

	}

	private void checkLocked(Document doc) throws PermissionException {
		if (doc.getStatus() != AbstractDocument.DOC_UNLOCKED
				|| doc.getExportStatus() != AbstractDocument.EXPORT_UNLOCKED) {
			throw new PermissionException("Document " + doc.getId() + " is locked");
		}
	}

	private void checkImmutable(Document doc) throws PermissionException {
		if (doc.getImmutable() == 1) {
			throw new PermissionException("Document " + doc.getId() + " is immutable");
		}
	}

	private void copy(Session session, long[] docIds, long folderId, boolean links, boolean notes, boolean security)
			throws ServerException {
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
					docManager.copyToFolder(doc, selectedFolderFolder, transaction, links, notes, security);
				} else {
					if (doc.getFolder().getId() != selectedFolderFolder.getId()) {
						transaction.setEvent(DocumentEvent.SHORTCUT_STORED.toString());
						docManager.copyToFolder(doc, selectedFolderFolder, transaction, false, false, false);
					}
				}
			}
		} catch (PersistenceException | IOException e) {
			log.error("Exception copying documents: {}", e.getMessage(), e);
			throwServerException(session, null, e);
		}
	}

	@Override
	public void pasteAsAlias(long[] docIds, long folderId, String type) throws ServerException {
		Session session = validateSession();

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
		} catch (PersistenceException e) {
			log.error("Exception copying documents alias: {}", e.getMessage(), e);
			throwServerException(session, null, e);
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
	 * @throws PersistenceException error at data layer
	 */
	private void updateExtendedAttributes(Folder folder, GUIFolder f) throws PersistenceException {
		if (f.getTemplateId() == null) {
			folder.setTemplate(null);
			folder.getAttributes().clear();
			return;
		}

		TemplateDAO templateDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		Template template = templateDao.findById(f.getTemplateId());
		templateDao.initialize(template);

		folder.setTemplate(template);
		folder.setTemplateLocked(f.getTemplateLocked());
		folder.getAttributes().clear();

		if (f.getAttributes() != null && f.getAttributes().length > 0) {
			for (GUIAttribute guiAttribute : f.getAttributes()) {
				Attribute templateAttribute = template.getAttributes()
						.get(guiAttribute.getParent() != null ? guiAttribute.getParent() : guiAttribute.getName());
				// This control is necessary because, changing
				// the template, the values of the old template
				// attributes keys remains on the form value
				// manager,
				// so the GUIFolder contains also the old
				// template attributes keys that must be
				// skipped.
				if (templateAttribute == null)
					continue;

				Attribute extAttr = newAttribute(guiAttribute, templateAttribute);

				folder.getAttributes().put(guiAttribute.getName(), extAttr);
			}
		}
	}

	private Attribute newAttribute(GUIAttribute guiAttribute, Attribute templateAttribute) {
		Attribute extAttr = new Attribute();
		int extAttrType = guiAttribute.getType();
		int currentTemplateExtAttrType = templateAttribute.getType();
		if (currentTemplateExtAttrType != extAttrType) {
			// This check is useful to avoid errors
			// related to the old template
			// attributes keys that remains on the form
			// value manager
			if (guiAttribute.getValue().toString().trim().isEmpty() && currentTemplateExtAttrType != 0) {
				extAttr.setValue(null);
			} else {
				switch (currentTemplateExtAttrType) {
				case GUIAttribute.TYPE_DOUBLE:
					extAttr.setValue(Double.parseDouble(guiAttribute.getValue().toString()));
					break;
				case GUIAttribute.TYPE_INT:
					extAttr.setValue(Long.parseLong(guiAttribute.getValue().toString()));
					break;
				case GUIAttribute.TYPE_BOOLEAN:
					extAttr.setValue(guiAttribute.getBooleanValue());
					extAttr.setType(Attribute.TYPE_BOOLEAN);
					break;
				case GUIAttribute.TYPE_USER, GUIAttribute.TYPE_FOLDER, GUIAttribute.TYPE_DOCUMENT:
					extAttr.setIntValue(guiAttribute.getIntValue());
					extAttr.setStringValue(guiAttribute.getStringValue());
					break;
				default:
					extAttr.setStringValue(guiAttribute.getStringValue());
				}
			}
		} else {
			switch (currentTemplateExtAttrType) {
			case Attribute.TYPE_USER, Attribute.TYPE_FOLDER, GUIAttribute.TYPE_DOCUMENT:
				if (guiAttribute.getValue() != null) {
					extAttr.setStringValue(guiAttribute.getStringValue());
					extAttr.setIntValue(guiAttribute.getIntValue());
				} else {
					extAttr.setStringValue(null);
					extAttr.setIntValue(null);
				}
				break;
			case Attribute.TYPE_BOOLEAN:
				extAttr.setValue(guiAttribute.getBooleanValue());
				break;
			case Attribute.TYPE_DATE:
				extAttr.setValue(fixDateForGUI(guiAttribute.getDateValue()));
				break;
			default:
				extAttr.setValue(guiAttribute.getValue());
				break;
			}
		}

		extAttr.setType(currentTemplateExtAttrType);

		extAttr.setParent(guiAttribute.getParent());
		extAttr.setDependsOn(guiAttribute.getDependsOn());
		extAttr.setLabel(templateAttribute.getLabel());
		extAttr.setPosition(templateAttribute.getPosition());
		extAttr.setMandatory(templateAttribute.getMandatory());
		extAttr.setHidden(templateAttribute.getHidden());
		extAttr.setMultiple(templateAttribute.getMultiple());

		return extAttr;
	}

	@Override
	public void restore(Long[] folderIds, long parentId) throws ServerException {
		Session session = validateSession();

		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);

		for (Long foldId : folderIds) {
			if (foldId == null)
				continue;
			FolderHistory transaction = new FolderHistory();
			transaction.setSession(session);
			try {
				dao.restore(foldId, parentId, transaction);
			} catch (PersistenceException e) {
				throwServerException(session, log, e);
			}
		}

	}

	@Override
	public void deleteFromTrash(Long[] ids) throws ServerException {
		Session session = validateSession();
		if (ids == null || ids.length < 1)
			return;

		String idsStr = Arrays.asList(ids).toString().replace('[', '(').replace(']', ')');
		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		try {
			dao.bulkUpdate("set ld_deleted=2 where ld_id in " + idsStr, (Map<String, Object>) null);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}

	}

	@Override
	public void applyTags(long parentId) throws ServerException {
		Session session = validateSession();

		executeLongRunningOperation("Apply Tags", () -> {
			FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			FolderHistory transaction = new FolderHistory();
			transaction.setSession(session);
			try {
				fdao.applyTagsToTree(parentId, transaction);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
		}, session);
	}

	@Override
	public void setFolderPagination(long folderId, Integer startRecord, Integer pageSize) throws ServerException {
		Session session = validateSession();

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
		Session session = validateSession();

		/*
		 * Just apply the current security settings to the whole subtree
		 */
		executeLongRunningOperation("Apply Grid Layout", () -> {
			FolderHistory history = new FolderHistory();
			history.setSession(session);
			FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			try {
				fdao.applyGridToTree(folderId, history);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
		}, session);
	}

	@Override
	public void applyOCR(long parentId) throws ServerException {
		Session session = validateSession();

		executeLongRunningOperation("Apply OCR", () -> {
			FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			FolderHistory transaction = new FolderHistory();
			transaction.setSession(session);
			try {
				fdao.applyOCRToTree(parentId, transaction);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
		}, session);
	}

	@Override
	public void applyStorage(long parentId) throws ServerException {
		Session session = validateSession();

		executeLongRunningOperation("Apply Storage", () -> {
			try {
				checkPermission(Permission.STORAGE, session.getUser(), parentId);
				FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
				FolderHistory transaction = new FolderHistory();
				transaction.setSession(session);
				fdao.applyStorageToTree(parentId, transaction);
			} catch (PersistenceException | AccessDeniedException e) {
				log.error(e.getMessage(), e);
			}
		}, session);
	}

	@Override
	public void merge(long[] folderIds, long targetId) throws ServerException {
		Session session = validateSession();

		checkPermission(Permission.ADD, session.getUser(), targetId);
		checkPermission(Permission.WRITE, session.getUser(), targetId);
		checkPermission(Permission.DELETE, session.getUser(), targetId);

		executeLongRunningOperation("Merge", () -> {
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
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
		}, session);
	}

	@Override
	public String readImage() throws ServerException {
		final Session session = validateSession();
		List<String> allowedExts = Arrays.asList("png", "gif", "jpg", "jpeg", "webp", "jfif");

		Map<String, File> uploadedFilesMap = getUploadedFiles(session.getSid());
		for (Map.Entry<String, File> entry : uploadedFilesMap.entrySet()) {
			String ext = FileUtil.getExtension(entry.getKey()).toLowerCase();
			if (!allowedExts.contains(ext))
				continue;
			StringBuilder sb = new StringBuilder("data:image/");
			sb.append(ext);
			sb.append(";base64,");

			try {
				sb.append(Base64.getEncoder().encodeToString(FileUtils.readFileToByteArray(entry.getValue())));
			} catch (IOException e) {
				throwServerException(session, log, e);
			}
			return sb.toString();
		}

		throw new ServerException(I18N.message("unsupportedformat", session.getUser().getLocale()));
	}
}