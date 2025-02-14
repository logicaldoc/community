package com.logicaldoc.webdav.resource.service;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import javax.servlet.http.HttpServletResponse;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.jackrabbit.webdav.DavException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Bookmark;
import com.logicaldoc.core.document.BookmarkDAO;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.document.Version;
import com.logicaldoc.core.document.VersionDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderEvent;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.store.Store;
import com.logicaldoc.util.Context;
import com.logicaldoc.webdav.context.ImportContext;
import com.logicaldoc.webdav.resource.model.Resource;
import com.logicaldoc.webdav.resource.model.ResourceImpl;
import com.logicaldoc.webdav.session.WebdavSession;

/**
 * Base implementation of a {@link ResourceService}
 * 
 * @author Sebastian Wenzky
 */
@Component("resourceService")
public class ResourceServiceImpl implements ResourceService {

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(ResourceServiceImpl.class);

	@javax.annotation.Resource(name = "DocumentDAO")
	private transient DocumentDAO documentDAO;

	@javax.annotation.Resource(name = "VersionDAO")
	private transient VersionDAO versionDAO;

	@javax.annotation.Resource(name = "FolderDAO")
	private transient FolderDAO folderDAO;

	@javax.annotation.Resource(name = "documentManager")
	private transient DocumentManager documentManager;

	@javax.annotation.Resource(name = "Store")
	private transient Store store;

	@javax.annotation.Resource(name = "UserDAO")
	private transient UserDAO userDAO;

	public void setUserDAO(UserDAO userDAO) {
		this.userDAO = userDAO;
	}

	public void setDocumentDAO(DocumentDAO documentDAO) {
		this.documentDAO = documentDAO;
	}

	public void setDocumentManager(DocumentManager documentManager) {
		this.documentManager = documentManager;
	}

	public void setVersionDAO(VersionDAO versionDAO) {
		this.versionDAO = versionDAO;
	}

	public void setFolderDAO(FolderDAO folderDAO) {
		this.folderDAO = folderDAO;
	}

	public void setStore(Store store) {
		this.store = store;
	}

	private Resource marshallFolder(Folder folder, WebdavSession session) {
		Resource resource = new ResourceImpl();
		resource.setID(String.valueOf(folder.getId()));
		resource.setContentLength(0L);
		resource.setName(folder.getName());
		resource.setLastModified(folder.getLastModified());

		resource.setETag(String.format("f-%d_%d", folder.getId(), folder.getRecordVersion()));

		resource.setSession(session);
		resource.setFolder(true);
		if (folder.getType() == Folder.TYPE_WORKSPACE)
			resource.setWorkspace(true);

		if (session != null && (Long) session.getObject("id") != null) {
			resource.setRequestedPerson((Long) session.getObject("id"));
		}

		return resource;
	}

	private Resource marshallDocument(Document document, WebdavSession session) {
		Resource resource = new ResourceImpl();
		resource.setID(String.valueOf(document.getId()));
		resource.setName(document.getFileName());
		resource.setContentLength(document.getFileSize());
		resource.setCreationDate(document.getCreation());
		resource.setLastModified(document.getDate());
		resource.setFolder(false);
		resource.setCheckedOut(document.getStatus() == AbstractDocument.DOC_CHECKED_OUT
				|| document.getStatus() == AbstractDocument.DOC_LOCKED);
		resource.setVersionLabel(document.getVersion());
		resource.setAuthor(document.getPublisher());
		resource.setDocRef(document.getDocRef());
		resource.setFolderID(String.valueOf(document.getFolder().getId()));

		// this is for getetag property
		resource.setETag(String.format("d-%d_%s", document.getId(), document.getVersion()));

		resource.setSession(session);
		resource.setLocked(document.getStatus() == AbstractDocument.DOC_LOCKED
				|| document.getStatus() == AbstractDocument.DOC_CHECKED_OUT);
		resource.setLockUser(document.getLockUser());

		if (session != null && (Long) session.getObject("id") != null) {
			resource.setRequestedPerson((Long) session.getObject("id"));
		}

		return resource;
	}

	@Override
	public List<Resource> getChildResources(Resource parentResource) throws DavException {
		List<Resource> resourceList = new LinkedList<>();
		final Long folderID = Long.parseLong(parentResource.getID());

		try {
			boolean hasAccess = isFolderAccessible(parentResource, folderID);

			if (!hasAccess)
				return resourceList;

			User user = userDAO.findById(parentResource.getRequestedPerson());
			userDAO.initialize(user);

			// Find children visible by the current user
			Collection<Folder> folders = folderDAO.findChildren(folderID, parentResource.getRequestedPerson());
			if (folders != null) {
				for (Folder currentFolder : folders) {
					if (currentFolder.getHidden() == 0)
						resourceList.add(marshallFolder(currentFolder, parentResource.getSession()));
				}
			}

			getDocumentChildren(parentResource, resourceList, user);
		} catch (PersistenceException e) {
			throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e);
		}

		return resourceList;
	}

	private void getDocumentChildren(Resource parentResource, List<Resource> resourceList, User user)
			throws PersistenceException {

		Collection<Document> documents = documentDAO.findByFileNameAndParentFolderId(
				Long.parseLong(parentResource.getID()), "", null, user.getTenantId(), null);
		for (Document document : documents) {
			try {
				checkPublished(user, document);
			} catch (Exception t) {
				continue;
			}
			resourceList.add(marshallDocument(document, parentResource.getSession()));
		}
	}

	private boolean isFolderAccessible(Resource parentResource, final Long folderID) throws PersistenceException {
		boolean hasAccess = folderDAO.isReadAllowed(folderID, parentResource.getRequestedPerson());

		if (!hasAccess) {
			// Check if the folder is a root and in that case we mark it as
			// readable.
			Folder folder = folderDAO.findFolder(folderID);
			hasAccess = folder != null && folder.equals(folderDAO.findRoot(folder.getTenantId()));
		}
		return hasAccess;
	}

	@Override
	public Resource getResource(String requestPath, WebdavSession session) throws DavException {
		log.debug("getResource: {}", requestPath);

		validateSession(session);

		long userId = 0;
		String currentStablePath = "";
		String name = "";
		try {
			userId = (Long) session.getObject("id");
			if (requestPath == null)
				requestPath = "/";

			requestPath = requestPath.replace("/store", "");

			if (requestPath.length() > 0 && requestPath.substring(0, 1).equals("/"))
				requestPath = requestPath.substring(1);

			String path = "/" + requestPath;
			currentStablePath = path;
			name = null;
			int lastidx = path.lastIndexOf("/");
			if (lastidx > -1) {
				name = path.substring(lastidx + 1, path.length());
				path = path.substring(0, lastidx + 1);
			}

			Folder folder = null;

			if (path.equals("/") && StringUtils.isEmpty(name)) {
				folder = folderDAO.findRoot(session.getTenantId());
			} else {
				String extPath = path + name;
				folder = folderDAO.findByPathExtended(extPath, session.getTenantId());
			}

			// if this resource request is a folder
			if (folder != null)
				return marshallFolder(folder, session);
		} catch (Exception e) {
			// Nothing to do
		}

		Resource parentFolder = this.getParentResource(currentStablePath, userId, session);
		boolean hasAccess;
		Document document = null;

		try {
			Collection<Document> docs = documentDAO.findByFileNameAndParentFolderId(
					Long.parseLong(parentFolder.getID()), name, null, session.getTenantId(), null);

			if (docs.isEmpty())
				return null;

			document = docs.iterator().next();

			hasAccess = folderDAO.isReadAllowed(document.getFolder().getId(), userId);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			hasAccess = false;
		}

		if (!hasAccess)
			throw new DavException(HttpServletResponse.SC_FORBIDDEN,
					"You have no appropriated rights to read this document");

		User user;
		try {
			user = userDAO.findById(userId);
			userDAO.initialize(user);
		} catch (PersistenceException e) {
			throw new DavException(HttpServletResponse.SC_FORBIDDEN, e);
		}
		
		checkPublished(user, document);

		return marshallDocument(document, session);
	}

	@Override
	public Resource getParentResource(String resourcePath, long userId, WebdavSession session) throws DavException {

		log.debug("Find parent DAV resource: {}", resourcePath);

		long tenantId;
		if (session != null) {
			tenantId = session.getTenantId();
		} else {
			try {
				User user = userDAO.findById(userId);
				tenantId = user.getTenantId();
			} catch (PersistenceException e) {
				throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e);
			}
		}

		resourcePath = resourcePath.replaceFirst("/store", "").replaceFirst("/vstore", "");
		if (!resourcePath.startsWith("/"))
			resourcePath = "/" + resourcePath;

		String name = "";
		resourcePath = resourcePath.substring(0, resourcePath.lastIndexOf('/'));
		if (!resourcePath.isEmpty()) {
			name = resourcePath.substring(resourcePath.lastIndexOf('/'));
			resourcePath = resourcePath.substring(0, resourcePath.lastIndexOf('/'));
		}

		resourcePath = resourcePath + "/";
		if (name.startsWith("/"))
			name = name.substring(1);

		if (log.isDebugEnabled())
			log.debug("Find DMS resource {} in path {}", name, resourcePath);

		try {
			Folder folder = null;
			if ("/".equals(resourcePath.trim()) && "".equals(name))
				folder = folderDAO.findRoot(tenantId);
			else {
				log.debug("resourcePath: {}", resourcePath);
				log.debug("name: {}", name);

				String folderToFind = resourcePath + "/" + name;
				folderToFind = folderToFind.replace("//", "/");
				log.debug("folderToFind: {}", folderToFind);
				folder = folderDAO.findByPathExtended(folderToFind, tenantId);
			}
			return marshallFolder(folder, session);
		} catch (PersistenceException e) {
			throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e);
		}
	}

	@Override
	public Resource createResource(Resource parentResource, String name, boolean isCollection, ImportContext context,
			WebdavSession session) throws DavException {
		validateSession(session);

		String sid = getSid(session);

		Folder parentFolder = getParentFolder(parentResource);
		enforceNotRoot(parentFolder);

		if (isCollection) {
			// check permission to add folder
			boolean addChildEnabled = parentResource.isAddChildEnabled();
			if (!addChildEnabled) {
				throw new DavException(HttpServletResponse.SC_FORBIDDEN, "Add Folder not granted to this user");
			}

			User user = (User) session.getObject("user");
			// Add a folder history entry
			FolderHistory transaction = new FolderHistory();
			transaction.setUser(user);
			transaction.setSessionId(sid);

			Folder newFolder = new Folder(name);
			newFolder.setTenantId(session.getTenantId());
			Folder createdFolder;
			try {
				createdFolder = folderDAO.create(parentFolder, newFolder, true, transaction);
			} catch (PersistenceException e) {
				throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
			}

			return this.marshallFolder(createdFolder, session);
		}

		// check permission to add document
		boolean writeEnabled = parentResource.isWriteEnabled();
		if (!writeEnabled)
			throw new DavException(HttpServletResponse.SC_FORBIDDEN, "Write Access not granted to this user");

		try (InputStream is = context.getInputStream()) {
			User user = getUser(parentResource);

			// Create the document history event
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSessionId(sid);
			transaction.setEvent(DocumentEvent.STORED.toString());
			transaction.setComment("");
			transaction.setUser(user);

			Document doc = new Document();
			doc.setFileName(name);
			doc.setFolder(parentFolder);
			doc.setLocale(user.getLocale());
			doc.setTenantId(session.getTenantId());

			doc = documentManager.create(is, doc, transaction);
			log.debug("Created document {}", doc);
			return marshallDocument(doc, session);
		} catch (PersistenceException | IOException e) {
			throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e);
		}
	}

	private User getUser(Resource parentResource) throws PersistenceException {
		return userDAO.findById(parentResource.getRequestedPerson());
	}

	private void enforceNotRoot(Folder parentFolder) throws DavException {
		try {
			long rootId = folderDAO.findRoot(parentFolder.getTenantId()).getId();
			if (parentFolder.getId() == rootId)
				throw new DavException(HttpServletResponse.SC_FORBIDDEN, "Cannot write in the root folder");
		} catch (PersistenceException e) {
			throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e);
		}
	}

	private Folder getParentFolder(Resource parentResource) throws DavException {
		Folder parentFolder;
		try {
			parentFolder = folderDAO.findById(Long.parseLong(parentResource.getID()));
		} catch (PersistenceException e) {
			throw new DavException(HttpServletResponse.SC_FORBIDDEN, e);
		}
		return parentFolder;
	}

	private String getSid(WebdavSession session) {
		String sid = null;
		if (session != null)
			sid = (String) session.getObject("sid");
		return sid;
	}

	private void validateSession(WebdavSession session) throws DavException {
		if (session == null)
			throw new DavException(HttpServletResponse.SC_FORBIDDEN, "No WebDAV session");
	}

	@Override
	public void updateResource(Resource resource, ImportContext context, WebdavSession session) throws DavException {
		try {
			User user = userDAO.findById(resource.getRequestedPerson());
			Document document = documentDAO.findById(Long.parseLong(resource.getID()));
			String sid = getSid(session);

			// verify the write permission on the parent folder
			Resource parent = getParentResource(resource);
			if (!parent.isWriteEnabled())
				throw new DavException(HttpServletResponse.SC_FORBIDDEN, "No rights to write resource.");

			if ((document.getStatus() == AbstractDocument.DOC_CHECKED_OUT
					|| document.getStatus() == AbstractDocument.DOC_LOCKED)
					&& (user.getId() != document.getLockUserId() && !"admin".equals(user.getUsername()))) {
				throw new DavException(HttpServletResponse.SC_FORBIDDEN, "Current user didn't locked the document");
			}

			assertDocumentIsNotImmutable(user, document);

			// Create the document history event
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSessionId(sid);
			transaction.setUser(user);
			transaction.setComment("");

			documentManager.checkin(Long.parseLong(resource.getID()), context.getInputStream(), resource.getName(),
					false, null, transaction);
		} catch (DavException de) {
			throw de;
		} catch (NumberFormatException | IOException | PersistenceException e) {
			throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e);
		}
	}

	@Override
	public Resource getChildByName(Resource parentResource, String name) throws DavException {
		try {
			Folder parentFolder = folderDAO.findById(Long.parseLong(parentResource.getID()));
			if (parentFolder == null)
				return null;
			Collection<Document> docs = documentDAO.findByFileNameAndParentFolderId(parentFolder.getId(), name, null,
					parentFolder.getTenantId(), null);
			User user = userDAO.findById(parentResource.getRequestedPerson());
			userDAO.initialize(user);

			if (!docs.isEmpty()) {
				for (Document document : docs) {
					if (name.equals(document.getFileName())) {
						return marshallDocument(document, parentResource.getSession());
					}
				}
			}
			return null;
		} catch (PersistenceException e) {
			throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e);
		}
	}

	@Override
	public Resource move(Resource source, Resource destination, String newName, WebdavSession session)
			throws DavException {
		String sid = getSid(session);
		try {
			if (source.isWorkspace()) {
				throw new DavException(HttpServletResponse.SC_FORBIDDEN, "Cannot move a workspace");
			} else if (source.isFolder()) {
				return folderRenameOrMove(source, destination, newName, session, sid);
			} else {
				return fileRenameOrMove(source, destination, newName, session, sid);
			}
		} catch (PersistenceException e) {
			throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e);
		}
	}

	private Resource fileRenameOrMove(Resource source, Resource destination, String newName, WebdavSession session,
			String sid) throws DavException, PersistenceException {
		// The source is a file so the requested operation can be: file
		// rename/file move

		// if the destination is null we can't do anything
		assertNotNullDestination(destination);

		// verify the move permission on source folders
		Resource parentFolder = getParentResource(source);
		Document document = documentDAO.findById(Long.parseLong(source.getID()));
		User user = userDAO.findById(source.getRequestedPerson());
		if (!documentDAO.isMoveAllowed(Long.parseLong(source.getID()), source.getRequestedPerson()))
			throw new DavException(HttpServletResponse.SC_FORBIDDEN,
					"User does not have move permission on source resource");

		assertDocumentIsNotImmutable(user, document);

		documentDAO.initialize(document);

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId(sid);
		transaction.setUser(user);

		if (!source.getName().equals(document.getFileName())) {
			boolean renameEnabled = parentFolder.isRenameEnabled();
			if (!renameEnabled)
				throw new DavException(HttpServletResponse.SC_FORBIDDEN, "Rename Permission not granted to this user");

			// we are doing a file rename
			documentManager.rename(document.getId(), source.getName(), transaction);
			document = documentDAO.findById(Long.parseLong(source.getID()));
			documentDAO.initialize(document);
		}

		final long targetFolderId = Long.parseLong(destination.getID());
		if (document.getFolder().getId() != targetFolderId) {
			// moving the document to another folder
			// verify the addchild permission on destination folder
			boolean addchildEnabled = destination.isAddChildEnabled();
			if (!addchildEnabled)
				throw new DavException(HttpServletResponse.SC_FORBIDDEN,
						"AddChild permission not granted to this user");

			Folder folder = folderDAO.findById(targetFolderId);
			if (document.getDocRef() != null)
				transaction.setEvent(DocumentEvent.SHORTCUT_MOVED.toString());
			documentManager.moveToFolder(document, folder, transaction);

			if (StringUtils.isNotEmpty(newName) && !document.getFileName().equals(newName)) {
				documentManager.rename(document.getId(), newName, new DocumentHistory(transaction));
				document = documentDAO.findById(Long.parseLong(source.getID()));
				documentDAO.initialize(document);
			}
		}

		return this.marshallDocument(document, session);
	}

	private void assertNotNullDestination(Resource destination) {
		if (destination == null)
			throw new UnsupportedOperationException();
	}

	private Resource folderRenameOrMove(Resource source, Resource destination, String newName, WebdavSession session,
			String sid) throws DavException, PersistenceException {

		Folder folder = getFolder(source);

		long currentParentFolder = folder.getParentId();
		long destinationParentFolder = Long.parseLong(destination.getID());

		User user = userDAO.findById(source.getRequestedPerson());

		if (!source.getName().equals(folder.getName())) {
			FolderHistory transaction = new FolderHistory();
			transaction.setUser(user);
			transaction.setEvent(FolderEvent.RENAMED.toString());
			transaction.setSessionId(sid);

			if (!source.isRenameEnabled())
				throw new DavException(HttpServletResponse.SC_FORBIDDEN, "Rename Permission not granted to this user");

			renameFolder(folder, source.getName(), destinationParentFolder, transaction);
			folder = getFolder(source);
		}

		// distinction between folder move and folder rename
		if (currentParentFolder != destinationParentFolder) {
			// Folder Move, verify the addchild permission on destination
			// folders
			if (!destination.isAddChildEnabled())
				throw new DavException(HttpServletResponse.SC_FORBIDDEN,
						"AddChild Permission not granted to this user");

			assertResourceIsDeletable(source);

			// Add a folder history entry
			FolderHistory transaction = new FolderHistory();
			transaction.setSessionId(sid);
			transaction.setUser(user);

			// we are doing a folder move
			moveFolder(folder, destinationParentFolder, newName, transaction);
		}

		folder = getFolder(source);
		return this.marshallFolder(folder, session);
	}

	private void renameFolder(Folder currentFolder, String newName, long destinationParentFolder,
			FolderHistory transaction) throws PersistenceException {
		// Folder Rename
		currentFolder.setName(newName);

		// Add a folder history entry
		folderDAO.store(currentFolder, transaction);

		currentFolder.setParentId(destinationParentFolder);
		folderDAO.store(currentFolder);
	}

	private void moveFolder(Folder currentFolder, long destinationParentFolder, String newName,
			FolderHistory transaction) throws PersistenceException {
		Folder destParentFolder = folderDAO.findById(destinationParentFolder);
		folderDAO.initialize(destParentFolder);
		folderDAO.move(currentFolder, destParentFolder, transaction);
		if (StringUtils.isNoneEmpty(newName)) {
			currentFolder.setName(newName);
			FolderHistory ft = new FolderHistory(transaction);
			ft.setEvent(FolderEvent.RENAMED.toString());
			folderDAO.store(currentFolder, ft);
		}

	}

	private Folder getFolder(Resource source) throws DavException, NumberFormatException, PersistenceException {
		Folder currentFolder = folderDAO.findById(Long.parseLong(source.getID()));

		if (currentFolder.getType() == Folder.TYPE_WORKSPACE)
			throw new DavException(HttpServletResponse.SC_FORBIDDEN, "Cannot move nor rename a workspace");

		folderDAO.initialize(currentFolder);
		return currentFolder;
	}

	@Override
	public void deleteResource(Resource resource, WebdavSession session) throws DavException {
		String sid = getSid(session);
		log.debug("deleteResource {}", resource.getID());
		try {
			Folder folder = assertResourceIsNotWorkspace(resource);

			User user = userDAO.findById(resource.getRequestedPerson());
			if (resource.isFolder()) {
				assertResourceIsDeletable(resource);

				FolderHistory transaction = new FolderHistory();
				transaction.setUser(user);
				transaction.setSessionId(sid);
				transaction.setUser(user);
				transaction.setEvent(FolderEvent.DELETED.toString());
				List<Folder> notDeletableFolders = folderDAO.deleteTree(folder, PersistentObject.DELETED_CODE_DEFAULT,
						transaction);

				if (CollectionUtils.isNotEmpty(notDeletableFolders))
					throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
							"Unable to delete some subfolders.");
			} else if (!resource.isFolder()) {
				// verify the write permission on the parent folder
				Resource parent = getParentResource(resource);
				if (!parent.isDeleteEnabled())
					throw new DavException(HttpServletResponse.SC_FORBIDDEN, "No rights to delete on parent resource.");

				DocumentHistory transaction = new DocumentHistory();
				transaction.setUser(user);
				transaction.setSessionId(sid);
				transaction.setUser(user);
				transaction.setEvent(DocumentEvent.DELETED.toString());

				Document document = documentDAO.findById(Long.parseLong(resource.getID()));
				assertDocumentIsNotImmutable(user, document);

				// Check if there are some shortcuts associated to the
				// deleting document. All the shortcuts must be deleted.
				if (CollectionUtils.isNotEmpty(documentDAO.findAliasIds(Long.parseLong(resource.getID()))))
					for (Long shortcutId : documentDAO.findAliasIds(Long.parseLong(resource.getID()))) {
						documentDAO.delete(shortcutId, transaction);
					}
				documentDAO.delete(Long.parseLong(resource.getID()), transaction);
			}
		} catch (DavException de) {
			throw de;
		} catch (Exception e) {
			throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
		}
	}

	private Folder assertResourceIsNotWorkspace(Resource resource) throws PersistenceException, DavException {
		Folder folder = folderDAO.findById(Long.parseLong(resource.getID()));
		if (resource.isFolder() && folder != null && (folder.getType() == Folder.TYPE_WORKSPACE))
			throw new DavException(HttpServletResponse.SC_FORBIDDEN, "Cannot delete a workspace.");
		return folder;
	}

	private void assertResourceIsDeletable(Resource resource) throws DavException {
		if (!resource.isDeleteEnabled())
			throw new DavException(HttpServletResponse.SC_FORBIDDEN, "No rights to delete resource.");
	}

	@Override
	public void copy(Resource source, Resource destination, String newName, WebdavSession session) throws DavException {
		String sid = getSid(session);

		log.debug("Copy resource {}", source.getID());

		Session ldSession = SessionManager.get().get(sid);
		validateTargetFolderForCopy(ldSession.getTenantId(), destination);

		if (source.isFolder()) {
			throw new DavException(HttpServletResponse.SC_FORBIDDEN, "FolderCopy not supported");
		} else {
			try {
				boolean writeEnabled = destination.isWriteEnabled();
				if (!writeEnabled)
					throw new DavException(HttpServletResponse.SC_FORBIDDEN, "No rights to write resource.");

				Document document = documentDAO.findById(Long.parseLong(source.getID()));
				Folder folder = folderDAO.findById(Long.parseLong(destination.getID()));

				User user = ldSession.getUser();
				assertDocumentIsNotImmutable(user, document);

				// Create the document history event
				DocumentHistory transaction = new DocumentHistory();
				transaction.setSessionId(sid);
				transaction.setEvent(DocumentEvent.STORED.toString());
				transaction.setComment("");
				transaction.setUser(user);

				Document createdDoc;
				if (document.getDocRef() != null) {
					document = documentDAO.findById(document.getDocRef());
					createdDoc = documentManager.createAlias(document, folder, document.getDocRefType(), transaction);
				} else {
					createdDoc = documentManager.copyToFolder(document, folder, transaction, true, true, true);
				}

				if (StringUtils.isNotEmpty(newName))
					documentManager.rename(createdDoc.getId(), newName, new DocumentHistory(transaction));
			} catch (DavException de) {
				throw de;
			} catch (Exception e) {
				throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e);
			}
		}
	}

	private void validateTargetFolderForCopy(long tenantId, Resource destination) throws DavException {
		long rootId = 0L;
		try {
			rootId = folderDAO.findRoot(tenantId).getId();
		} catch (PersistenceException e) {
			throw new DavException(HttpServletResponse.SC_FORBIDDEN, e);
		}

		/*
		 * Cannot write in the root
		 */
		if (destination.isFolder() && Long.parseLong(destination.getID()) == rootId)
			throw new DavException(HttpServletResponse.SC_FORBIDDEN, "Cannot write in the root");
	}

	private void assertDocumentIsNotImmutable(User user, Document document) throws DavException {
		if (document.getImmutable() == 1 && !user.isMemberOf(Group.GROUP_ADMIN))
			throw new DavException(HttpServletResponse.SC_FORBIDDEN, "The document is immutable");
	}

	@Override
	public Resource getParentResource(Resource resource) throws DavException {
		try {
			Document document = documentDAO.findById(Long.parseLong(resource.getID()));
			return this.marshallFolder(document.getFolder(), resource.getSession());
		} catch (PersistenceException e) {
			throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e);
		}
	}

	@Override
	public InputStream streamOut(Resource resource) throws DavException {
		if (!resource.isDownloadEnabled())
			throw new DavException(HttpServletResponse.SC_FORBIDDEN, "The user doesn't have the download permission");

		String version = resource.getVersionLabel();
		Document document = null;
		try {
			document = documentDAO.findById(Long.parseLong(resource.getID()));
		} catch (PersistenceException e) {
			throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e);
		}
		if (document == null) {
			// Document not found
			return new ByteArrayInputStream("not found".getBytes());
		}

		if (document.getVersion() != null && document.getVersion().equals(resource.getVersionLabel()))
			version = null;

		InputStream is = null;
		try {
			if (version == null || version.equals("")) {
				String res = store.getResourceName(document, null, null);
				is = store.getStream(document.getId(), res);
			} else {
				String res = store.getResourceName(document, resource.getVersionLabel(), null);
				is = store.getStream(document.getId(), res);
			}
		} catch (IOException e) {
			throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e);
		}
		return is;
	}

	@Override
	public void checkout(Resource resource, WebdavSession session) throws DavException {
		String sid = getSid(session);

		User user = null;
		try {
			user = userDAO.findById(resource.getRequestedPerson());
			userDAO.initialize(user);
		} catch (PersistenceException e1) {
			throw new DavException(HttpServletResponse.SC_FORBIDDEN, e1);
		}

		try {
			checkPublished(user, Long.parseLong(resource.getID()));
		} catch (PersistenceException e1) {
			throw new DavException(HttpServletResponse.SC_FORBIDDEN, e1);
		}

		// verify the write permission on the parent folder
		Resource parent = getParentResource(resource);
		if (!parent.isWriteEnabled())
			throw new DavException(HttpServletResponse.SC_FORBIDDEN, "No rights to checkout resource.");

		try {
			// Create the document history event
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSessionId(sid);
			transaction.setEvent(DocumentEvent.CHECKEDOUT.toString());
			transaction.setComment("");
			transaction.setUser(user);

			documentManager.checkout(Long.parseLong(resource.getID()), transaction);
		} catch (Exception e) {
			throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
		}
	}

	protected void checkPublished(User user, Document doc) throws DavException {
		if (!user.isMemberOf(Group.GROUP_ADMIN) && !user.isMemberOf("publisher") && !doc.isPublishing())
			throw new DavException(1, "Document not published");
	}

	protected void checkPublished(User user, long docId) throws DavException, PersistenceException {
		Document document = documentDAO.findById(docId);
		checkPublished(user, document);
	}

	@Override
	public List<Resource> getHistory(Resource resource) {
		List<Resource> resourceHistory = new LinkedList<>();
		try {
			Document document = documentDAO.findById(Long.parseLong(resource.getID()));

			documentDAO.initialize(document);

			Collection<Version> tmp = versionDAO.findByDocId(document.getId());
			Version[] sortIt = tmp.toArray(new Version[0]);

			// clear collection and add sorted elements
			Arrays.sort(sortIt);

			for (Version version : sortIt) {
				Resource res = marshallDocument(document, resource.getSession());
				res.setVersionLabel(version.getVersion());
				res.setVersionDate(version.getDate());
				res.setAuthor(version.getUsername());
				res.setCheckedOut(true);
				resourceHistory.add(res);
			}

			return resourceHistory;
		} catch (PersistenceException e) {
			return resourceHistory;
		}
	}

	@Override
	public void uncheckout(Resource resource, WebdavSession session) throws DavException {
		String sid = getSid(session);
		try {
			User user = userDAO.findById(resource.getRequestedPerson());
			// Create the document history event
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSessionId(sid);
			transaction.setUser(user);

			documentManager.unlock(Long.parseLong(resource.getID()), transaction);

			resource.setCheckedOut(false);
		} catch (Exception e) {
			throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
		}
	}

	@Override
	public void addBookmark(Resource resource, WebdavSession session) throws DavException {
		try {
			User user = userDAO.findById(resource.getRequestedPerson());

			BookmarkDAO bdao = Context.get(BookmarkDAO.class);

			Bookmark bmark = new Bookmark();
			if (resource.isFolder()) {
				bmark.setType(Bookmark.TYPE_FOLDER);
				bmark.setTitle(resource.getName());
				bmark.setFileType("folder");
				bmark.setUserId(user.getId());
				bmark.setTargetId(Long.parseLong(resource.getID()));
			} else {
				bmark.setType(Bookmark.TYPE_DOCUMENT);
				bmark.setTitle(resource.getName());
				// get the file extension
				String fileType = FilenameUtils.getExtension(resource.getName());
				bmark.setFileType(fileType);
				bmark.setUserId(user.getId());
				bmark.setTargetId(Long.parseLong(resource.getID()));
			}

			bdao.store(bmark);
		} catch (Exception e) {
			throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
		}
	}

	@Override
	public void deleteBookmark(Resource resource, WebdavSession session) throws DavException {
		log.info("deleteBookmark");
		try {
			User user = userDAO.findById(resource.getRequestedPerson());

			BookmarkDAO bdao = Context.get(BookmarkDAO.class);

			Bookmark bkm = null;
			if (resource.isFolder()) {
				bkm = bdao.findByUserIdAndFolderId(user.getId(), Long.parseLong(resource.getID()));
			} else {
				bkm = bdao.findByUserIdAndDocId(user.getId(), Long.parseLong(resource.getID()));
			}

			if (bkm != null) {
				bdao.delete(bkm.getId());
			}
		} catch (Exception e) {
			throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
		}
	}
}