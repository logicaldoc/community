package com.logicaldoc.dropbox;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.dropbox.core.DbxException;
import com.dropbox.core.v2.files.FileMetadata;
import com.dropbox.core.v2.files.Metadata;
import com.google.gwt.user.server.rpc.RemoteServiceServlet;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.store.Store;
import com.logicaldoc.gui.common.client.InvalidSessionServerException;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.frontend.client.dropbox.DropboxService;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;

/**
 * Implementation of the DropboxService
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.0
 */
public class DropboxServiceImpl extends RemoteServiceServlet implements DropboxService {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(DropboxServiceImpl.class);

	public static Session validateSession(HttpServletRequest request) throws InvalidSessionServerException {
		String sid = SessionManager.get().getSessionId(request);
		Session session = SessionManager.get().get(sid);
		if (session == null)
			throw new InvalidSessionServerException("Invalid Session");
		if (!SessionManager.get().isOpen(sid))
			throw new InvalidSessionServerException("Invalid or Expired Session");
		SessionManager.get().renew(sid);
		return session;
	}

	@Override
	public boolean isConnected() throws ServerException {
		Session session = DropboxServiceImpl.validateSession(getThreadLocalRequest());

		try {
			Dropbox dbox = new Dropbox(session.getUserId());
			if (!dbox.gotAccessToken())
				return false;
			return dbox.login();
		} catch (Exception e) {
			throw new ServerException(e.getMessage(), e);
		}
	}

	@Override
	public String startAuthorization() throws ServerException {
		Session session = DropboxServiceImpl.validateSession(getThreadLocalRequest());

		try {
			Dropbox dbox = new Dropbox(session.getUserId());
			return dbox.startAuthorization(session.getUser().getLocale());
		} catch (Exception t) {
			throw new ServerException(t.getMessage(), t);
		}
	}

	@Override
	public String finishAuthorization(String authorizationCode) throws ServerException {
		Session session = DropboxServiceImpl.validateSession(getThreadLocalRequest());

		try {
			User user = session.getUser();
			Dropbox dbox = new Dropbox(user.getId());
			String token = dbox.finishAuthorization(authorizationCode);
			if (token == null)
				return null;
			dbox.login();
			String account = dbox.getAccountName();
			saveAccessToken(user, token);
			return account;
		} catch (Exception t) {
			throw new ServerException(t.getMessage(), t);
		}
	}

	/**
	 * Saves the access token saved for the given user.
	 * 
	 * @throws PersistenceException Error in the database
	 */
	private void saveAccessToken(User user, String token) throws PersistenceException {
		try {
			Dropbox dBox = new Dropbox(user.getId());
			dBox.setAccessToken(token);
			dBox.saveSettings();
		} catch (Exception t) {
			log.error(t.getMessage(), t);
		}
	}

	@Override
	public boolean exportDocuments(String targetPath, List<Long> folderIds, List<Long> docIds) throws ServerException {
		Session session = DropboxServiceImpl.validateSession(getThreadLocalRequest());

		try {
			User user = session.getUser();
			Dropbox dbox = new Dropbox(user.getId());
			if (!dbox.gotAccessToken())
				return false;
			dbox.login();

			Metadata entry = dbox.get(targetPath);
			if (entry == null || entry instanceof FileMetadata)
				return false;

			if (!targetPath.endsWith("/"))
				targetPath += "/";

			FolderDAO folderDao = Context.get(FolderDAO.class);
			DocumentDAO docDao = Context.get(DocumentDAO.class);

			// Prepare a fieldsMap docId-path
			Map<Long, String> documents = new HashMap<>();

			// First of all put all single selected documents
			for (Document document : docDao.findByIds(docIds.stream().collect(Collectors.toSet()), null))
				documents.put(document.getId(), document.getFileName());

			/*
			 * Now browse all the tree adding the documents
			 */

			// Prepare a fieldsMap folderId-basepath
			Map<Long, String> folders = new HashMap<>();
			for (long folderId : folderIds) {
				Folder folder = folderDao.findFolder(folderId);
				if (folder == null || !folderDao.isPermissionAllowed(Permission.DOWNLOAD, folder.getId(), user.getId()))
					continue;

				loadFoldersTree(folder.getId(), folder.getName() + "/", user.getId(), folders);
			}

			for (Map.Entry<Long, String> entr : folders.entrySet()) {
				List<Document> folderDocs = docDao.findByFolder(entr.getKey(), null);
				for (Document doc : folderDocs)
					documents.put(doc.getId(), entr.getValue() + doc.getFileName());
			}

			for (Map.Entry<Long, String> entr : documents.entrySet())
				uploadDocument(entr.getKey(), targetPath + entr.getValue(), dbox, session);

			return true;
		} catch (Exception t) {
			log.error(t.getMessage(), t);
			return false;
		}
	}

	private void uploadDocument(Long docId, String path, Dropbox dropbox, Session session)
			throws IOException, PersistenceException {
		Store store = Context.get(Store.class);
		DocumentDAO ddao = Context.get(DocumentDAO.class);

		File temp = null;
		try {
			temp = FileUtil.createTempFile("dboxupload", ".tmp");
			store.writeToFile(docId, store.getResourceName(docId, null, null), temp);
			dropbox.uploadFile(temp, path);

			Document doc = ddao.findById(docId);

			// Add an history entry to track the download of the document
			DocumentHistory history = new DocumentHistory();
			history.setDocId(doc.getId());
			history.setVersion(doc.getVersion());
			history.setFileVersion(doc.getFileVersion());
			history.setFilename(doc.getFileName());
			history.setFolderId(doc.getFolder().getId());
			history.setComment("Exported into Dropbox");
			history.setSession(session);
			history.setDocument(doc);

			FolderDAO fdao = Context.get(FolderDAO.class);
			history.setPath(fdao.computePathExtended(doc.getFolder().getId()));
			history.setEvent(DocumentEvent.DOWNLOADED.toString());

			try {
				ddao.saveDocumentHistory(doc, history);
			} catch (Exception t) {
				log.error(t.getMessage(), t);
			}
		} finally {
			FileUtils.deleteQuietly(temp);
		}
	}

	private void loadFoldersTree(long parentId, String parentPath, long userId, Map<Long, String> folders)
			throws PersistenceException {
		FolderDAO fDao = Context.get(FolderDAO.class);
		folders.put(parentId, parentPath);
		for (Folder folder : fDao.findChildren(parentId, userId)) {
			if (parentId != folder.getId())
				loadFoldersTree(folder.getId(), parentPath + folder.getName() + "/", userId, folders);
		}
	}

	@Override
	public int importDocuments(long targetFolder, List<String> paths) throws ServerException {
		Session session = DropboxServiceImpl.validateSession(getThreadLocalRequest());
		FolderDAO fdao = Context.get(FolderDAO.class);

		int count = 0;
		try {
			if (!fdao.isPermissionAllowed(Permission.IMPORT, targetFolder, session.getUserId()))
				return 0;

			User user = session.getUser();
			Dropbox dbox = new Dropbox(user.getId());
			if (StringUtils.isEmpty(dbox.getAccessToken()))
				return 0;
			dbox.login();

			Folder root = fdao.findById(targetFolder);

			Set<String> imported = new HashSet<>();
			for (String path : paths) {
				if (imported.contains(path))
					continue;

				Metadata entry = dbox.get(path);
				importEntry(session, dbox, root, imported, entry);
			}

			count = imported.size();
		} catch (Exception t) {
			log.error(t.getMessage(), t);
		}

		return count;
	}

	private void importEntry(Session session, Dropbox dbox, Folder root, Set<String> imported, Metadata entry)
			throws DbxException, PersistenceException, IOException {
		if (entry instanceof FileMetadata metadata) {
			importDocument(root, metadata, dbox, session);
			imported.add(entry.getPathDisplay());
		} else {
			FolderDAO fdao = Context.get(FolderDAO.class);
			String rootPath = entry.getPathDisplay();
			if (!rootPath.endsWith("/"))
				rootPath += "/";

			List<FileMetadata> files = dbox.listFilesInTree(rootPath);
			for (FileMetadata file : files) {
				if (imported.contains(file.getPathDisplay()))
					continue;

				FolderHistory transaction = new FolderHistory();
				transaction.setSession(session);

				String folderPath = FileUtil.getPath(file.getPathDisplay());
				folderPath = folderPath.replace("\\\\", "/");

				Folder folder = fdao.createPath(root, folderPath, true, transaction);

				importDocument(folder, file, dbox, session);
				imported.add(file.getPathDisplay());
			}
		}
	}

	private void importDocument(Folder root, FileMetadata src, Dropbox dbox, Session session)
			throws IOException, PersistenceException {
		DocumentDAO ddao = Context.get(DocumentDAO.class);
		DocumentManager manager = Context.get(DocumentManager.class);

		File temp = null;
		try {
			temp = FileUtil.createTempFile("dboxdownload", ".tmp");
			dbox.downloadFile(src.getPathDisplay(), temp);

			List<Document> docs = ddao.findByFileNameAndParentFolderId(root.getId(), src.getPathDisplay(), null,
					root.getTenantId(), null);
			if (docs.size() == 1) {
				/*
				 * Checkout and checkin an existing document
				 */
				Document doc = docs.get(0);

				DocumentHistory history = new DocumentHistory();
				history.setFolderId(root.getId());
				history.setSession(session);

				FolderDAO fdao = Context.get(FolderDAO.class);
				String pathExtended = fdao.computePathExtended(root.getId());
				history.setPath(pathExtended);

				manager.checkout(doc.getId(), history);

				history = new DocumentHistory();
				history.setFolderId(root.getId());
				history.setSession(session);
				history.setPath(pathExtended);
				history.setComment("Updated from Dropbox");

				manager.checkin(doc.getId(), temp, doc.getFileName(), false, null, history);
			} else {
				/*
				 * Create a new document
				 */

				Document docVO = new Document();
				docVO.setFileName(src.getName());
				docVO.setFolder(root);
				docVO.setLanguage(session.getUser().getLanguage());

				DocumentHistory history = new DocumentHistory();
				history.setFolderId(root.getId());
				history.setComment("Imported from Dropbox");
				history.setSession(session);

				FolderDAO fdao = Context.get(FolderDAO.class);
				history.setPath(fdao.computePathExtended(root.getId()));
				history.setEvent(DocumentEvent.STORED.toString());

				manager.create(temp, docVO, history);
			}
		} finally {
			FileUtils.deleteQuietly(temp);
		}
	}

	@Override
	public void saveSettings(String apiKey, String apiSecret) throws ServerException {
		Session session = DropboxServiceImpl.validateSession(getThreadLocalRequest());
		try {
			User user = session.getUser();
			Dropbox dbox = new Dropbox(user.getId());
			dbox.setApiKey(apiKey);
			dbox.setApiSecret(apiSecret);
			dbox.saveSettings();
		} catch (Exception t) {
			throw new ServerException(t.getMessage(), t);
		}
	}

	@Override
	public List<String> loadSettings() throws ServerException {
		Session session = DropboxServiceImpl.validateSession(getThreadLocalRequest());
		try {
			User user = session.getUser();
			Dropbox dbox = new Dropbox(user.getId());
			return List.of(dbox.getApiKey(), dbox.getApiSecret());
		} catch (Exception t) {
			throw new ServerException(t.getMessage(), t);
		}
	}
}