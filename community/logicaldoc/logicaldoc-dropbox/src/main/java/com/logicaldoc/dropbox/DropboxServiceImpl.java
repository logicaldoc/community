package com.logicaldoc.dropbox;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.dropbox.core.v2.files.FileMetadata;
import com.dropbox.core.v2.files.Metadata;
import com.google.gwt.user.server.rpc.RemoteServiceServlet;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.document.History;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.document.dao.HistoryDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.frontend.client.services.DropboxService;
import com.logicaldoc.util.Context;

/**
 * Implementation of the DropboxService
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 7.0
 */
public class DropboxServiceImpl extends RemoteServiceServlet implements DropboxService {
	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(DropboxServiceImpl.class);

	@Override
	public boolean isConnected() throws ServerException {
		Session session = SessionUtil.validateSession(getThreadLocalRequest());

		try {
			Dropbox dbox = new Dropbox();
			String accessToken = loadAccessToken(session.getUser());
			if (accessToken == null)
				return false;
			return dbox.login(accessToken);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new RuntimeException(t.getMessage(), t);
		}
	}

	@Override
	public String startAuthorization() throws ServerException {
		Session session = SessionUtil.validateSession(getThreadLocalRequest());

		try {
			Dropbox dbox = new Dropbox();
			return dbox.startAuthorization(session.getUser().getLocale());
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new RuntimeException(t.getMessage(), t);
		}
	}

	@Override
	public String finishAuthorization(String authorizationCode) throws ServerException {
		Session session = SessionUtil.validateSession(getThreadLocalRequest());

		try {
			User user = session.getUser();
			Dropbox dbox = new Dropbox();
			String token = dbox.finishAuthorization(authorizationCode);
			if (token == null)
				return null;
			dbox.login(token);
			String account = dbox.getAccountName();
			saveAccessToken(user, token, account);
			return account;
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new RuntimeException(t.getMessage(), t);
		}
	}

	/**
	 * Loads the access token saved for the given user.
	 */
	static String loadAccessToken(User user) {
		GenericDAO dao = (GenericDAO) Context.get().getBean(GenericDAO.class);
		Generic generic = dao.findByAlternateKey("dropbox", "token", user.getId(), user.getTenantId());
		if (generic == null)
			return null;
		else
			return generic.getString1();
	}

	/**
	 * Saves the access token saved for the given user. The token is saved in a
	 * Generic(type: dropbox, subtype: token)
	 */
	protected void saveAccessToken(User user, String token, String account) {
		GenericDAO dao = (GenericDAO) Context.get().getBean(GenericDAO.class);
		Generic generic = dao.findByAlternateKey("dropbox", "token", user.getId(), user.getTenantId());
		if (generic == null)
			generic = new Generic("dropbox", "token", user.getId(), user.getTenantId());
		generic.setString1(token);
		generic.setString2(account);
		dao.store(generic);
	}

	@Override
	public boolean exportDocuments(String targetPath, long[] folderIds, long[] docIds) throws ServerException {
		Session session = SessionUtil.validateSession(getThreadLocalRequest());

		try {
			User user = session.getUser();
			Dropbox dbox = new Dropbox();
			String token = loadAccessToken(user);
			if (token == null)
				return false;
			dbox.login(token);

			Metadata entry = dbox.get(targetPath);
			if (entry == null || entry instanceof FileMetadata)
				return false;

			if (!targetPath.endsWith("/"))
				targetPath += "/";

			FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

			// Prepare a fieldsMap docId-path
			Map<Long, String> documents = new HashMap<Long, String>();

			// First of all put all single selected documents
			List<Long> dIds = new ArrayList<Long>();
			for (int i = 0; i < docIds.length; i++)
				dIds.add(docIds[i]);

			for (Document document : docDao.findByIds(dIds.toArray(new Long[0]), null))
				documents.put(document.getId(), document.getFileName());

			/*
			 * Now browse all the tree adding the documents
			 */

			// Prepare a fieldsMap folderId-basepath
			Map<Long, String> folders = new HashMap<Long, String>();
			for (long folderId : folderIds) {
				Folder folder = folderDao.findFolder(folderId);
				if (folder == null || !folderDao.isPermissionEnabled(Permission.DOWNLOAD, folder.getId(), user.getId()))
					continue;

				loadFoldersTree(folder.getId(), folder.getName() + "/", user.getId(), folders);
			}
			for (Long folderId : folders.keySet()) {
				List<Document> folderDocs = docDao.findByFolder(folderId, null);
				for (Document doc : folderDocs)
					documents.put(doc.getId(), folders.get(folderId) + doc.getFileName());
			}

			for (Long docId : documents.keySet()) {
				uploadDocument(docId, targetPath + documents.get(docId), dbox, session);
			}

			return true;
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			return false;
		}
	}

	private void uploadDocument(Long docId, String path, Dropbox dropbox, Session session) throws IOException {
		Storer store = (Storer) Context.get().getBean(Storer.class);
		HistoryDAO hdao = (HistoryDAO) Context.get().getBean(HistoryDAO.class);
		DocumentDAO ddao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

		File temp = null;
		try {
			temp = File.createTempFile("dboxupload", ".tmp");
			store.writeToFile(docId, store.getResourceName(docId, null, null), temp);
			dropbox.uploadFile(temp, path);

			Document doc = ddao.findById(docId);

			// Add an history entry to track the download of the document
			History history = new History();
			history.setDocId(doc.getId());
			history.setVersion(doc.getVersion());
			history.setFilename(doc.getFileName());
			history.setFolderId(doc.getFolder().getId());
			history.setComment("Exported into Dropbox");
			history.setSession(session);

			FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			history.setPath(fdao.computePathExtended(doc.getFolder().getId()));
			history.setEvent(DocumentEvent.DOWNLOADED.toString());
			hdao.store(history);
		} finally {
			FileUtils.deleteQuietly(temp);
		}
	}

	private void loadFoldersTree(long parentId, String parentPath, long userId, Map<Long, String> folders) {
		FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		folders.put(parentId, parentPath);
		for (Folder folder : fDao.findChildren(parentId, userId)) {
			if (parentId == folder.getId())
				continue;
			else
				loadFoldersTree(folder.getId(), parentPath + folder.getName() + "/", userId, folders);
		}
	}

	@Override
	public int importDocuments(long targetFolder, String[] paths) throws ServerException {
		Session session = SessionUtil.validateSession(getThreadLocalRequest());
		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);

		if (!fdao.isPermissionEnabled(Permission.IMPORT, targetFolder, session.getUserId()))
			return 0;

		int count = 0;
		try {
			User user = session.getUser();
			Dropbox dbox = new Dropbox();
			String token = loadAccessToken(user);
			if (token == null)
				return 0;
			dbox.login(token);

			Folder root = fdao.findById(targetFolder);
			
			Set<String> imported = new HashSet<String>();
			for (String path : paths) {
				if (imported.contains(path))
					continue;

				Metadata entry = dbox.get(path);
				if (entry instanceof FileMetadata) {
					importDocument(root, (FileMetadata) entry, dbox, session);
					imported.add(entry.getPathDisplay());
				} else {
					String rootPath = entry.getPathDisplay();
					if (!rootPath.endsWith("/"))
						rootPath += "/";

					List<FileMetadata> files = dbox.listFilesInTree(rootPath);
					for (FileMetadata file : files) {
						if (imported.contains(file.getPathDisplay()))
							continue;

						FolderHistory transaction = new FolderHistory();
						transaction.setSession(session);

						String folderPath = file.getPathDisplay().substring(rootPath.length());
						folderPath = FilenameUtils.getPath(file.getPathDisplay());
						folderPath = folderPath.replaceAll("\\\\", "/");

						Folder folder = fdao.createPath(root, folderPath, true, transaction);

						importDocument(folder, file, dbox, session);
						imported.add(file.getPathDisplay());
					}
				}
			}

			count = imported.size();
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
		}

		return count;
	}

	private void importDocument(Folder root, FileMetadata src, Dropbox dbox, Session session) throws Exception {
		DocumentDAO ddao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);

		File temp = null;
		try {
			temp = File.createTempFile("dboxdownload", ".tmp");
			dbox.downloadFile(src.getPathDisplay(), temp);

			List<Document> docs = ddao.findByFileNameAndParentFolderId(root.getId(), src.getPathDisplay(), null,
					root.getTenantId(), null);
			if (docs.size() == 1) {
				/*
				 * Checkout and checkin an existing document
				 */
				Document doc = docs.get(0);

				History history = new History();
				history.setFolderId(root.getId());
				history.setSession(session);

				FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
				String pathExtended = fdao.computePathExtended(root.getId());
				history.setPath(pathExtended);

				manager.checkout(doc.getId(), history);

				history = new History();
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

				History history = new History();
				history.setFolderId(root.getId());
				history.setComment("Imported from Dropbox");
				history.setSession(session);

				FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
				history.setPath(fdao.computePathExtended(root.getId()));
				history.setEvent(DocumentEvent.STORED.toString());

				manager.create(temp, docVO, history);
			}
		} finally {
			FileUtils.deleteQuietly(temp);
		}
	}
}