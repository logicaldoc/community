package com.logicaldoc.core.transfer;

import java.io.File;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.ZipUtil;

/**
 * This is an import utilities that imports documents stored in a zip archive.
 * The entire import process is followed in memory, to replicate correctly the
 * names of directories and documents when they contain native characters. All
 * folders in the zip will be replicated. Also, if required the parsing of
 * documents is executed for the extraction of the tags of the documents.
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 4.5.2
 */
public class InMemoryZipImport extends ZipImport {

	protected static Logger logger = LoggerFactory.getLogger(InMemoryZipImport.class);

	public InMemoryZipImport(Document docVo, String charset) {
		super(docVo, charset);
	}

	@Override
	public void process(File zipsource, Folder parent, long userId, String sessionId) {
		this.zipFile = zipsource;
		this.sessionId = sessionId;

		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);

		try {
			this.user = userDao.findById(userId);
			extractEntries(zipsource, parent, sessionId);
		} catch (Exception e) {
			logger.error("InMemoryZipImport process failed", e);
		}

		if (isNotifyUser())
			sendNotificationMessage();
	}

	private void extractEntries(File sourceZipFile, Folder parent, String sessionId) throws PersistenceException {
		FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		DocumentManager docManager = (DocumentManager) Context.get().getBean(DocumentManager.class);

		// Open the Zip and list all the contents
		ZipUtil zipUtil = new ZipUtil();
		zipUtil.setFileNameCharset(fileNameCharset);
		List<String> entries = zipUtil.listEntries(sourceZipFile);

		for (String entry : entries) {
			String fileName = FileUtil.getName(entry);
			String title = FileUtil.getBaseName(fileName);

			if (StringUtils.isEmpty(fileName) || StringUtils.isEmpty(title))
				continue;

			String relativePath = FileUtil.getPath(entry);
			if (relativePath.startsWith("/"))
				relativePath = relativePath.substring(1);
			if (relativePath.endsWith("/"))
				relativePath = relativePath.substring(0, relativePath.length() - 1);

			// Ensure to have the proper folder to upload the file into
			FolderHistory folderTransaction = new FolderHistory();
			folderTransaction.setSessionId(sessionId);
			folderTransaction.setUser(user);
			Folder folder = fDao.createPath(parent, relativePath, true, folderTransaction);

			// Create the document
			Document doc = new Document(docVo);
			doc.setId(0L);
			doc.setFileName(fileName);
			doc.setFolder(folder);

			DocumentHistory history = new DocumentHistory();
			history.setEvent(DocumentEvent.STORED.toString());
			history.setComment("");
			history.setUser(user);
			history.setSessionId(sessionId);

			try {
				docManager.create(zipUtil.getEntryStream(sourceZipFile, entry), doc, history);
			} catch (Exception e) {
				logger.warn("InMemoryZipImport unable to import ZIP entry {}", entry, e);
			}
		}
	}
}