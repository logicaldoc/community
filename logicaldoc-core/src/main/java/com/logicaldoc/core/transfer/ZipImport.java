package com.logicaldoc.core.transfer;

import java.io.File;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.Set;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.communication.SystemMessage;
import com.logicaldoc.core.communication.SystemMessageDAO;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.util.UserUtil;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.ZipUtil;

/**
 * This is an import utilities that imports documents stored in a zip archive.
 * All folders in the zip will be replicated.
 * 
 * @author Sebastian Stein
 * @author Matteo Caruso - LogicalDOC
 */
public class ZipImport {

	protected User user;

	protected static Logger log = LoggerFactory.getLogger(ZipImport.class);

	protected File zipFile;

	private boolean notifyUser = true;

	protected String sessionId = null;

	protected Document docVo = null;

	protected String fileNameCharset = "UTF-8";

	/**
	 * Constructor.
	 * 
	 * @param docVo Value object for the common documents
	 * @param fileNameCharset the charset to use to process the zip
	 */
	public ZipImport(Document docVo, String fileNameCharset) {
		this.docVo = docVo;
		if (fileNameCharset != null)
			this.fileNameCharset = fileNameCharset;
	}

	public void process(File zipsource, Folder parent, long userId, String sessionId) throws PersistenceException {
		this.zipFile = zipsource;
		this.sessionId = sessionId;

		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		this.user = userDao.findById(userId);

		File dir = UserUtil.getUserResource(userId, "unzip");
		if (dir.exists()) {
			try {
				FileUtils.deleteDirectory(dir);
			} catch (IOException e) {
				// Nothing to do
			}
		}

		try {
			FileUtils.forceMkdir(dir);
		} catch (IOException e) {
			// Nothing to do
		}

		try {
			ZipUtil zipUtil = new ZipUtil();
			zipUtil.unzip(zipFile.getPath(), dir.getPath());
			File[] files = dir.listFiles();

			for (int i = 0; i < files.length; i++) {
				if (StringUtils.isNotEmpty(files[i].getName())
						|| StringUtils.isNotEmpty(FilenameUtils.getBaseName(files[i].getName())))
					try {
						addEntry(files[i], parent);
					} catch (PersistenceException e) {
						log.error("Error adding entry " + files[i].getName(), e);
					}
			}
		} catch (IOException e) {
			log.error(e.getMessage(), e);
		}

		try {
			FileUtils.deleteDirectory(dir);
		} catch (IOException e) {
			// Nothing to do
		}

		if (notifyUser)
			sendNotificationMessage();
	}

	public void process(String zipsource, Locale locale, Folder parent, long userId, Long templateId, String sessionId)
			throws PersistenceException {
		File srcfile = new File(zipsource);
		process(srcfile, parent, userId, sessionId);
	}

	/**
	 * Stores a file in the repository of logicaldoc and inserts some
	 * information in the database of logicaldoc (folder, document, version,
	 * history, searchdocument).
	 * 
	 * @param file
	 * @param parent
	 * @throws PersistenceException
	 */
	protected void addEntry(File file, Folder parent) throws PersistenceException {
		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		String folderName = file.getName();
		FolderHistory transaction = new FolderHistory();
		transaction.setUser(user);
		transaction.setSessionId(sessionId);

		Session session = SessionManager.get().get(sessionId);
		if (transaction != null)
			transaction.setSession(session);

		if (file.isDirectory()) {
			// creates a logicaldoc folder
			Folder folderVO = new Folder();
			folderVO.setName(folderName);
			Folder folder = dao.create(parent, folderVO, true, transaction);

			File[] files = file.listFiles();

			for (int i = 0; i < files.length; i++)
				if (StringUtils.isNotEmpty(files[i].getName())
						|| StringUtils.isNotEmpty(FilenameUtils.getBaseName(files[i].getName())))
					addEntry(files[i], folder);
		} else {
			// creates a document
			DocumentManager docManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			try {
				DocumentHistory history = new DocumentHistory();
				history.setEvent(DocumentEvent.STORED.toString());
				history.setComment("");
				history.setUser(user);
				history.setSessionId(sessionId);
				if (session != null)
					history.setSession(session);

				Document doc = new Document(docVo);
				doc.setId(0L);
				doc.setFolder(parent);

				docManager.create(file, doc, history);
			} catch (Exception e) {
				log.error("InMemoryZipImport addEntry failed", e);
			}
		}
	}

	/**
	 * Sends a system message to the user that imported the zip
	 */
	protected void sendNotificationMessage() {
		SystemMessageDAO smdao = (SystemMessageDAO) Context.get().getBean(SystemMessageDAO.class);
		Date now = new Date();
		Recipient recipient = new Recipient();
		recipient.setName(user.getUsername());
		recipient.setAddress(user.getUsername());
		recipient.setType(Recipient.TYPE_SYSTEM);
		recipient.setMode("message");
		Set<Recipient> recipients = new HashSet<Recipient>();
		recipients.add(recipient);
		SystemMessage sysmess = new SystemMessage();
		sysmess.setAuthor("SYSTEM");
		sysmess.setRecipients(recipients);
		ResourceBundle bundle = ResourceBundle.getBundle("i18n.messages", user.getLocale());
		sysmess.setSubject(bundle.getString("zip.import.subject"));
		String message = bundle.getString("zip.import.body");
		String body = MessageFormat.format(message, new Object[] { zipFile.getName() });
		sysmess.setMessageText(body);
		sysmess.setSentDate(now);
		sysmess.setConfirmation(0);
		sysmess.setPrio(0);
		sysmess.setDateScope(1);

		try {
			smdao.store(sysmess);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}

	public boolean isNotifyUser() {
		return notifyUser;
	}

	public void setNotifyUser(boolean notifyUser) {
		this.notifyUser = notifyUser;
	}
}