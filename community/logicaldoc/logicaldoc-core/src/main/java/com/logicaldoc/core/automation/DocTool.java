package com.logicaldoc.core.automation;

import java.io.File;
import java.io.InputStream;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailAttachment;
import com.logicaldoc.core.communication.MailUtil;
import com.logicaldoc.core.conversion.FormatConverterManager;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.document.DocumentNote;
import com.logicaldoc.core.document.Version;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.document.dao.DocumentHistoryDAO;
import com.logicaldoc.core.document.dao.DocumentNoteDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.core.ticket.Ticket;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;

/**
 * Utility methods to handle documents from within Automation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
@AutomationDictionary
public class DocTool {

	protected static Logger log = LoggerFactory.getLogger(DocTool.class);

	/**
	 * Builds the download url of a document(download permalink)
	 * 
	 * @param docId identifier of the document
	 * 
	 * @return the download permalink
	 */
	public String downloadUrl(long docId) {
		ContextProperties config = Context.get().getProperties();
		String url = config.getProperty("server.url");
		if (!url.endsWith("/"))
			url += "/";
		url += "download?docId=" + docId;
		return url;
	}

	/**
	 * Builds the display url of a document(display permalink)
	 * 
	 * @param tenantId identifier of the tenant
	 * @param docId identifier of the document
	 * 
	 * @return the display permalink
	 */
	public String displayUrl(long tenantId, long docId) {
		ContextProperties config = Context.get().getProperties();
		String url = config.getProperty("server.url");
		if (!url.endsWith("/"))
			url += "/";

		TenantDAO tenantDao = (TenantDAO) Context.get().getBean(TenantDAO.class);
		Tenant tenant = tenantDao.findById(tenantId);

		url += "display?tenant=" + tenant.getName() + "&docId=" + docId;
		return url;
	}

	/**
	 * Builds the download url of a document(download permalink)
	 * 
	 * @param doc the document
	 * 
	 * @return the download permalink
	 */
	public String downloadUrl(Document doc) {
		return downloadUrl(doc.getId());
	}

	/**
	 * Builds the download url of a document(download permalink)
	 * 
	 * @param history object representing an event on a document
	 * 
	 * @return the download permalink
	 */
	public String downloadUrl(DocumentHistory history) {
		return downloadUrl(history.getDocId());
	}

	/**
	 * Builds the display url of a document(display permalink)
	 * 
	 * @param doc the document
	 * 
	 * @return the display permalink
	 */
	public String displayUrl(Document doc) {
		return displayUrl(doc.getTenantId(), doc.getId());
	}

	
	/**
	 * Builds the display url of a document(display permalink)
	 * 
	 * @param history object representing an event on a document
	 * 
	 * @return the display permalink
	 */
	public String displayUrl(DocumentHistory history) {
		return displayUrl(history.getTenantId(), history.getDocId());
	}

	/**
	 * Creates a new download ticket for a document
	 * 
	 * @param docId identifier of the document
	 * @param pdfConversion if the pdf conversion should be downloaded instead of the original file
	 * @param expireHours number of hours after which the ticket expires
	 * @param expireDate a specific expiration date
	 * @param maxDownloads number of downloads over which the ticket expires
	 * @param username the user in whose name the method is run
	 * 
	 * @return the complete download ticket's URL
	 * 
	 * @throws Exception a generic error happened
	 */
	public String downloadTicket(final long docId, boolean pdfConversion, Integer expireHours, Date expireDate,
			Integer maxDownloads, String username) throws Exception {

		ContextProperties config = Context.get().getProperties();
		String urlPrefix = config.getProperty("server.url");
		if (!urlPrefix.endsWith("/"))
			urlPrefix += "/";

		User user = getUser(username);

		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		DocumentHistory transaction = new DocumentHistory();
		transaction.setUser(user);
		Ticket ticket = manager.createDownloadTicket(docId, pdfConversion ? "" : "conversion.pdf", expireHours,
				expireDate, maxDownloads, urlPrefix, transaction);

		return ticket.getUrl();
	}

	/**
	 * Prints the size of a file in human readable form
	 * 
	 * @param size the file size
	 * 
	 * @return the human readable representation
	 */
	public String displayFileSize(Long size) {
		if (size == null || size.longValue() < 0)
			return "0 Bytes";
		final String[] units = new String[] { "Bytes", "KB", "MB", "GB", "TB" };
		int digitGroups = (int) (Math.log10(size) / Math.log10(1024));
		return new DecimalFormat("#,##0.#").format(size / Math.pow(1024, digitGroups)) + " " + units[digitGroups];
	}

	/**
	 * Saves / updates a document into the database
	 * 
	 * @param doc the document to save
	 */
	public void store(Document doc) {
		store(doc, null);
	}

	/**
	 * Saves / updates a document into the database
	 * 
	 * @param doc the document to save
	 * @param username the user in whose name the method is run
	 */
	public void store(Document doc, String username) {
		User user = getUser(username);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setDocId(doc.getId());
		transaction.setDate(new Date());
		transaction.setUser(user);

		try {
			docDao.store(doc, transaction);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
		}
	}

	/**
	 * Moves a document into a target folder
	 * 
	 * @param doc the document
	 * @param targetPath the full path of the target folder
	 * @param username the user in whose name the method is run
	 * 
	 * @throws Exception a generic error happened
	 */
	public void move(Document doc, String targetPath, String username) throws Exception {
		User user = getUser(username);
		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);

		Folder folder = createPath(doc, targetPath, username);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setDocId(doc.getId());
		transaction.setTenantId(doc.getTenantId());
		transaction.setDate(new Date());
		transaction.setUser(user);

		manager.moveToFolder(doc, folder, transaction);
	}

	/**
	 * Copies a document into a target folder
	 * 
	 * @param doc the document
	 * @param targetPath the full path of the target folder
	 * @param username the user in whose name the method is run
	 * 
	 * @throws Exception a generic error happened
	 */
	public void copy(Document doc, String targetPath, String username) throws Exception {
		User user = getUser(username);

		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);

		Folder folder = createPath(doc, targetPath, username);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setDocId(doc.getId());
		transaction.setUser(user);

		manager.copyToFolder(doc, folder, transaction);
	}

	/**
	 * Locks a document
	 * 
	 * @param docId identifier of the document
	 * @param username the  user that locks the document
	 * 
	 * @throws Exception a generic error happened
	 */
	public void lock(long docId, String username) throws Exception {
		User user = getUser(username);

		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setDocId(docId);
		transaction.setUser(user);

		manager.lock(docId, AbstractDocument.DOC_LOCKED, transaction);
	}

	/**
	 * Unlocks a document
	 * 
	 * @param docId identifier of the document
	 * @param username the  user that locks the document
	 * 
	 * @throws Exception a generic error happened
	 */
	public void unlock(long docId, String username) throws Exception {
		User user = getUser(username);

		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setDocId(docId);
		transaction.setUser(user);

		manager.unlock(docId, transaction);
	}

	/**
	 * Delete a document
	 * 
	 * @param docId identifier of the document
	 * @param username the user in whose name the method is run
	 * 
	 * @throws Exception a generic error happened
	 */
	public void delete(long docId, String username) throws Exception {
		User user = getUser(username);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setDocId(docId);
		transaction.setUser(user);

		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		dao.delete(docId, transaction);
	}

	/**
	 * Copies a resource in a file in the same folder of the original document
	 * 
	 * @param doc The document
	 * @param fileVersion The file version
	 * @param suffix the suffix (eg conversion.pdf)
	 * @param newFileName the file name of the new file
	 * @param username the user in whose name the method is run
	 * 
	 * @return the document
	 * 
	 * @throws Exception a generic error happened
	 */
	public Document copyResource(Document doc, String fileVersion, String suffix, String newFileName, String username)
			throws Exception {
		User user = getUser(username);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setUser(user);

		Storer storer = (Storer) Context.get().getBean(Storer.class);
		String resource = storer.getResourceName(doc, fileVersion, suffix);

		File tmpFile = null;

		try {
			tmpFile = File.createTempFile("res-", suffix);
			storer.writeToFile(doc.getId(), resource, tmpFile);

			Document docVO = new Document();
			docVO.setFileName(newFileName);
			docVO.setTenantId(doc.getTenantId());
			docVO.setFolder(doc.getFolder());
			docVO.setLanguage(doc.getLanguage());

			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			return manager.create(tmpFile, docVO, transaction);
		} finally {
			if (tmpFile != null)
				FileUtil.strongDelete(tmpFile);
		}
	}

	/**
	 * Writes a resource in a file in the local file system
	 * 
	 * @param docId The document's identifier
	 * @param fileVersion The file version
	 * @param suffix the suffix (eg conversion.pdf)
	 * @param outputFile the user in name of which to take this action
	 * 
	 * @throws Exception error writing the file
	 */
	public void writeToFile(long docId, String fileVersion, String suffix, String outputFile) throws Exception {
		Storer storer = (Storer) Context.get().getBean(Storer.class);
		String resource = storer.getResourceName(docId, fileVersion, suffix);
		storer.writeToFile(docId, resource, new File(outputFile));
	}

	/**
	 * Convert a document in another format and saves the result in another file in the same folder
	 * 
	 * @param doc the document to convert
	 * @param format it is the output format(e.g. <b>pdf</b>, <b>txt</b>, ...)
	 * @param username the user in whose name the method is run
	 * 
	 * @return the generated conversion document
	 * 
	 * @throws Exception generic error happened
	 */
	public Document convert(Document doc, String format, String username) throws Exception {
		User user = getUser(username);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setUser(user);

		FormatConverterManager manager = (FormatConverterManager) Context.get().getBean(FormatConverterManager.class);
		Document conversion = manager.convert(doc, null, format, transaction);
		return conversion;
	}

	/**
	 * Retrieves a user object
	 * 
	 * @param username the username
	 * 
	 * @return the user object
	 */
	@Deprecated
	public User getUser(String username) {
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		User user = StringUtils.isNotEmpty(username) ? userDao.findByUsername(username)
				: userDao.findByUsername("_system");
		return user;
	}

	/**
	 * Retrieves a document by it's identifier
	 * 
	 * @param docId identifier of the document
	 * 
	 * @return the document object
	 */
	public Document findById(long docId) {
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = docDao.findDocument(docId);
		docDao.initialize(doc);
		return doc;
	}

	/**
	 * Retrieves a document by it's path
	 * 
	 * @param path full path of the document
	 * 
	 * @return the document object
	 */
	public Document findByPath(String path) {
		return findByPath(path, null);
	}

	/**
	 * Retrieves a document by it's path
	 * 
	 * @param path full path of the document
	 * @param tenantId identifier of the tenant(optional)
	 * 
	 * @return the document object
	 */
	public Document findByPath(String path, Long tenantId) {
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = docDao.findByPath(path, tenantId != null ? tenantId : Tenant.DEFAULT_ID);
		return doc;
	}

	/**
	 * Calculates the full path of a document
	 * 
	 * @param doc the document to inspect
	 * 
	 * @return the full path
	 */
	public String getPath(Document doc) {
		if (doc == null)
			return "";
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		String path = folderDao.computePathExtended(doc.getFolder().getId());
		if (!path.endsWith("/"))
			path += "/";
		path += doc.getFileName();
		return path;
	}

	/**
	 * Converts a collection of documents in a collection of identifiers
	 * 
	 * @param docs the input collection
	 * 
	 * @return the list of all the IDs of the documents in the input collection
	 */
	public List<Long> getIds(Collection<Document> docs) {
		if (docs == null || docs.isEmpty())
			return new ArrayList<Long>();
		return docs.stream().map(d -> d.getId()).collect(Collectors.toList());
	}

	/**
	 * Creates a path, all existing nodes in the specified path will be reused
	 * 
	 * @param doc document used to take the root folder in case the <code>targetPath</code> represents a relative path
	 * @param targetPath absolute or relative(in relation to the document's parent folder) path to be created
	 * @param username the user in whose name the method is run
	 * 
	 * @return the folder leaf of the path
	 */
	public Folder createPath(Document doc, String targetPath, String username) {
		SecurityTool secTool=new SecurityTool();
		User user = secTool.getUser(username);

		FolderHistory transaction = new FolderHistory();
		transaction.setUser(user);

		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Folder parent = doc.getFolder();
		if (targetPath.startsWith("/")) {
			targetPath = targetPath.substring(1);
			/*
			 * Cannot write in the root so if the parent is the root, we have to
			 * guarantee that the first element in the path is a workspace. If
			 * not the Default one will be used.
			 */
			parent = fdao.findRoot(doc.getTenantId());
			Folder workspace = null;

			/*
			 * Check if the path contains the workspace specification
			 */
			for (Folder w : fdao.findWorkspaces(doc.getTenantId())) {
				if (targetPath.startsWith(w.getName())) {
					workspace = fdao.findById(w.getId());
					break;
				}
			}

			if (workspace == null) {
				parent = fdao.findDefaultWorkspace(doc.getTenantId());
			}
		}

		Folder folder = null;
		try {
			folder = fdao.createPath(parent, targetPath, true, transaction);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		return folder;
	}

	/**
	 * Retrieve the list of events of a document
	 * 
	 * @param docId identifier of the document
	 * @param event event name, optional
	 * 
	 * @return list of histories
	 */
	public List<DocumentHistory> getHistories(long docId, String event) {
		DocumentHistoryDAO hDao = (DocumentHistoryDAO) Context.get().getBean(DocumentHistoryDAO.class);
		return hDao.findByDocIdAndEvent(docId, event);
	}

	/**
	 * Creates a new note for the whole document
	 * 
	 * @param doc the document
	 * @param text text of the note
	 * @param username username of the user in name of which this action is taken
	 */
	public void addNote(Document doc, String text, String username) {
		User user = getUser(username);

		DocumentNote note = new DocumentNote();
		note.setTenantId(doc.getTenantId());
		note.setDocId(doc.getId());
		note.setUserId(user.getId());
		note.setUsername(user.getUsername());
		note.setDate(new Date());
		note.setMessage(text);
		note.setFileName(doc.getFileName());
		note.setFileVersion(doc.getFileVersion());

		DocumentHistory transaction = new DocumentHistory();
		transaction.setUser(user);

		try {
			DocumentNoteDAO dao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);
			dao.store(note, transaction);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
		}
	}

	/**
	 * Lists the notes of a given document
	 * 
	 * @param docId identifier of the document
	 * @param fileVersion version of the file, optional
	 * 
	 * @return the list of notes
	 */
	public List<DocumentNote> getNotes(long docId, String fileVersion) {
		DocumentNoteDAO dao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);

		String fVer = fileVersion;
		if (StringUtils.isEmpty(fileVersion)) {
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document doc = docDao.findDocument(docId);
			fVer = doc.getFileVersion();
		}
		return dao.findByDocId(docId, fVer);
	}

	/**
	 * Calculates what will be the next version specification. @see {@link Version#getNewVersionName(String, boolean)} 
	 * 
	 * @param currentVersion the current version(e.g. 1.1, 2.15, ...)
	 * @param major if the new release must be a major release
	 * 
	 * @return the new version
	 */
	public String calculateNextVersion(String currentVersion, boolean major) {
		return Version.calculateNewVersion(currentVersion, major);
	}
	
	/**
	 * Extracts attachments of email files (.eml, .msg) in the current folder
	 * 
	 * @param doc the document
	 * @param filterFileName a filter on the extensions of the attachment to be extracted (comma separated)
	 * @param username the user that will be impersonated to write the attachments
	 * @return a list with the new documents created
	 */
	public List<Document> extractAttachments(Document doc, String filterFileName, String username) {
		
		List<Document> createdDocs = new ArrayList<Document>();
		
		User user = getUser(username);
		InputStream is = null;
		try {
			long docId = doc.getId();
			Storer storer = (Storer) Context.get().getBean(Storer.class);
			String resource = storer.getResourceName(docId, doc.getFileVersion(), null);
			is = storer.getStream(docId, resource);

			EMail email = null;

			if (doc.getFileName().toLowerCase().endsWith(".eml"))
				email = MailUtil.messageToMail(is, true);
			else
				email = MailUtil.msgToMail(is, true);

			
			if (email.getAttachments().size() > 0) {
				for (EMailAttachment att : email.getAttachments().values()) {
					
					// Apply the filter
					if (isAllowed(att.getFileName(), filterFileName)) {
						
						File tmpFile = null;
						try {
							tmpFile = File.createTempFile("att-", null);
							FileUtils.writeByteArrayToFile(tmpFile, att.getData());

							Document docVO = new Document();
							docVO.setFileName(att.getFileName());
							docVO.setTenantId(doc.getTenantId());
							docVO.setFolder(doc.getFolder());
							docVO.setLanguage(doc.getLanguage());
							
							DocumentHistory transaction = new DocumentHistory();
							transaction.setUser(user);

							DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
							Document attDoc = manager.create(tmpFile, docVO, transaction);
							createdDocs.add(attDoc);
						} finally {
							if (tmpFile != null)
								FileUtil.strongDelete(tmpFile);
						}
					}
				}
			}

		} catch (Throwable t) {
			log.error(t.getMessage(), t);
		} finally {
			IOUtils.closeQuietly(is);
		}
		return createdDocs;
	}
	
	/**
	 * Check if the specified filename is allowed or not.
	 * 
	 * @param filename The file name to check
	 * @return True if <code>filename</code> is included in
	 *         <code>includes</code>
	 */
	private boolean isAllowed(String filename, String includes) {
		return FileUtil.matches(filename, includes, "");
	}		
}