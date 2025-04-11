package com.logicaldoc.core.automation;

import java.io.File;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.conversion.FormatConverterManager;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentHistoryDAO;
import com.logicaldoc.core.document.IndexingStatus;
import com.logicaldoc.core.document.DocumentLink;
import com.logicaldoc.core.document.DocumentLinkDAO;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.document.DocumentNote;
import com.logicaldoc.core.document.DocumentNoteDAO;
import com.logicaldoc.core.document.DocumentStatus;
import com.logicaldoc.core.document.Version;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.parser.ParsingException;
import com.logicaldoc.core.searchengine.Hit;
import com.logicaldoc.core.searchengine.SearchEngine;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.store.Store;
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

	private static final String SERVER_URL = "server.url";

	private static final Logger log = LoggerFactory.getLogger(DocTool.class);

	/**
	 * Builds the download url of a document(download permalink)
	 * 
	 * @param docId identifier of the document
	 * 
	 * @return the download permalink
	 */
	public String downloadUrl(long docId) {
		ContextProperties config = Context.get().getProperties();
		String url = config.getProperty(SERVER_URL);
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
		String url = config.getProperty(SERVER_URL);
		if (!url.endsWith("/"))
			url += "/";

		try {
			TenantDAO tenantDao = Context.get(TenantDAO.class);
			Tenant tenant = tenantDao.findById(tenantId);
			url += "display?tenant=" + tenant.getName() + "&docId=" + docId;
			return url;
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
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
	 * @param pdfConversion if the pdf conversion should be downloaded instead
	 *        of the original file
	 * @param expireHours number of hours after which the ticket expires
	 * @param expireDate a specific expiration date
	 * @param maxDownloads number of downloads over which the ticket expires
	 * @param username the user in whose name the method is run
	 * 
	 * @return the complete download ticket's URL
	 */
	public String downloadTicket(final long docId, boolean pdfConversion, Integer expireHours, Date expireDate,
			Integer maxDownloads, String username) {

		ContextProperties config = Context.get().getProperties();
		String urlPrefix = config.getProperty(SERVER_URL);
		if (!urlPrefix.endsWith("/"))
			urlPrefix += "/";

		User user = new SecurityTool().getUser(username);

		DocumentManager manager = Context.get(DocumentManager.class);
		DocumentHistory transaction = new DocumentHistory();
		transaction.setUser(user);
		try {
			Ticket ticket = new Ticket();
			ticket.setTenantId(user.getTenantId());
			ticket.setType(Ticket.DOWNLOAD);
			ticket.setDocId(docId);
			ticket.setSuffix(pdfConversion ? "" : "conversion.pdf");
			ticket.setExpireHours(expireHours);
			ticket.setExpired(expireDate);
			ticket.setMaxCount(maxDownloads);
			ticket.setUrl(urlPrefix);

			ticket = manager.createTicket(ticket, transaction);
			return ticket.getUrl();
		} catch (PermissionException | PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	/**
	 * Creates a new view ticket for a document
	 * 
	 * @param docId identifier of the document
	 * @param pdfConversion if the pdf conversion should be downloaded instead
	 *        of the original file
	 * @param expireHours number of hours after which the ticket expires
	 * @param expireDate a specific expiration date
	 * @param maxDownloads number of downloads over which the ticket expires
	 * @param maxDownloads number of views over which the ticket expires
	 * @param username the user in whose name the method is run
	 * 
	 * @return the complete download ticket's URL
	 */
	public String viewTicket(final long docId, boolean pdfConversion, Integer expireHours, Date expireDate,
			Integer maxDownloads, Integer maxViews, String username) {

		ContextProperties config = Context.get().getProperties();
		String urlPrefix = config.getProperty(SERVER_URL);
		if (!urlPrefix.endsWith("/"))
			urlPrefix += "/";

		User user = new SecurityTool().getUser(username);

		DocumentManager manager = Context.get(DocumentManager.class);
		DocumentHistory transaction = new DocumentHistory();
		transaction.setUser(user);
		try {
			Ticket ticket = new Ticket();
			ticket.setTenantId(user.getTenantId());
			ticket.setType(Ticket.VIEW);
			ticket.setDocId(docId);
			ticket.setSuffix(pdfConversion ? "" : "conversion.pdf");
			ticket.setExpireHours(expireHours);
			ticket.setExpired(expireDate);
			ticket.setMaxCount(maxDownloads);
			ticket.setMaxViews(maxViews);
			ticket.setUrl(urlPrefix);

			ticket = manager.createTicket(ticket, transaction);
			return ticket.getUrl();
		} catch (PermissionException | PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
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
	 * @param transaction entry to log the event
	 * 
	 */
	public void store(Document doc, DocumentHistory transaction) {
		DocumentDAO docDao = Context.get(DocumentDAO.class);
		try {
			docDao.store(doc, transaction);
		} catch (Exception t) {
			log.error(t.getMessage(), t);
		}
	}

	/**
	 * Saves / updates a document into the database
	 * 
	 * @param doc the document to save
	 */
	public void store(Document doc) {
		store(doc, (String) null);
	}

	/**
	 * Saves / updates a document into the database
	 * 
	 * @param doc the document to save
	 * @param username the user in whose name the method is run
	 */
	public void store(Document doc, String username) {
		DocumentHistory transaction = new DocumentHistory();
		transaction.setDocument(doc);
		transaction.setDate(new Date());
		transaction.setUser(new SecurityTool().getUser(username));
		store(doc, transaction);
	}

	/**
	 * Created a new document into the database
	 * 
	 * @param doc the document that carries the metadata
	 * @param file the document's content
	 * @param username the user in whose name the method is run
	 * 
	 * @return the created document
	 */
	public Document create(Document doc, File file, String username) {
		DocumentHistory transaction = new DocumentHistory();
		transaction.setDocument(doc);
		transaction.setDate(new Date());
		transaction.setUser(new SecurityTool().getUser(username));

		DocumentManager manager = Context.get(DocumentManager.class);
		try {
			return manager.create(file, doc, transaction).getDocument();
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	/**
	 * Initializes lazy loaded collections
	 * 
	 * @param doc the document to initialize
	 */
	public void initialize(Document doc) {
		DocumentDAO docDao = Context.get(DocumentDAO.class);
		docDao.initialize(doc);
	}

	/**
	 * Moves a document into a target folder
	 * 
	 * @param doc the document
	 * @param targetPath the full path of the target folder
	 * @param username the user in whose name the method is run
	 */
	public void move(Document doc, String targetPath, String username) {
		User user = new SecurityTool().getUser(username);
		DocumentManager manager = Context.get(DocumentManager.class);

		Folder folder = createPath(doc, targetPath, username);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setDocument(doc);
		transaction.setDate(new Date());
		transaction.setUser(user);

		try {
			manager.moveToFolder(doc, folder, transaction);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}

	/**
	 * Instantiate a new document that is a clone of the given one. The returned
	 * clone is not persisted.
	 * 
	 * @param doc The source document to clone
	 * 
	 * @return The cloned document
	 */
	public Document clone(Document doc) {
		return new Document(doc);
	}

	/**
	 * Copies a document into a target folder
	 * 
	 * @param doc the document
	 * @param targetPath the full path of the target folder
	 * @param links if the links must be copied too
	 * @param notes if the notes must be copied too
	 * @param security if the security settings must be copied too
	 * 
	 * @param username the user in whose name the method is run
	 * 
	 * @return the new document created
	 */
	public Document copy(Document doc, String targetPath, boolean links, boolean notes, boolean security,
			String username) {
		User user = new SecurityTool().getUser(username);

		DocumentManager manager = Context.get(DocumentManager.class);

		Folder folder = createPath(doc, targetPath, username);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setDocId(doc.getId());
		transaction.setUser(user);

		try {
			return manager.copyToFolder(doc, folder, transaction, links, notes, security).getDocument();
		} catch (PersistenceException | IOException e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	/**
	 * Copies a document into a target folder
	 * 
	 * @param doc the document
	 * @param targetPath the full path of the target folder
	 * @param username the user in whose name the method is run
	 * 
	 * @return the new document created
	 */
	public Document copy(Document doc, String targetPath, String username) {
		return copy(doc, targetPath, false, false, true, username);
	}

	/**
	 * Links two documents
	 * 
	 * @param doc1 first document
	 * @param doc2 second document
	 * @param type type of link(optional)
	 * 
	 * @return the created link
	 */
	public DocumentLink link(Document doc1, Document doc2, String type) {
		DocumentLinkDAO linkDao = Context.get(DocumentLinkDAO.class);
		DocumentLink link = null;
		try {
			link = linkDao.findByDocIdsAndType(doc1.getId(), doc2.getId(), "default");
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		if (link == null) {
			// The link doesn't exist and must be created
			link = new DocumentLink();
			link.setTenantId(doc1.getTenantId());
			link.setDocument1(doc1);
			link.setDocument2(doc2);
			link.setType(StringUtils.isEmpty(type) ? "default" : type);
			try {
				linkDao.store(link);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
		}
		return link;
	}

	/**
	 * Creates an alias to a document
	 * 
	 * @param originalDoc the original document to be referenced
	 * @param targetPath the full path of the target folder
	 * @param type the alias type(<b>null</b> for the original file or
	 *        <b>pdf</b> for it's pdf conversion)
	 * @param username the user in whose name the method is run
	 * 
	 * @return The new alias created
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public Document createAlias(Document originalDoc, String targetPath, String type, String username)
			throws PersistenceException {
		Folder targetFolder = createPath(originalDoc, targetPath, username);
		return createAlias(originalDoc, targetFolder, type, username);
	}

	/**
	 * Creates an alias to a document
	 * 
	 * @param originalDoc the original document to be referenced
	 * @param targetFolder the folder that will contain the alias
	 * @param type the alias type(<b>null</b> for the original file or
	 *        <b>pdf</b> for it's pdf conversion)
	 * @param username the user in whose name the method is run
	 * 
	 * @return The new alias created
	 */
	public Document createAlias(Document originalDoc, Folder targetFolder, String type, String username) {
		User user = new SecurityTool().getUser(username);

		DocumentManager manager = Context.get(DocumentManager.class);
		DocumentHistory transaction = new DocumentHistory();
		transaction.setUser(user);

		try {
			return manager.createAlias(originalDoc, targetFolder, type, transaction);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	/**
	 * Locks a document
	 * 
	 * @param docId identifier of the document
	 * @param username the user that locks the document
	 */
	public void lock(long docId, String username) {
		User user = new SecurityTool().getUser(username);

		DocumentManager manager = Context.get(DocumentManager.class);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setDocId(docId);
		transaction.setUser(user);

		try {
			manager.lock(docId, DocumentStatus.LOCKED, transaction);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}

	/**
	 * Unlocks a document
	 * 
	 * @param docId identifier of the document
	 * @param username the user that locks the document
	 */
	public void unlock(long docId, String username) {
		User user = new SecurityTool().getUser(username);

		DocumentManager manager = Context.get(DocumentManager.class);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setDocId(docId);
		transaction.setUser(user);

		try {
			manager.unlock(docId, transaction);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}

	/**
	 * Delete a document
	 * 
	 * @param docId identifier of the document
	 * @param username the user in whose name the method is run
	 */
	public void delete(long docId, String username) {
		User user = new SecurityTool().getUser(username);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setDocId(docId);
		transaction.setUser(user);

		DocumentDAO dao = Context.get(DocumentDAO.class);
		try {
			dao.delete(docId, transaction);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
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
	 */
	public Document copyResource(Document doc, String fileVersion, String suffix, String newFileName, String username) {
		User user = new SecurityTool().getUser(username);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setUser(user);

		Store store = Context.get(Store.class);
		String resource = store.getResourceName(doc, fileVersion, suffix);

		File tmpFile = null;

		try {
			tmpFile = FileUtil.createTempFile("res-", suffix);
			store.writeToFile(doc.getId(), resource, tmpFile);

			Document docVO = new Document();
			docVO.setFileName(newFileName);
			docVO.setTenantId(doc.getTenantId());
			docVO.setFolder(doc.getFolder());
			docVO.setLanguage(doc.getLanguage());

			DocumentManager manager = Context.get(DocumentManager.class);
			return manager.create(tmpFile, docVO, transaction).getDocument();
		} catch (IOException | PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		} finally {
			FileUtil.delete(tmpFile);
		}
	}

	/**
	 * Gets the content of a resource and reads it as a string
	 * 
	 * @param docId The document's identifier
	 * @param fileVersion The file version
	 * @param suffix The suffix
	 * 
	 * @return the file content as string
	 */
	public String readAsString(long docId, String fileVersion, String suffix) {
		Store store = Context.get(Store.class);
		String resource = store.getResourceName(docId, fileVersion, suffix);
		return store.getString(docId, resource);
	}

	/**
	 * Writes a resource in a file in the local file system
	 * 
	 * @param docId The document's identifier
	 * @param fileVersion The file version
	 * @param suffix the suffix (eg conversion.pdf)
	 * @param outputFile the user in name of which to take this action
	 */
	public void writeToFile(long docId, String fileVersion, String suffix, String outputFile) {
		Store store = Context.get(Store.class);
		String resource = store.getResourceName(docId, fileVersion, suffix);
		try {
			store.writeToFile(docId, resource, new File(outputFile));
		} catch (IOException e) {
			log.error(e.getMessage(), e);
		}
	}

	/**
	 * Converts a document in PDF format and saves it as ancillary resource of
	 * the document with suffix
	 * {@link FormatConverterManager#PDF_CONVERSION_SUFFIX}. If the conversion
	 * already exists, nothing will be done.
	 * 
	 * @param doc the document to convert
	 */
	public void convertPDF(Document doc) {
		FormatConverterManager manager = Context.get(FormatConverterManager.class);
		try {
			manager.convertToPdf(doc, null);
		} catch (IOException e) {
			log.error(e.getMessage(), e);
		}
	}

	/**
	 * Convert a document in another format and saves the result in another file
	 * in the same folder
	 * 
	 * @param doc the document to convert
	 * @param format it is the output format(e.g. <b>pdf</b>, <b>txt</b>, ...)
	 * @param username the user in whose name the method is run
	 * 
	 * @return the generated conversion document
	 */
	public Document convert(Document doc, String format, String username) {
		User user = new SecurityTool().getUser(username);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setUser(user);

		FormatConverterManager manager = Context.get(FormatConverterManager.class);
		try {
			return manager.convert(doc, null, format, transaction);
		} catch (IOException | PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	/**
	 * Merges a set of documents into a single PDF file
	 * 
	 * @param documents the list of documents to merge(the order counts)
	 * @param targetFolderId identifier of the target folder
	 * @param fileName name of the output file(must ends with .pdf)
	 * @param username the user in whose name the method is run
	 * 
	 * @return the generated merged document
	 */
	public Document merge(Collection<Document> documents, long targetFolderId, String fileName, String username) {

		User user = new SecurityTool().getUser(username);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setUser(user);

		DocumentManager manager = Context.get(DocumentManager.class);
		try {
			return manager.merge(documents, targetFolderId, fileName, transaction).getDocument();
		} catch (IOException | PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	/**
	 * Retrieves a document by it's identifier
	 * 
	 * @param docId identifier of the document
	 * 
	 * @return the document object
	 */
	public Document findById(long docId) {
		DocumentDAO docDao = Context.get(DocumentDAO.class);
		try {
			Document doc = docDao.findDocument(docId);
			docDao.initialize(doc);
			return doc;
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
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
		DocumentDAO docDao = Context.get(DocumentDAO.class);
		try {
			return docDao.findByPath(path, tenantId != null ? tenantId : Tenant.DEFAULT_ID);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
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
		try {
			FolderDAO folderDao = Context.get(FolderDAO.class);
			String path = folderDao.computePathExtended(doc.getFolder().getId());
			if (!path.endsWith("/"))
				path += "/";
			path += doc.getFileName();
			return path;
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
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
			return new ArrayList<>();
		return docs.stream().map(d -> d.getId()).toList();
	}

	/**
	 * Creates a path, all existing nodes in the specified path will be reused
	 * 
	 * @param doc document used to take the root folder in case the
	 *        <code>targetPath</code> represents a relative path
	 * @param targetPath absolute or relative(in relation to the document's
	 *        parent folder) path to be created
	 * @param username the user in whose name the method is run
	 * 
	 * @return the folder leaf of the path
	 */
	public Folder createPath(Document doc, String targetPath, String username) {
		SecurityTool secTool = new SecurityTool();
		User user = secTool.getUser(username);

		FolderHistory transaction = new FolderHistory();
		transaction.setUser(user);

		Folder folder = null;
		try {
			FolderDAO fdao = Context.get(FolderDAO.class);
			Folder parent = doc.getFolder();

			if (targetPath.startsWith("/")) {
				targetPath = targetPath.substring(1);
				/*
				 * Cannot write in the root so if the parent is the root, we
				 * have to guarantee that the first element in the path is a
				 * workspace. If not the Default one will be used.
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
		DocumentHistoryDAO hDao = Context.get(DocumentHistoryDAO.class);

		try {
			return hDao.findByDocIdAndEvent(docId, event);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	/**
	 * Creates a new note for the whole document
	 * 
	 * @param doc the document
	 * @param text text of the note
	 * @param username username of the user in name of which this action is
	 *        taken
	 */
	public void addNote(Document doc, String text, String username) {
		User user = new SecurityTool().getUser(username);

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
			DocumentNoteDAO dao = Context.get(DocumentNoteDAO.class);
			dao.store(note, transaction);
		} catch (Exception t) {
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
		DocumentNoteDAO dao = Context.get(DocumentNoteDAO.class);

		String fVer = fileVersion;
		if (StringUtils.isEmpty(fileVersion)) {
			DocumentDAO docDao = Context.get(DocumentDAO.class);
			try {
				Document doc = docDao.findDocument(docId);
				fVer = doc.getFileVersion();
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
		}
		try {
			return dao.findByDocId(docId, fVer);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	/**
	 * Calculates what will be the next version specification. @see
	 * {@link Version#getNewVersionName(String, boolean)}
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
	 * This method finds a template by name
	 * 
	 * @param name Name of the template
	 * @param tenantId Identifier of the owning tenant
	 * 
	 * @return Template with given name
	 */
	public Template findTemplateByName(String name, long tenantId) {
		TemplateDAO dao = Context.get(TemplateDAO.class);

		try {
			return dao.findByName(name, tenantId);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	/**
	 * This method finds a template by ID
	 * 
	 * @param templateId Identifier of the template
	 * 
	 * @return Template with given name
	 */
	public Template findTemplateById(long templateId) {
		TemplateDAO dao = Context.get(TemplateDAO.class);
		try {
			return dao.findById(templateId);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	/**
	 * Counts the number of pages of a document
	 * 
	 * @param document the document
	 * 
	 * @return the number of pages
	 */
	public int countPages(Document document) {
		DocumentManager manager = Context.get(DocumentManager.class);
		return manager.countPages(document);
	}

	/**
	 * Extracts the texts from a document, using the same analyzer used for the
	 * full-text processing.
	 * 
	 * @param document The document to elaborate
	 * @param fileVersion Optional file version to consier
	 * 
	 * @return the extracted texts
	 */
	public String parse(Document document, String fileVersion) {
		DocumentManager manager = Context.get(DocumentManager.class);
		try {
			return manager.parseDocument(document, fileVersion);
		} catch (ParsingException e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	/**
	 * Same as {@link #DocTool.parse()} but first it tries to get the text from
	 * the already built fulltext index
	 * 
	 * @param document The document to elaborate
	 * 
	 * @return the extracted texts
	 */
	public String getText(Document document) {
		String text = null;

		if (document.getIndexed() == IndexingStatus.INDEXED) {
			SearchEngine indexer = Context.get(SearchEngine.class);
			Hit hit = indexer.getHit(document.getId());
			if (hit != null)
				text = hit.getContent();
		}

		if (StringUtils.isEmpty(text))
			text = parse(document, null);
		return text;
	}

	/**
	 * This method checks if the given permission is enabled for a document and
	 * an user.
	 * 
	 * @param permission the permission to check
	 * @param documentId ID of the folder
	 * @param userId ID of the user
	 * 
	 * @return if the permission is granted to the user
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public boolean isPermissionAllowed(String permission, long documentId, long userId) {
		try {
			return Context.get(DocumentDAO.class).isPermissionAllowed(Permission.valueOf(permission), documentId,
					userId);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return false;
		}
	}

	public boolean isWriteAllowed(long documentId, long userId) {
		return isPermissionAllowed(Permission.WRITE.name(), documentId, userId);
	}

	public boolean isReadAllowed(long documentId, long userId) {
		return isPermissionAllowed(Permission.READ.name(), documentId, userId);
	}

	public boolean isPreviewAllowed(long documentId, long userId) {
		return isPermissionAllowed(Permission.PREVIEW.name(), documentId, userId);
	}

	public boolean isDownloadAllowed(long documentId, long userId) {
		return isPermissionAllowed(Permission.DOWNLOAD.name(), documentId, userId);
	}
}