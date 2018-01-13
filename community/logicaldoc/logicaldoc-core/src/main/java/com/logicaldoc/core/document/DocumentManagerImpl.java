package com.logicaldoc.core.document;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.mail.MessagingException;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.document.dao.DocumentNoteDAO;
import com.logicaldoc.core.document.dao.VersionDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.parser.Parser;
import com.logicaldoc.core.parser.ParserFactory;
import com.logicaldoc.core.searchengine.SearchEngine;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.core.ticket.Ticket;
import com.logicaldoc.core.ticket.TicketDAO;
import com.logicaldoc.core.util.MailUtil;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.crypt.CryptUtil;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.time.TimeDiff;
import com.logicaldoc.util.time.TimeDiff.TimeField;

/**
 * Basic Implementation of <code>DocumentManager</code>
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 3.5
 */
public class DocumentManagerImpl implements DocumentManager {

	protected static Logger log = LoggerFactory.getLogger(DocumentManagerImpl.class);

	private DocumentDAO documentDAO;

	private DocumentNoteDAO documentNoteDAO;

	private FolderDAO folderDAO;

	private TemplateDAO templateDAO;

	private DocumentListenerManager listenerManager;

	private VersionDAO versionDAO;

	private UserDAO userDAO;

	private TicketDAO ticketDAO;

	private SearchEngine indexer;

	private Storer storer;

	private ContextProperties config;

	public void setListenerManager(DocumentListenerManager listenerManager) {
		this.listenerManager = listenerManager;
	}

	public void setDocumentDAO(DocumentDAO documentDAO) {
		this.documentDAO = documentDAO;
	}

	public void setTemplateDAO(TemplateDAO templateDAO) {
		this.templateDAO = templateDAO;
	}

	public void setIndexer(SearchEngine indexer) {
		this.indexer = indexer;
	}

	@Override
	public void checkin(long docId, File file, String filename, boolean release, Document docVO, History transaction)
			throws Exception {
		assert (transaction != null);
		assert (transaction.getUser() != null);
		assert (transaction.getComment() != null);
		assert (filename != null);

		transaction.setEvent(DocumentEvent.CHECKEDIN.toString());

		// identify the document and folder
		Document document = documentDAO.findById(docId);
		document.setComment(transaction.getComment());

		if (document.getImmutable() == 0) {
			documentDAO.initialize(document);

			Folder originalDocFolder = document.getFolder();

			// Check CustomId uniqueness
			if (docVO != null && docVO.getCustomId() != null) {
				Document test = documentDAO.findByCustomId(docVO.getCustomId(), document.getTenantId());
				if (test != null && test.getId() != docId)
					throw new Exception("Duplicated CustomID");
			}

			/*
			 * Now apply the metadata, if any
			 */
			if (docVO != null)
				document.copyAttributes(docVO);
			document.setFolder(originalDocFolder);

			try {
				checkEmailAttachments(file, document);
			} catch (Throwable t) {
				log.warn("Unable to detect the presence of attachments in the email file");
				log.debug(t.getMessage(), t);
			}

			Map<String, Object> dictionary = new HashMap<String, Object>();

			log.debug("Invoke listeners before checkin");
			for (DocumentListener listener : listenerManager.getListeners()) {
				listener.beforeCheckin(document, transaction, dictionary);
			}

			document.setStamped(0);
			document.setSigned(0);
			document.setPages(-1);

			if (document.getIndexed() != AbstractDocument.INDEX_SKIP)
				document.setIndexed(AbstractDocument.INDEX_TO_INDEX);
			if (document.getBarcoded() != AbstractDocument.BARCODE_SKIP)
				document.setBarcoded(AbstractDocument.BARCODE_TO_PROCESS);

			documentDAO.store(document);
			document = documentDAO.findById(document.getId());
			Folder folder = document.getFolder();
			documentDAO.initialize(document);

			// create some strings containing paths
			document.setFileName(filename);
			document.setType(FilenameUtils.getExtension(filename));

			// set other properties of the document
			document.setDate(new Date());
			document.setPublisher(transaction.getUsername());
			document.setPublisherId(transaction.getUserId());
			document.setStatus(Document.DOC_UNLOCKED);
			document.setLockUserId(null);
			document.setFolder(folder);
			document.setDigest(null);
			document.setFileSize(file.length());
			document.setExtResId(null);

			// Create new version (a new version number is created)
			Version version = Version.create(document, transaction.getUser(), transaction.getComment(),
					Version.EVENT_CHECKIN, release);
			document.setStatus(Document.DOC_UNLOCKED);
			if (documentDAO.store(document, transaction) == false)
				throw new Exception("Errors saving document " + document.getId());

			// store the document in the repository (on the file system)
			store(document, file);

			version.setFileSize(document.getFileSize());
			version.setDigest(null);
			versionDAO.store(version);
			log.debug("Stored version " + version.getVersion());

			log.debug("Invoke listeners after store");
			for (DocumentListener listener : listenerManager.getListeners()) {
				listener.afterCheckin(document, transaction, dictionary);
			}

			log.debug("Checked in document " + docId);

			documentNoteDAO.deleteContentAnnotations(docId);
		}
	}

	@Override
	public void checkin(long docId, InputStream content, String filename, boolean release, Document docVO,
			History transaction) throws Exception {
		assert (transaction != null);
		assert (transaction.getUser() != null);
		assert (transaction.getComment() != null);

		// Write content to temporary file, then delete it
		File tmp = File.createTempFile("checkin", "");
		try {
			FileUtil.writeFile(content, tmp.getPath());
			checkin(docId, tmp, filename, release, docVO, transaction);
		} finally {
			FileUtils.deleteQuietly(tmp);
		}
	}

	@Override
	public void checkout(long docId, History transaction) throws Exception {
		if (transaction.getEvent() == null)
			transaction.setEvent(DocumentEvent.CHECKEDOUT.toString());
		lock(docId, Document.DOC_CHECKED_OUT, transaction);
	}

	@Override
	public void lock(long docId, int status, History transaction) throws Exception {
		assert (transaction != null);
		assert (transaction.getUser() != null);

		Document document = documentDAO.findById(docId);

		if (document.getStatus() == status && document.getLockUserId() == transaction.getUserId()) {
			log.debug("Document " + document + " is already locked by user " + transaction.getUser().getFullName());
			return;
		}

		if (document.getStatus() != Document.DOC_UNLOCKED)
			throw new Exception("Document " + document + " is already locked by user " + document.getLockUser()
					+ " and cannot be locked by " + transaction.getUser().getFullName());

		documentDAO.initialize(document);
		document.setLockUserId(transaction.getUser().getId());
		document.setLockUser(transaction.getUser().getFullName());
		document.setStatus(status);
		document.setFolder(document.getFolder());

		// Modify document history entry
		documentDAO.store(document, transaction);

		log.debug("locked document " + docId);
	}

	private long store(Document doc, File file) throws IOException {
		Storer storer = (Storer) Context.get().getBean(Storer.class);
		String resourceName = storer.getResourceName(doc, null, null);

		// In case the resouce already exists, avoid the overwrite
		if (storer.exists(doc.getId(), resourceName))
			return storer.size(doc.getId(), resourceName);

		// Prepare the inputStream
		InputStream is = null;
		try {
			is = new BufferedInputStream(new FileInputStream(file), 2048);
		} catch (FileNotFoundException e) {
			return -1;
		}

		// stores it
		long stored = storer.store(is, doc.getId(), resourceName);
		if (stored < 0)
			throw new IOException("Unable to store the document");

		return stored;
	}

	/**
	 * Utility method for document removal from index
	 * 
	 * @param doc
	 */
	@Override
	public void deleteFromIndex(Document doc) {
		try {
			long docId = doc.getId();

			// Physically remove the document from full-text index
			if (doc != null) {
				indexer.deleteHit(docId);
			}

			doc.setIndexed(AbstractDocument.INDEX_TO_INDEX);
			documentDAO.store(doc);

			markAliasesToIndex(doc.getId());
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}
	}

	/**
	 * Retrieves the document's content as a string
	 * 
	 * @param doc The document representation
	 * @return The document's content
	 */
	@Override
	public String parseDocument(Document doc, String fileVersion) {
		String content = null;

		// Check if the document is an alias
		if (doc.getDocRef() != null) {
			long docref = doc.getDocRef();
			doc = documentDAO.findById(docref);
			if (doc == null)
				throw new RuntimeException("Unexisting referenced document " + docref);
		}

		// Parses the file where it is already stored
		Locale locale = doc.getLocale();
		String resource = storer.getResourceName(doc, fileVersion, null);
		Parser parser = ParserFactory.getParser(storer.getStream(doc.getId(), resource), doc.getFileName(), locale,
				null, doc.getTenantId());

		log.debug("Using parser " + parser.getClass().getName() + " to parse document " + doc.getId());

		// and gets some fields
		if (parser != null) {
			content = parser.getContent();
		}
		if (content == null) {
			content = "";
		}
		return content;
	}

	@Override
	public long reindex(long docId, String content) throws Exception {
		Document doc = documentDAO.findById(docId);
		if (doc == null) {
			log.warn("Unexisting document with ID: " + docId);
			return 0;
		}

		log.debug("Reindexing document " + docId + " - " + doc.getFileName());

		try {
			long parsingTime = 0;
			String cont = content;
			if (StringUtils.isEmpty(cont)) {
				// Extracts the content from the file. This may take very long
				// time.
				Date beforeParsing = new Date();
				cont = parseDocument(doc, null);
				parsingTime = TimeDiff.getTimeDifference(beforeParsing, new Date(), TimeField.MILLISECOND);
			}

			// This may take time
			indexer.addHit(doc, cont);

			// For additional safety update the DB directly
			doc.setIndexed(AbstractDocument.INDEX_INDEXED);
			documentDAO.store(doc);

			markAliasesToIndex(docId);

			return parsingTime;
		} catch (Throwable e) {
			log.error("Error reindexing document " + docId + " - " + doc.getFileName());
			throw e;
		}
	}

	private void markAliasesToIndex(long referencedDocId) {
		documentDAO.jdbcUpdate("update ld_document set ld_indexed=" + AbstractDocument.INDEX_TO_INDEX
				+ " where ld_docref=" + referencedDocId + " and not ld_id = " + referencedDocId);
	}

	@Override
	public void update(Document doc, Document docVO, History transaction) throws Exception {
		assert (transaction != null);
		assert (transaction.getUser() != null);
		assert (doc != null);
		assert (docVO != null);
		try {
			documentDAO.initialize(doc);
			if (doc.getImmutable() == 0 || ((doc.getImmutable() == 1 && transaction.getUser().isMemberOf("admin")))) {
				History renameTransaction = null;
				if (!doc.getFileName().equals(docVO.getFileName()) && docVO.getFileName() != null) {
					renameTransaction = (History) transaction.clone();
					renameTransaction.setFilenameOld(doc.getFileName());
					renameTransaction.setEvent(DocumentEvent.RENAMED.toString());
				}

				// Check CustomId uniqueness
				if (docVO.getCustomId() != null) {
					Document test = documentDAO.findByCustomId(docVO.getCustomId(), docVO.getTenantId());
					if (test != null && test.getId() != doc.getId())
						throw new Exception("Duplicated CustomID");
					doc.setCustomId(docVO.getCustomId());
				}

				// The document must be re-indexed
				doc.setIndexed(AbstractDocument.INDEX_TO_INDEX);
				doc.setBarcoded(docVO.getBarcoded());
				doc.setWorkflowStatus(docVO.getWorkflowStatus());

				// Save retention policies
				doc.setPublished(docVO.getPublished());
				doc.setStartPublishing(docVO.getStartPublishing());
				doc.setStopPublishing(docVO.getStopPublishing());

				// Intercept locale changes
				if (!doc.getLocale().equals(docVO.getLocale())) {
					indexer.deleteHit(doc.getId());
					doc.setLocale(docVO.getLocale());
				}

				if (StringUtils.isNotEmpty(docVO.getFileName())) {
					if (!doc.getFileName().equals(docVO.getFileName())) {
						doc.setFileName(docVO.getFileName());
					}
				}

				doc.clearTags();
				doc.setTags(docVO.getTags());

				Template template = docVO.getTemplate();
				if (template == null && docVO.getTemplateId() != null)
					template = templateDAO.findById(docVO.getTemplateId());

				// Change the template and attributes
				if (template != null) {
					doc.setTemplate(template);
					doc.setTemplateId(template.getId());
					if (docVO.getAttributes() != null) {
						doc.getAttributes().clear();
						for (String attrName : docVO.getAttributes().keySet()) {
							if (template.getAttributes().get(attrName) != null) {
								Attribute templateExtAttribute = template.getAttributes().get(attrName);
								Attribute docExtendedAttribute = docVO.getAttributes().get(attrName);
								docExtendedAttribute.setMandatory(templateExtAttribute.getMandatory());
								docExtendedAttribute.setLabel(templateExtAttribute.getLabel());
								if (templateExtAttribute.getType() == docExtendedAttribute.getType()) {
									doc.getAttributes().put(attrName, docExtendedAttribute);
								} else {
									throw new Exception("The given type value is not correct for attribute: "
											+ attrName);
								}
							} else {
								throw new Exception("The attribute name '" + attrName
										+ "' is not correct for template '" + template.getName() + "'.");
							}
						}
					}
				} else {
					doc.setTemplate(null);
				}

				// create a new version
				Version version = Version.create(doc, transaction.getUser(), transaction.getComment(),
						Version.EVENT_CHANGED, false);

				// Modify document history entry
				doc.setVersion(version.getVersion());
				if (renameTransaction != null) {
					renameTransaction.setUser(transaction.getUser());
					documentDAO.store(doc, renameTransaction);
				} else {
					documentDAO.store(doc, transaction);
				}
				versionDAO.store(version);

				markAliasesToIndex(doc.getId());
			} else {
				throw new Exception(String.format("Document %s is immutable", doc));
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public void moveToFolder(Document doc, Folder folder, History transaction) throws Exception {
		assert (transaction != null);
		assert (transaction.getUser() != null);

		if (folder.equals(doc.getFolder()))
			return;

		if (doc.getImmutable() == 0 || ((doc.getImmutable() == 1 && transaction.getUser().isMemberOf("admin")))) {
			documentDAO.initialize(doc);
			transaction.setPathOld(folderDAO.computePathExtended(doc.getFolder().getId()));
			transaction.setFilenameOld(doc.getFileName());

			doc.setFolder(folder);

			// The document needs to be reindexed
			if (doc.getIndexed() == AbstractDocument.INDEX_INDEXED) {
				doc.setIndexed(AbstractDocument.INDEX_TO_INDEX);
				indexer.deleteHit(doc.getId());

				// The same thing should be done on each shortcut
				documentDAO.jdbcUpdate("update ld_document set ld_indexed=" + AbstractDocument.INDEX_TO_INDEX
						+ " where ld_docref=" + doc.getId());
			}

			// Modify document history entry
			if (transaction.getEvent().trim().isEmpty())
				transaction.setEvent(DocumentEvent.MOVED.toString());

			documentDAO.store(doc, transaction);

			Version version = Version.create(doc, transaction.getUser(), transaction.getComment(), Version.EVENT_MOVED,
					false);
			version.setId(0);
			versionDAO.store(version);
		} else {
			throw new Exception("Document is immutable");
		}
	}

	@Override
	public Document create(File file, Document docVO, History transaction) throws Exception {
		assert (transaction != null);
		assert (docVO != null);

		boolean documentSaved = false;
		try {
			String type = "unknown";
			int lastDotIndex = docVO.getFileName().lastIndexOf(".");
			if (lastDotIndex > 0) {
				type = FilenameUtils.getExtension(docVO.getFileName()).toLowerCase();
			}

			if (docVO.getDate() == null)
				docVO.setDate(new Date());

			if (docVO.getCreation() == null)
				docVO.setCreation(docVO.getDate());

			if (StringUtils.isNotEmpty(docVO.getPublisher()))
				docVO.setPublisher(docVO.getPublisher());
			else
				docVO.setPublisher(transaction.getUsername());

			if (docVO.getPublisherId() != 0L)
				docVO.setPublisherId(docVO.getPublisherId());
			else
				docVO.setPublisherId(transaction.getUserId());

			if (StringUtils.isNotEmpty(docVO.getCreator()))
				docVO.setCreator(docVO.getCreator());
			else
				docVO.setCreator(transaction.getUsername());

			if (docVO.getCreatorId() != 0L)
				docVO.setCreatorId(docVO.getCreatorId());
			else
				docVO.setCreatorId(transaction.getUserId());

			docVO.setStatus(Document.DOC_UNLOCKED);
			docVO.setType(type);
			docVO.setVersion(config.getProperty("document.startversion"));
			docVO.setFileVersion(docVO.getVersion());
			docVO.setFileSize(file.length());

			if (docVO.getTemplate() == null && docVO.getTemplateId() != null)
				docVO.setTemplate(templateDAO.findById(docVO.getTemplateId()));

			/* Set template and extended attributes */
			if (docVO.getTemplate() != null) {
				for (String attrName : docVO.getAttributeNames()) {
					if (docVO.getTemplate().getAttributes().get(attrName) != null) {
						Attribute templateExtAttribute = docVO.getTemplate().getAttributes().get(attrName);
						Attribute docExtendedAttribute = docVO.getAttribute(attrName);
						if (templateExtAttribute.getType() == docExtendedAttribute.getType()) {
							docVO.getAttributes().put(attrName, docExtendedAttribute);
						} else {
							throw new Exception("The given type value is not correct.");
						}
					}
				}
			}

			docVO.setId(0L);

			if (file != null)
				transaction.setFile(file.getAbsolutePath());

			try {
				checkEmailAttachments(file, docVO);
			} catch (Throwable t) {
				log.warn("Unable to detect the presence of attachments in the email file");
				log.debug(t.getMessage(), t);
			}

			// Create the record
			transaction.setEvent(DocumentEvent.STORED.toString());
			documentSaved = documentDAO.store(docVO, transaction);

			if (documentSaved) {
				/* store the document into filesystem */
				if (file != null)
					try {
						store(docVO, file);
					} catch (Throwable e) {
						documentDAO.delete(docVO.getId());
						throw new Exception("Unable to store the document's file", e);
					}

				// Store the initial version (default 1.0)
				Version vers = Version.create(docVO, userDAO.findById(transaction.getUserId()),
						transaction.getComment(), Version.EVENT_STORED, true);
				versionDAO.store(vers);

				log.debug("Stored version " + vers.getVersion());
				return docVO;
			} else
				throw new Exception("Document not stored in the database");
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			throw new Exception(e);
		}
	}

	/**
	 * In case of email, put pages to 2 to mark the existence of attachments
	 */
	private void checkEmailAttachments(File file, Document doc) throws MessagingException, IOException {
		if (doc.getFileName().toLowerCase().endsWith(".eml") || doc.getFileName().toLowerCase().endsWith(".msg"))
			if (doc.getTemplate() != null && doc.getTemplate().getName().equals("email")
					&& doc.getValue("attachments") != null && "true".equals(doc.getValue("attachments"))) {
				doc.setPages(2);
			} else {
				EMail email = doc.getFileName().endsWith(".eml") ? MailUtil.messageToMail(file, false) : MailUtil
						.msgToMail(file, false);
				if (email.getAttachmentsCount() > 0)
					doc.setPages(2);
			}
	}

	@Override
	public Document create(InputStream content, Document docVO, History transaction) throws Exception {
		assert (transaction != null);
		assert (docVO != null);

		// Write content to temporary file, then delete it
		File tmp = File.createTempFile("create", "");
		try {
			if (content != null)
				FileUtil.writeFile(content, tmp.getPath());
			return create(tmp, docVO, transaction);
		} finally {
			FileUtils.deleteQuietly(tmp);
		}
	}

	public Document copyToFolder(Document doc, Folder folder, History transaction) throws Exception {
		assert (transaction != null);
		assert (transaction.getUser() != null);

		// initialize the document
		documentDAO.initialize(doc);

		if (doc.getDocRef() != null) {
			return createAlias(doc, folder, doc.getDocRefType(), transaction);
		}

		String resource = storer.getResourceName(doc, null, null);
		InputStream is = storer.getStream(doc.getId(), resource);
		try {
			Document cloned = (Document) doc.clone();
			cloned.setId(0);
			if (doc.getFolder().getId() != folder.getId())
				cloned.setFolder(folder);
			cloned.setLastModified(null);
			cloned.setDate(null);
			if (cloned.getBarcoded() == Document.BARCODE_PROCESSED)
				cloned.setBarcoded(Document.BARCODE_TO_PROCESS);
			if (cloned.getIndexed() == Document.INDEX_INDEXED)
				cloned.setIndexed(Document.INDEX_TO_INDEX);
			return create(is, cloned, transaction);
		} finally {
			if (is != null)
				is.close();
			is = null;
		}
	}

	@Override
	public void unlock(long docId, History transaction) throws Exception {
		assert (transaction != null);
		assert (transaction.getUser() != null);

		Document document = documentDAO.findById(docId);
		documentDAO.initialize(document);

		if (transaction.getUser().isMemberOf("admin")) {
			document.setImmutable(0);
		} else if (document.getLockUserId() == null || document.getStatus() == Document.DOC_UNLOCKED) {
			log.debug("The document " + document + " is already unlocked");
			return;
		} else if (transaction.getUserId() != document.getLockUserId()) {
			String message = "The document " + document + " is locked by " + document.getLockUser()
					+ " and cannot be unlocked by " + transaction.getUser().getFullName();
			throw new Exception(message);
		}

		document.setLockUserId(null);
		document.setLockUser(null);
		document.setExtResId(null);
		document.setStatus(Document.DOC_UNLOCKED);

		// Modify document history entry
		transaction.setEvent(DocumentEvent.UNLOCKED.toString());
		documentDAO.store(document, transaction);

		log.debug("Unlocked document " + docId);
	}

	@Override
	public void makeImmutable(long docId, History transaction) throws Exception {
		assert (transaction != null);
		assert (transaction.getUser() != null);

		Document document = documentDAO.findById(docId);
		if (document.getImmutable() == 0) {
			// Modify document history entry
			transaction.setEvent(DocumentEvent.IMMUTABLE.toString());
			documentDAO.makeImmutable(docId, transaction);

			log.debug("The document " + docId + " has been marked as immutable ");
		} else {
			throw new Exception("Document is immutable");
		}
	}

	@Override
	public void rename(Document doc, String newName, History transaction) throws Exception {
		assert (doc != null);
		assert (transaction != null);
		assert (transaction.getUser() != null);

		Document document = doc;
		if (doc.getDocRef() != null) {
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			document = docDao.findById(doc.getDocRef());
		}

		if (document.getImmutable() == 0 || ((doc.getImmutable() == 1 && transaction.getUser().isMemberOf("admin")))) {
			documentDAO.initialize(document);
			document.setFileName(newName.trim());
			String extension = FilenameUtils.getExtension(newName.trim());
			if (StringUtils.isNotEmpty(extension)) {
				document.setType(FilenameUtils.getExtension(newName));
			} else {
				document.setType("unknown");
			}

			document.setIndexed(AbstractDocument.INDEX_TO_INDEX);

			// Modify document history entry
			transaction.setEvent(DocumentEvent.RENAMED.toString());
			documentDAO.store(document, transaction);

			Version version = Version.create(document, transaction.getUser(), transaction.getComment(),
					Version.EVENT_RENAMED, false);
			versionDAO.store(version);

			markAliasesToIndex(doc.getId());

			log.debug("Document renamed: " + document.getId());
		} else {
			throw new Exception("Document is immutable");
		}
	}

	@Override
	public Document replaceAlias(long aliasId, History transaction) throws Exception {
		assert (transaction != null);
		assert (transaction.getUser() != null);

		try {
			// get the alias
			Document alias = documentDAO.findById(aliasId);
			if (alias == null || alias.getDocRef() == null)
				throw new Exception(String.format("Unable to find alias %s", aliasId));

			Folder folder = alias.getFolder();
			folderDAO.initialize(folder);

			if (!folderDAO.isWriteEnabled(alias.getFolder().getId(), transaction.getUserId()))
				throw new Exception(String.format("User %s without WRITE permission in folder %s",
						transaction.getUsername(), folder.getId()));

			Document originalDoc = documentDAO.findById(alias.getDocRef());
			documentDAO.initialize(originalDoc);
			documentDAO.delete(aliasId, transaction);

			return copyToFolder(originalDoc, folder, (History) transaction.clone());
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public Document createAlias(Document doc, Folder folder, String aliasType, History transaction) throws Exception {
		assert (doc != null);
		assert (folder != null);
		assert (transaction != null);
		assert (transaction.getUser() != null);

		try {
			// initialize the document
			documentDAO.initialize(doc);

			Document alias = new Document();
			alias.setFolder(folder);
			alias.setFileName(doc.getFileName());
			alias.setDate(new Date());

			String type = "unknown";
			int lastDotIndex = doc.getFileName().lastIndexOf(".");
			if (lastDotIndex > 0)
				type = FilenameUtils.getExtension(doc.getFileName());

			if (StringUtils.isNotEmpty(aliasType)) {
				alias.setFileName(FilenameUtils.getBaseName(doc.getFileName()) + "."
						+ FilenameUtils.getExtension(aliasType).toLowerCase());
				type = FilenameUtils.getExtension(aliasType).toLowerCase();
			}

			alias.setPublisher(transaction.getUsername());
			alias.setPublisherId(transaction.getUserId());
			alias.setCreator(transaction.getUsername());
			alias.setCreatorId(transaction.getUserId());
			alias.setStatus(Document.DOC_UNLOCKED);
			alias.setType(type);

			// Set the Doc Reference
			if (doc.getDocRef() == null) {
				// Set the docref as the id of the original document
				alias.setDocRef(doc.getId());
			} else {
				// The doc is a shortcut, so we still copy a shortcut
				alias.setDocRef(doc.getDocRef());
			}
			alias.setDocRefType(aliasType);

			// Modify document history entry
			transaction.setEvent(DocumentEvent.SHORTCUT_STORED.toString());

			documentDAO.store(alias, transaction);

			return alias;
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			throw e;
		}
	}

	public void setVersionDAO(VersionDAO versionDAO) {
		this.versionDAO = versionDAO;
	}

	public void setStorer(Storer storer) {
		this.storer = storer;
	}

	public void setConfig(ContextProperties config) {
		this.config = config;
	}

	@Override
	public void changeIndexingStatus(Document doc, int status) {
		assert (status != AbstractDocument.INDEX_INDEXED);

		if (status == AbstractDocument.INDEX_SKIP && doc.getIndexed() == AbstractDocument.INDEX_SKIP)
			return;
		if (status == AbstractDocument.INDEX_TO_INDEX && doc.getIndexed() == AbstractDocument.INDEX_TO_INDEX)
			return;

		documentDAO.initialize(doc);
		if (doc.getIndexed() == AbstractDocument.INDEX_INDEXED)
			deleteFromIndex(doc);
		doc.setIndexed(status);
		documentDAO.store(doc);
	}

	public void setUserDAO(UserDAO userDAO) {
		this.userDAO = userDAO;
	}

	public void setFolderDAO(FolderDAO folderDAO) {
		this.folderDAO = folderDAO;
	}

	@Override
	public Version deleteVersion(long versionId, History transaction) throws Exception {
		Version versionToDelete = versionDAO.findById(versionId);
		assert (versionToDelete != null);

		String versionToDeleteSpec = versionToDelete.getVersion();

		Document document = documentDAO.findById(versionToDelete.getDocId());
		assert (document != null);

		List<Version> versions = versionDAO.findByDocId(versionToDelete.getDocId());

		// Exit if there is only one version
		if (versions.size() == 1)
			return versions.get(0);

		// Iterate over the versions to check if the file is referenced by other
		// versions
		boolean referenced = false;
		for (Version v : versions)
			if (v.getId() != versionId && versionToDelete.getFileVersion().equals(v.getFileVersion())) {
				referenced = true;
				break;
			}

		// If no more referenced, can delete the document's resources
		if (!referenced) {
			List<String> resources = storer.listResources(versionToDelete.getDocId(), versionToDelete.getFileVersion());
			for (String resource : resources)
				try {
					storer.delete(versionToDelete.getDocId(), resource);
				} catch (Throwable t) {
					log.warn("Unable to delete resource " + resource + " od document " + versionToDelete.getDocId());
				}
		}

		versionDAO.delete(versionId);

		// Save the version deletion history
		History delHistory = null;
		if (transaction != null) {
			delHistory = (History) transaction.clone();
			delHistory.setEvent(DocumentEvent.VERSION_DELETED.toString());
			delHistory.setComment(versionToDeleteSpec + " - " + versionToDelete.getFileVersion());
		}
		documentDAO.saveDocumentHistory(document, delHistory);

		versions = versionDAO.findByDocId(versionToDelete.getDocId());

		Version lastVersion = null;
		for (Version version : versions) {
			if (version.getDeleted() == 0 && version.getId() != versionToDelete.getId()) {
				lastVersion = version;
				break;
			}
		}

		/*
		 * Downgrade the document version in case the deleted version is the
		 * actual one
		 */
		String currentVersion = document.getVersion();
		if (currentVersion.equals(versionToDeleteSpec)) {
			documentDAO.initialize(document);
			document.setVersion(lastVersion.getVersion());
			document.setFileVersion(lastVersion.getFileVersion());

			if (transaction != null) {
				transaction.setEvent(DocumentEvent.CHANGED.toString());
				transaction.setComment("Version changed to " + document.getVersion() + " (" + document.getFileVersion()
						+ ")");
			}

			documentDAO.store(document, transaction);
		}

		return lastVersion;
	}

	public void setDocumentNoteDAO(DocumentNoteDAO documentNoteDAO) {
		this.documentNoteDAO = documentNoteDAO;
	}

	@Override
	public long archiveFolder(long folderId, History transaction) throws Exception {
		List<Long> docIds = new ArrayList<Long>();
		Folder root = folderDAO.findFolder(folderId);

		Collection<Long> folderIds = folderDAO.findFolderIdByUserIdAndPermission(transaction.getUserId(),
				Permission.ARCHIVE, root.getId(), true);
		for (Long fid : folderIds) {
			String where = " where ld_deleted=0 and not ld_status=" + AbstractDocument.DOC_ARCHIVED
					+ " and ld_folderid=" + fid;
			@SuppressWarnings("unchecked")
			List<Long> ids = (List<Long>) documentDAO
					.queryForList("select ld_id from ld_document " + where, Long.class);
			if (ids.isEmpty())
				continue;
			docIds.addAll(ids);
			long[] idsArray = new long[ids.size()];
			for (int i = 0; i < idsArray.length; i++)
				idsArray[i] = ids.get(i).longValue();
			archiveDocuments(idsArray, transaction);
		}

		return docIds.size();
	}

	@Override
	public void archiveDocuments(long[] docIds, History transaction) throws Exception {
		assert (transaction.getUser() != null);
		List<Long> idsList = new ArrayList<Long>();
		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Collection<Long> folderIds = folderDAO.findFolderIdByUserIdAndPermission(transaction.getUserId(),
				Permission.ARCHIVE, null, true);

		for (long id : docIds) {
			Document doc = dao.findById(id);

			// Skip documents in folders without Archive permission
			if (!(transaction.getUser().isMemberOf("admin") || transaction.getUser().getUsername().equals("_retention"))
					&& !folderIds.contains(doc.getFolder().getId()))
				continue;

			// Create the document history event
			History t = (History) transaction.clone();
			if (dao.archive(id, t))
				idsList.add(id);
		}

		// Remove all corresponding hits from the index
		SearchEngine engine = (SearchEngine) Context.get().getBean(SearchEngine.class);
		engine.deleteHits(idsList);

		log.info("Archived documents " + idsList);
	}

	@Override
	public Ticket createDownloadTicket(long docId, String suffix, Integer expireHours, Date expireDate,
			String urlPrefix, History transaction) throws Exception {
		assert (transaction.getUser() != null);

		Document document = documentDAO.findById(docId);
		if (document == null)
			throw new Exception("Unexisting document");

		if (!folderDAO.isDownloadEnabled(document.getFolder().getId(), transaction.getUserId()))
			throw new RuntimeException("You don't have the download permission");

		Ticket ticket = prepareTicket(docId, transaction.getUser());
		ticket.setSuffix(suffix);

		Calendar cal = GregorianCalendar.getInstance();
		if (expireDate != null) {
			cal.setTime(expireDate);
			cal.set(Calendar.HOUR_OF_DAY, 23);
			cal.set(Calendar.MINUTE, 59);
			cal.set(Calendar.SECOND, 59);
			cal.set(Calendar.MILLISECOND, 999);
			ticket.setExpired(cal.getTime());
		} else if (expireHours != null) {
			cal.add(Calendar.HOUR_OF_DAY, expireHours.intValue());
			ticket.setExpired(cal.getTime());
		} else {
			cal.add(Calendar.HOUR_OF_DAY, config.getInt("ticket.ttl"));
			ticket.setExpired(cal.getTime());
		}

		transaction.setEvent(DocumentEvent.DTICKET_CREATED.toString());
		transaction.setSessionId(transaction.getSessionId());

		ticketDAO.store(ticket, transaction);

		// Try to clean the DB from old tickets
		ticketDAO.deleteExpired();

		ticket.setUrl(composeTicketUrl(ticket, urlPrefix));

		return ticket;
	}

	private Ticket prepareTicket(long docId, User user) {
		String temp = new Date().toString() + user.getId();
		String ticketid = CryptUtil.cryptString(temp);
		Ticket ticket = new Ticket();
		ticket.setTicketId(ticketid);
		ticket.setDocId(docId);
		ticket.setUserId(user.getId());
		return ticket;
	}

	private String composeTicketUrl(Ticket ticket, String urlPrefix) {
		if (StringUtils.isEmpty(urlPrefix))
			urlPrefix = config.getProperty("server.url");
		String address = urlPrefix + "/download-ticket?ticketId=" + ticket.getTicketId();
		return address;
	}

	public void setTicketDAO(TicketDAO ticketDAO) {
		this.ticketDAO = ticketDAO;
	}

	@Override
	public boolean unprotect(String sid, long docId, String password) {
		Session session = SessionManager.get().get(sid);
		if (!session.isOpen())
			return false;

		if (session.getUnprotectedDocs().containsKey(docId))
			return session.getUnprotectedDocs().get(docId).equals(password);

		Document doc = documentDAO.findDocument(docId);
		if (!doc.isPasswordProtected())
			return true;

		boolean granted = doc.isGranted(password);
		if (granted)
			session.getUnprotectedDocs().put(docId, password);
		return granted;
	}
}