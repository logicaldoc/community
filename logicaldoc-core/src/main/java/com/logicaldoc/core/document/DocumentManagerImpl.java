package com.logicaldoc.core.document;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.pdfbox.io.MemoryUsageSetting;
import org.apache.pdfbox.multipdf.PDFMergerUtility;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.conversion.FormatConverterManager;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.document.dao.DocumentNoteDAO;
import com.logicaldoc.core.document.dao.VersionDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.parser.ParseException;
import com.logicaldoc.core.parser.Parser;
import com.logicaldoc.core.parser.ParserFactory;
import com.logicaldoc.core.searchengine.SearchEngine;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.core.threading.ThreadPools;
import com.logicaldoc.core.ticket.Ticket;
import com.logicaldoc.core.ticket.TicketDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.time.TimeDiff;
import com.logicaldoc.util.time.TimeDiff.TimeField;

/**
 * Basic Implementation of <code>DocumentManager</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.5
 */
public class DocumentManagerImpl implements DocumentManager {

	private static final String MERGE = "merge";

	private static final String UNKNOWN = "unknown";

	private static final String DOCUMENT_IS_IMMUTABLE = "Document is immutable";

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
	public void replaceFile(long docId, String fileVersion, InputStream content, DocumentHistory transaction)
			throws IOException, PersistenceException {
		assert (transaction != null);

		// Write content to temporary file, then delete it
		File tmp = FileUtil.createTempFile("replacefile", "");
		try {
			if (content != null)
				FileUtil.writeFile(content, tmp.getPath());
			replaceFile(docId, fileVersion, tmp, transaction);
		} finally {
			FileUtils.deleteQuietly(tmp);
		}
	}

	@Override
	public void replaceFile(long docId, String fileVersion, File newFile, DocumentHistory transaction)
			throws PersistenceException, IOException {
		validateTransaction(transaction);
		assert transaction.getComment() != null : "No comment in transaction";

		transaction.setEvent(DocumentEvent.VERSION_REPLACED.toString());
		transaction.setComment(String.format("file version %s - %s", fileVersion, transaction.getComment()));

		// identify the document and folder
		Document document = documentDAO.findDocument(docId);

		if (document.getImmutable() == 0 && document.getStatus() == AbstractDocument.DOC_UNLOCKED) {
			// Remove the files of the same fileVersion
			List<String> resources = storer.listResources(document.getId(), fileVersion);
			for (String resource : resources)
				storer.delete(document.getId(), resource);

			// Store the new file
			storer.store(newFile, document.getId(), storer.getResourceName(document, fileVersion, null));

			long fileSize = newFile.length();

			// Now update the file size in the versions
			List<Version> versions = versionDAO.findByDocId(document.getId());
			for (Version version : versions) {
				if (version.getFileVersion().equals(fileVersion)) {
					versionDAO.initialize(version);
					version.setFileSize(fileSize);
					storeVersionAsync(version);
				}
			}

			// Update the document's record
			documentDAO.initialize(document);
			document.setFileSize(fileSize);
			if (document.getIndexed() != AbstractDocument.INDEX_SKIP)
				document.setIndexed(AbstractDocument.INDEX_TO_INDEX);
			document.setOcrd(0);
			document.setBarcoded(0);
			document.setSigned(0);
			document.setStamped(0);
			documentDAO.store(document, transaction);

			log.debug("Replaced fileVersion {} of document {}", fileVersion, docId);
		}
	}

	private void validateTransaction(DocumentHistory transaction) {
		assert transaction != null : "No transaction";
		assert transaction.getUser() != null : "No user in transaction";
	}

	@Override
	public void checkin(long docId, File file, String filename, boolean release, AbstractDocument docVO,
			DocumentHistory transaction) throws PersistenceException {
		validateTransaction(transaction);
		assert transaction.getComment() != null : "No comment in transaction";
		assert filename != null : "File name is mandatory";

		transaction.setEvent(DocumentEvent.CHECKEDIN.toString());

		/*
		 * Better to synchronize this block because under high multi-threading
		 * may lead to hibernate's sessions rollbacks
		 */
		synchronized (this) {
			// identify the document and folder
			Document document = documentDAO.findDocument(docId);
			String oldFileVersion = document.getFileVersion();

			document.setComment(transaction.getComment());

			Document oldDocument = null;
			if (document.getImmutable() == 0) {
				documentDAO.initialize(document);

				oldDocument = new Document(document);

				// Check CustomId uniqueness
				checkCustomIdUniquenessOnCheckin(document, docVO);

				/*
				 * Now apply the metadata, if any
				 */
				if (docVO != null) {
					Folder originalFolder = document.getFolder();
					String originalVersion = document.getVersion();
					String originalFileVersion = document.getFileVersion();
					String originalFileName = document.getFileName();

					document.copyAttributes(docVO);

					// Restore important original information
					document.setFolder(originalFolder);
					document.setVersion(originalVersion);
					document.setFileVersion(originalFileVersion);
					if (StringUtils.isNotEmpty(filename))
						document.setFileName(filename);
					else
						document.setFileName(originalFileName);
				}

				countPages(file, document);

				Map<String, Object> dictionary = new HashMap<String, Object>();

				log.debug("Invoke listeners before checkin");
				for (DocumentListener listener : listenerManager.getListeners())
					listener.beforeCheckin(document, transaction, dictionary);

				document.setStamped(0);
				document.setSigned(0);
				document.setOcrd(0);
				document.setBarcoded(0);

				if (document.getIndexed() != AbstractDocument.INDEX_SKIP)
					document.setIndexed(AbstractDocument.INDEX_TO_INDEX);

				documentDAO.store(document);

				document = documentDAO.findById(document.getId());
				Folder folder = document.getFolder();
				documentDAO.initialize(document);

				// create some strings containing paths
				document.setFileName(filename);
				document.setType(FileUtil.getExtension(filename));

				// set other properties of the document
				document.setDate(new Date());
				document.setPublisher(transaction.getUsername());
				document.setPublisherId(transaction.getUserId());
				document.setStatus(AbstractDocument.DOC_UNLOCKED);
				document.setLockUserId(null);
				document.setFolder(folder);
				document.setDigest(null);
				document.setFileSize(file.length());
				document.setExtResId(null);

				// Create new version (a new version number is created)
				Version version = Version.create(document, transaction.getUser(), transaction.getComment(),
						DocumentEvent.CHECKEDIN.toString(), release);

				document.setStatus(AbstractDocument.DOC_UNLOCKED);
				documentDAO.store(document, transaction);

				// store the document in the repository (on the file system)
				try {
					storeFile(document, file);
				} catch (IOException ioe) {
					log.error("Cannot save the new version {} into the storage", document, ioe);

					document.copyAttributes(oldDocument);
					document.setOcrd(oldDocument.getOcrd());
					document.setOcrTemplateId(oldDocument.getOcrTemplateId());
					document.setBarcoded(oldDocument.getBarcoded());
					document.setBarcodeTemplateId(oldDocument.getBarcodeTemplateId());
					document.setIndexed(oldDocument.getIndexed());
					document.setCustomId(oldDocument.getCustomId());
					document.setStatus(oldDocument.getStatus());
					document.setStamped(oldDocument.getStamped());
					document.setSigned(oldDocument.getSigned());
					document.setComment(oldDocument.getComment());
					documentDAO.store(document);
					throw new PersistenceException(ioe.getMessage());
				}

				version.setFileSize(document.getFileSize());
				version.setDigest(null);
				storeVersionAsync(version);

				log.debug("Stored version {}", version.getVersion());
				log.debug("Invoke listeners after checkin");
				for (DocumentListener listener : listenerManager.getListeners())
					listener.afterCheckin(document, transaction, dictionary);
				documentDAO.store(document);

				log.debug("Checked in document {}", docId);

				if (!document.getFileVersion().equals(oldFileVersion))
					documentNoteDAO.copyAnnotations(document.getId(), oldFileVersion, document.getFileVersion());
			}
		}
	}

	private void checkCustomIdUniquenessOnCheckin(Document document, AbstractDocument docVO)
			throws PersistenceException {
		if (docVO != null && docVO.getCustomId() != null) {
			Document test = documentDAO.findByCustomId(docVO.getCustomId(), document.getTenantId());
			if (test != null && test.getId() != document.getId())
				throw new PersistenceException("Duplicated CustomID");
		}
	}

	@Override
	public void checkin(long docId, InputStream content, String filename, boolean release, AbstractDocument docVO,
			DocumentHistory transaction) throws IOException, PersistenceException {
		validateTransaction(transaction);
		assert transaction.getComment() != null : "No comment in transaction";

		// Write content to temporary file, then delete it
		File tmp = FileUtil.createTempFile("checkin", "");
		try {
			FileUtil.writeFile(content, tmp.getPath());
			checkin(docId, tmp, filename, release, docVO, transaction);
		} finally {
			FileUtils.deleteQuietly(tmp);
		}
	}

	@Override
	public void checkout(long docId, DocumentHistory transaction) throws PersistenceException {
		if (transaction.getEvent() == null)
			transaction.setEvent(DocumentEvent.CHECKEDOUT.toString());
		lock(docId, AbstractDocument.DOC_CHECKED_OUT, transaction);
	}

	@Override
	public void lock(long docId, int status, DocumentHistory transaction) throws PersistenceException {
		validateTransaction(transaction);

		/*
		 * Better to synchronize this block because under high multi-threading
		 * may lead to hibernate's sessions rollbacks
		 */
		synchronized (this) {
			Document document = documentDAO.findDocument(docId);

			if (document.getStatus() == status && document.getLockUserId().equals(transaction.getUserId())) {
				log.debug("Document {} is already locked by user {}", document, transaction.getUser().getFullName());
				return;
			}

			if (document.getStatus() != AbstractDocument.DOC_UNLOCKED)
				throw new PersistenceException(
						String.format("Document %s is already locked by user %s and cannot be locked by %s", document,
								document.getLockUser(), transaction.getUser().getFullName()));

			documentDAO.initialize(document);
			document.setLockUserId(transaction.getUser().getId());
			document.setLockUser(transaction.getUser().getFullName());
			document.setStatus(status);
			document.setFolder(document.getFolder());

			if (transaction.getEvent() == null)
				transaction.setEvent(DocumentEvent.LOCKED.toString());

			// Modify document history entry
			documentDAO.store(document, transaction);
		}

		log.debug("locked document {}", docId);
	}

	private void storeFile(Document doc, File file) throws IOException {
		String resource = storer.getResourceName(doc, null, null);
		storer.store(file, doc.getId(), resource);
	}

	/**
	 * Utility method for document removal from index
	 * 
	 * @param doc the document to delete from the index
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

	@Override
	public String parseDocument(Document doc, String fileVersion) throws ParseException {
		String content = null;

		// Check if the document is an alias
		if (doc.getDocRef() != null) {
			long docref = doc.getDocRef();
			try {
				doc = documentDAO.findById(docref);
				if (doc == null)
					throw new Exception(String.format("Unexisting referenced document {}", docref));
			} catch (Throwable e) {
				throw new RuntimeException(e.getMessage(), e);
			}
		}

		// Parses the file where it is already stored
		Locale locale = doc.getLocale();
		String resource = storer.getResourceName(doc, fileVersion, null);
		Parser parser = ParserFactory.getParser(doc.getFileName());

		log.debug("Using parser {} to parse document {}", parser.getClass().getName(), doc.getId());

		// and gets some fields
		if (parser != null) {
			TenantDAO tDao = (TenantDAO) Context.get().getBean(TenantDAO.class);
			try {
				content = parser.parse(storer.getStream(doc.getId(), resource), doc.getFileName(), null, locale,
						tDao.findById(doc.getTenantId()).getName(), doc, fileVersion);
			} catch (Throwable e) {
				log.error("Cannot parse document {}", doc, e);
				if (e instanceof ParseException)
					throw (ParseException) e;
				else
					throw new ParseException(e);
			}
		}

		if (content == null) {
			content = "";
		}

		return content;
	}

	@Override
	public long reindex(long docId, String content, DocumentHistory transaction)
			throws PersistenceException, ParseException {
		Document doc = documentDAO.findById(docId);
		assert doc != null : "Unexisting document with ID: " + docId;

		log.debug("Reindexing document {} - {}", docId, doc.getFileName());

		boolean alreadyIndexed = doc.getIndexed() == AbstractDocument.INDEX_INDEXED;

		String cont = content;
		long parsingTime = 0;
		if (doc.getDocRef() != null) {
			// We are indexing an alias, so index the real document first
			Document realDoc = documentDAO.findById(doc.getDocRef());
			if (realDoc != null) {
				if (realDoc.getIndexed() == AbstractDocument.INDEX_TO_INDEX
						|| realDoc.getIndexed() == AbstractDocument.INDEX_TO_INDEX_METADATA)
					parsingTime = reindex(realDoc.getId(), content, new DocumentHistory(transaction));

				// Take the content from the real document to avoid double
				// parsing
				if (StringUtils.isEmpty(content))
					cont = indexer.getHit(realDoc.getId()).getContent();
			} else {
				log.debug("Alias {} cannot be indexed because it references an unexisting document {}", doc,
						doc.getDocRef());
				documentDAO.initialize(doc);
				doc.setIndexed(AbstractDocument.INDEX_SKIP);
				documentDAO.store(doc);
				return 0;
			}
		}

		if (StringUtils.isEmpty(cont) && doc.getIndexed() != AbstractDocument.INDEX_TO_INDEX_METADATA) {
			// Extracts the content from the file. This may take very long
			// time.
			Date beforeParsing = new Date();
			cont = parseDocument(doc, null);
			parsingTime = TimeDiff.getTimeDifference(beforeParsing, new Date(), TimeField.MILLISECOND);
		}

		// This may take time
		try {
			indexer.addHit(doc, cont);
		} catch (Exception e) {
			throw new ParseException(e.getMessage(), e);
		}

		// For additional safety update the DB directly
		doc.setIndexed(AbstractDocument.INDEX_INDEXED);

		if (transaction != null)
			transaction.setEvent(DocumentEvent.INDEXED.toString());

		documentDAO.store(doc, transaction);

		/*
		 * If the document was already indexed, mark the aliases to be
		 * re-indexed
		 */
		if (alreadyIndexed)
			markAliasesToIndex(docId);

		return parsingTime;

	}

	private void markAliasesToIndex(long referencedDocId) throws PersistenceException {
		documentDAO.jdbcUpdate("update ld_document set ld_indexed=" + AbstractDocument.INDEX_TO_INDEX
				+ " where ld_docref=" + referencedDocId + " and not ld_id = " + referencedDocId);
	}

	@Override
	public void update(Document document, Document docVO, DocumentHistory transaction) throws PersistenceException {
		validateTransaction(transaction);
		assert (document != null);
		assert (docVO != null);
		try {
			/*
			 * Better to synchronize this block because under high
			 * multi-threading may lead to hibernate's sessions rollbacks
			 */
			synchronizedUpdate(document, docVO, transaction);
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			if (e instanceof PersistenceException)
				throw (PersistenceException) e;
			else
				throw new PersistenceException(e);
		}
	}

	private synchronized void synchronizedUpdate(Document document, Document docVO, DocumentHistory transaction)
			throws Exception, PersistenceException {
		documentDAO.initialize(document);
		if (document.getImmutable() == 0
				|| ((document.getImmutable() == 1 && transaction.getUser().isMemberOf(Group.GROUP_ADMIN)))) {
			DocumentHistory renameTransaction = checkDocumentRenamed(document, docVO, transaction);

			// Check CustomId uniqueness
			checkCustomIdUniquenesOnUpdate(document, docVO);

			// The document must be re-indexed
			document.setIndexed(AbstractDocument.INDEX_TO_INDEX);

			document.setWorkflowStatus(docVO.getWorkflowStatus());
			document.setColor(docVO.getColor());

			// Save retention policies
			document.setPublished(docVO.getPublished());
			document.setStartPublishing(docVO.getStartPublishing());
			document.setStopPublishing(docVO.getStopPublishing());

			// Intercept locale changes
			if (!document.getLocale().equals(docVO.getLocale())) {
				indexer.deleteHit(document.getId());
				document.setLocale(docVO.getLocale());
			}

			setFileName(document, docVO);

			document.clearTags();
			document.setTags(docVO.getTags());

			setTemplate(document, docVO);

			if (document.getTemplate() == null) {
				document.setOcrTemplateId(null);
				document.setOcrd(0);
			}

			setOcrTemplate(document, docVO);

			setBarcodeTemplate(document, docVO);

			// create a new version

			Version version = Version.create(document, transaction.getUser(), transaction.getComment(),
					DocumentEvent.CHANGED.toString(), false);

			// Modify document history entry
			document.setVersion(version.getVersion());
			if (renameTransaction != null) {
				renameTransaction.setUser(transaction.getUser());
				documentDAO.store(document, renameTransaction);
			} else {
				documentDAO.store(document, transaction);
			}

			storeVersionAsync(version);

			markAliasesToIndex(document.getId());
		} else {
			throw new Exception(String.format("Document %s is immutable", document));
		}
	}

	private DocumentHistory checkDocumentRenamed(Document document, Document docVO, DocumentHistory transaction) {
		DocumentHistory renameTransaction = null;
		if (!document.getFileName().equals(docVO.getFileName()) && docVO.getFileName() != null) {
			renameTransaction = new DocumentHistory(transaction);
			renameTransaction.setFilenameOld(document.getFileName());
			renameTransaction.setEvent(DocumentEvent.RENAMED.toString());
		}
		return renameTransaction;
	}

	private void setFileName(Document document, Document docVO) {
		if (StringUtils.isNotEmpty(docVO.getFileName())) {
			if (!document.getFileName().equals(docVO.getFileName())) {
				document.setFileName(docVO.getFileName());
			}
		}
	}

	private void setTemplate(Document document, Document docVO) throws PersistenceException {
		Template template = docVO.getTemplate();
		if (template == null && docVO.getTemplateId() != null)
			template = templateDAO.findById(docVO.getTemplateId());

		// Change the template and attributes
		if (template != null) {
			document.setTemplate(template);
			document.setTemplateId(template.getId());
			if (docVO.getAttributes() != null) {
				document.getAttributes().clear();
				for (String attrName : docVO.getAttributes().keySet()) {
					Attribute docExtendedAttribute = docVO.getAttributes().get(attrName);
					document.getAttributes().put(attrName, docExtendedAttribute);
				}
			}
		} else {
			document.setTemplate(null);
		}
	}

	private void setBarcodeTemplate(Document document, Document docVO) {
		if ((document.getBarcodeTemplateId() == null && docVO.getBarcodeTemplateId() != null)
				|| (document.getBarcodeTemplateId() != null && docVO.getBarcodeTemplateId() == null)
				|| (document.getBarcodeTemplateId() == null && docVO.getBarcodeTemplateId() == null)
				|| !document.getBarcodeTemplateId().equals(docVO.getBarcodeTemplateId()))
			document.setBarcoded(0);
		else
			document.setBarcoded(docVO.getBarcoded());
		document.setBarcodeTemplateId(docVO.getBarcodeTemplateId());
	}

	private void setOcrTemplate(Document document, Document docVO) {
		if ((document.getOcrTemplateId() == null && docVO.getOcrTemplateId() != null)
				|| (document.getOcrTemplateId() != null && docVO.getOcrTemplateId() == null)
				|| (document.getOcrTemplateId() == null && docVO.getOcrTemplateId() == null)
				|| !document.getOcrTemplateId().equals(docVO.getOcrTemplateId()))
			document.setOcrd(0);
		else
			document.setOcrd(docVO.getOcrd());
		document.setOcrTemplateId(docVO.getOcrTemplateId());
	}

	private void checkCustomIdUniquenesOnUpdate(Document document, Document docVO) throws PersistenceException {
		if (docVO.getCustomId() != null) {
			Document test = documentDAO.findByCustomId(docVO.getCustomId(), docVO.getTenantId());
			if (test != null && test.getId() != document.getId())
				throw new PersistenceException("Duplicated CustomID");
			document.setCustomId(docVO.getCustomId());
		}
	}

	@Override
	public void moveToFolder(Document doc, Folder folder, DocumentHistory transaction) throws PersistenceException {
		validateTransaction(transaction);

		if (folder.equals(doc.getFolder()))
			return;

		if (doc.getImmutable() == 0
				|| ((doc.getImmutable() == 1 && transaction.getUser().isMemberOf(Group.GROUP_ADMIN)))) {

			/*
			 * Better to synchronize this block because under high
			 * multi-threading may lead to hibernate's sessions rollbacks
			 */
			synchronized (this) {
				documentDAO.initialize(doc);
				transaction.setPathOld(folderDAO.computePathExtended(doc.getFolder().getId()));
				transaction.setFilenameOld(doc.getFileName());
				transaction.setEvent(DocumentEvent.MOVED.toString());

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

				Version version = Version.create(doc, transaction.getUser(), transaction.getComment(),
						DocumentEvent.MOVED.toString(), false);
				version.setId(0);

				documentDAO.store(doc, transaction);

				storeVersionAsync(version);
			}
		} else {
			throw new PersistenceException(DOCUMENT_IS_IMMUTABLE);
		}
	}

	@Override
	public Document create(InputStream content, Document docVO, DocumentHistory transaction)
			throws PersistenceException {
		assert (transaction != null);
		assert (docVO != null);

		// Write content to temporary file, then delete it
		File tmp = null;
		try {
			tmp = FileUtil.createTempFile("create", "");
			if (content != null)
				FileUtil.writeFile(content, tmp.getPath());
			return create(tmp, docVO, transaction);
		} catch (PersistenceException pe) {
			throw pe;
		} catch (Exception ioe) {
			throw new PersistenceException(ioe.getMessage(), ioe);
		} finally {
			FileUtils.deleteQuietly(tmp);
		}
	}

	@Override
	public Document create(File file, Document docVO, DocumentHistory transaction) throws PersistenceException {
		assert transaction != null : "Empty transaction";
		assert docVO != null : "No value object provided";

		setAtributesForCreation(file, docVO, transaction);

		/*
		 * Better to synchronize this block because under high multi-threading
		 * may lead to hibernate's sessions rollbacks
		 */
		synchronized (this) {
			countPages(file, docVO);

			if (docVO.getTemplate() == null && docVO.getTemplateId() != null)
				docVO.setTemplate(templateDAO.findById(docVO.getTemplateId()));

			if (file != null)
				transaction.setFile(file.getAbsolutePath());

			// Create the record
			transaction.setEvent(DocumentEvent.STORED.toString());
			documentDAO.store(docVO, transaction);

			/* store the document into filesystem */
			if (file != null)
				try {
					storeFile(docVO, file);
				} catch (Throwable e) {
					String message = String.format("Unable to store the file of document %d", docVO.getId());
					log.error(message);
					documentDAO.delete(docVO.getId());
					throw new PersistenceException(message, e);
				}

			// The document record has been written, now store the initial
			// version (default 1.0)
			Version vers = Version.create(docVO, userDAO.findById(transaction.getUserId()), transaction.getComment(),
					DocumentEvent.STORED.toString(), true);

			// Wait for the document's record write
			String documentWriteCheckQuery = "select count(*) from ld_document where ld_id=" + vers.getDocId();
			int count = documentDAO.queryForInt(documentWriteCheckQuery);
			int tests = 0;
			while (count == 0 && tests < 100) {
				count = documentDAO.queryForInt(documentWriteCheckQuery);
				try {
					Thread.sleep(100L);
				} catch (InterruptedException e) {
					// Nothing to do
				}
				tests++;
			}

			if (log.isDebugEnabled() && count > 0)
				log.debug("Record of document {} has been written", docVO.getId());

			storeVersionAsync(vers);
			
			return docVO;
		}

	}

	/**
	 * Saves a version in another thread waiting for the referenced document to
	 * be available into the database.
	 * 
	 * @param version the version to save
	 */
	void storeVersionAsync(Version version) {
		/*
		 * Probably the document's record has not been written yet, we should
		 * fork a thread to wait for it's write.
		 */
		ThreadPools.get().schedule(() -> {
			try {
				// Wait for the document's record write
				String documentWriteCheckQuery = "select count(*) from ld_document where ld_id=" + version.getDocId();
				int count = 0;
				int tests = 0;
				while (count == 0 && tests < 100) {
					count = documentDAO.queryForInt(documentWriteCheckQuery);
					try {
						Thread.sleep(1000L);
					} catch (Throwable ie) {
						break;
					}
					tests++;
				}

				if (log.isDebugEnabled() && count > 0)
					log.debug("Record of document {} has been written", version.getDocId());

				versionDAO.store(version);
			} catch (PersistenceException ex) {
				log.error(ex.getMessage(), ex);
			}
		}, "VersionSave", 100L);
	}

	private void setAtributesForCreation(File file, Document docVO, DocumentHistory transaction) {
		String type = UNKNOWN;
		int lastDotIndex = docVO.getFileName().lastIndexOf(".");
		if (lastDotIndex > 0) {
			type = FileUtil.getExtension(docVO.getFileName()).toLowerCase();
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

		docVO.setStatus(AbstractDocument.DOC_UNLOCKED);
		docVO.setType(type);
		docVO.setVersion(config.getProperty("document.startversion"));
		docVO.setFileVersion(docVO.getVersion());
		docVO.setFileSize(file.length());
		docVO.setId(0L);
	}

	/**
	 * Processes a file trying to calculate the pages and updates the pages
	 * property of the given document.
	 * 
	 * @param file The document's file
	 * @param doc The document
	 */
	private void countPages(File file, Document doc) {
		try {
			Parser parser = ParserFactory.getParser(doc.getFileName());
			log.debug("Using parser {} to count pages of document {}", parser.getClass().getName(), doc);
			if (parser != null)
				doc.setPages(parser.countPages(file, doc.getFileName()));
		} catch (Throwable e) {
			log.warn("Cannot count pages of document {}", doc, e);
		}
	}

	@Override
	public int countPages(Document doc) {
		try {
			Parser parser = ParserFactory.getParser(doc.getFileName());
			Storer storer = (Storer) Context.get().getBean(Storer.class);
			return parser.countPages(storer.getStream(doc.getId(), storer.getResourceName(doc, null, null)),
					doc.getFileName());
		} catch (Throwable e) {
			log.warn("Cannot count pages of document {}", doc, e);
			return 1;
		}
	}

	public Document copyToFolder(Document doc, Folder folder, DocumentHistory transaction)
			throws PersistenceException, IOException {
		validateTransaction(transaction);

		// initialize the document
		documentDAO.initialize(doc);

		if (doc.getDocRef() != null) {
			return createAlias(doc, folder, doc.getDocRefType(), transaction);
		}

		String resource = storer.getResourceName(doc, null, null);
		InputStream is = storer.getStream(doc.getId(), resource);
		try {
			Document cloned = new Document(doc);
			cloned.setId(0);
			if (doc.getFolder().getId() != folder.getId())
				cloned.setFolder(folder);
			cloned.setLastModified(null);
			cloned.setDate(null);
			if (cloned.getIndexed() == AbstractDocument.INDEX_INDEXED)
				cloned.setIndexed(AbstractDocument.INDEX_TO_INDEX);
			cloned.setStamped(0);
			cloned.setSigned(0);
			cloned.setLinks(0);
			cloned.setOcrd(0);
			cloned.setBarcoded(0);
			Document createdDocument = create(is, cloned, transaction);

			// Save the event of the copy
			DocumentHistory copyEvent = new DocumentHistory(transaction);
			copyEvent.setDocument(doc);
			copyEvent.setFolder(doc.getFolder());
			copyEvent.setEvent(DocumentEvent.COPYED.toString());

			String newPath = folderDAO.computePathExtended(folder.getId());
			copyEvent.setComment(newPath + "/" + createdDocument.getFileName());
			documentDAO.saveDocumentHistory(doc, copyEvent);

			return createdDocument;
		} finally {
			if (is != null)
				is.close();
			is = null;
		}
	}

	@Override
	public void unlock(long docId, DocumentHistory transaction) throws PersistenceException {
		validateTransaction(transaction);

		/*
		 * Better to synchronize this block because under high multi-threading
		 * may lead to hibernate's sessions rollbacks
		 */
		synchronized (this) {
			Document document = documentDAO.findDocument(docId);
			documentDAO.initialize(document);

			if (transaction.getUser().isMemberOf(Group.GROUP_ADMIN)) {
				document.setImmutable(0);
			} else if (document.getLockUserId() == null || document.getStatus() == AbstractDocument.DOC_UNLOCKED) {
				log.debug("The document {} is already unlocked", document);
				return;
			} else if (!transaction.getUserId().toString().equals(document.getLockUserId().toString())) {
				/*
				 * We compare the string representations because found that
				 * sometimes the comparison between longs fails
				 */
				String message = String.format("The document %s is locked by %s and cannot be unlocked by %s", document,
						document.getLockUser(), transaction.getUser().getFullName());
				throw new PersistenceException(message);
			}

			document.setLockUserId(null);
			document.setLockUser(null);
			document.setExtResId(null);
			document.setStatus(AbstractDocument.DOC_UNLOCKED);

			// Modify document history entry
			transaction.setEvent(DocumentEvent.UNLOCKED.toString());
			documentDAO.store(document, transaction);
		}
		log.debug("Unlocked document {}", docId);
	}

	@Override
	public void makeImmutable(long docId, DocumentHistory transaction) throws PersistenceException {
		validateTransaction(transaction);

		Document document = documentDAO.findById(docId);
		if (document.getImmutable() == 0) {
			// Modify document history entry
			transaction.setEvent(DocumentEvent.IMMUTABLE.toString());
			documentDAO.makeImmutable(docId, transaction);

			log.debug("The document {} has been marked as immutable", docId);
		} else {
			throw new PersistenceException(DOCUMENT_IS_IMMUTABLE);
		}
	}

	@Override
	public void rename(long docId, String newName, DocumentHistory transaction) throws PersistenceException {
		validateTransaction(transaction);

		/*
		 * Better to synchronize this block because under high multi-threading
		 * may lead to hibernate's sessions rollbacks
		 */
		synchronized (this) {
			Document document = documentDAO.findById(docId);

			if (document.getImmutable() == 0
					|| ((document.getImmutable() == 1 && transaction.getUser().isMemberOf(Group.GROUP_ADMIN)))) {
				documentDAO.initialize(document);
				document.setFileName(newName.trim());
				String extension = FileUtil.getExtension(newName.trim());
				if (StringUtils.isNotEmpty(extension)) {
					document.setType(FileUtil.getExtension(newName));
				} else {
					document.setType(UNKNOWN);
				}

				document.setIndexed(AbstractDocument.INDEX_TO_INDEX);

				Version version = Version.create(document, transaction.getUser(), transaction.getComment(),
						DocumentEvent.RENAMED.toString(), false);
				storeVersionAsync(version);

				transaction.setEvent(DocumentEvent.RENAMED.toString());
				documentDAO.store(document, transaction);

				markAliasesToIndex(docId);
				log.debug("Document renamed: {}", document.getId());
			} else {
				throw new PersistenceException(DOCUMENT_IS_IMMUTABLE);
			}
		}
	}

	@Override
	public Document replaceAlias(long aliasId, DocumentHistory transaction) throws PersistenceException {
		validateTransaction(transaction);

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

			return copyToFolder(originalDoc, folder, new DocumentHistory(transaction));
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			throw new PersistenceException(e);
		}
	}

	@Override
	public Document createAlias(Document doc, Folder folder, String aliasType, DocumentHistory transaction)
			throws PersistenceException {
		assert (doc != null);
		assert (folder != null);
		validateTransaction(transaction);

		try {
			// initialize the document
			documentDAO.initialize(doc);

			Document alias = new Document();
			alias.setFolder(folder);
			alias.setFileName(doc.getFileName());
			alias.setFileSize(doc.getFileSize());
			alias.setDate(new Date());
			alias.setVersion(doc.getVersion());
			alias.setFileVersion(doc.getFileVersion());

			String type = UNKNOWN;
			int lastDotIndex = doc.getFileName().lastIndexOf(".");
			if (lastDotIndex > 0)
				type = FileUtil.getExtension(doc.getFileName());

			if (StringUtils.isNotEmpty(aliasType)) {
				alias.setFileName(
						FileUtil.getBaseName(doc.getFileName()) + "." + FileUtil.getExtension(aliasType).toLowerCase());
				type = FileUtil.getExtension(aliasType).toLowerCase();
			}

			alias.setPublisher(transaction.getUsername());
			alias.setPublisherId(transaction.getUserId());
			alias.setCreator(transaction.getUsername());
			alias.setCreatorId(transaction.getUserId());
			alias.setStatus(AbstractDocument.DOC_UNLOCKED);
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
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			throw new PersistenceException(e);
		}
	}

	@Override
	public void changeIndexingStatus(Document doc, int status) {
		assert (status != AbstractDocument.INDEX_INDEXED);

		if (status == AbstractDocument.INDEX_SKIP && doc.getIndexed() == AbstractDocument.INDEX_SKIP)
			return;
		if (status == AbstractDocument.INDEX_TO_INDEX && doc.getIndexed() == AbstractDocument.INDEX_TO_INDEX)
			return;
		if (status == AbstractDocument.INDEX_TO_INDEX_METADATA
				&& doc.getIndexed() == AbstractDocument.INDEX_TO_INDEX_METADATA)
			return;

		documentDAO.initialize(doc);
		if (doc.getIndexed() == AbstractDocument.INDEX_INDEXED)
			deleteFromIndex(doc);
		doc.setIndexed(status);
		try {
			documentDAO.store(doc);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}

	public void setUserDAO(UserDAO userDAO) {
		this.userDAO = userDAO;
	}

	public void setFolderDAO(FolderDAO folderDAO) {
		this.folderDAO = folderDAO;
	}

	@Override
	public Version deleteVersion(long versionId, DocumentHistory transaction) throws PersistenceException {
		Version versionToDelete = versionDAO.findById(versionId);
		assert versionToDelete != null : "Unexisting version " + versionId;

		String versionToDeleteSpec = versionToDelete.getVersion();

		Document document = documentDAO.findById(versionToDelete.getDocId());
		assert document != null : "Unexisting referenced document " + versionToDelete.getDocId();

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
					log.warn("Unable to delete resource {} of document {}", resource, versionToDelete.getDocId());
				}
		}

		try {
			versionDAO.delete(versionId);
		} catch (PersistenceException e) {
			throw new PersistenceException("Version not deleted from the database", e);
		}

		// Save the version deletion history
		DocumentHistory delHistory = null;
		if (transaction != null) {
			delHistory = new DocumentHistory(transaction);
			delHistory.setEvent(DocumentEvent.VERSION_DELETED.toString());
			delHistory.setComment(versionToDeleteSpec + " - " + versionToDelete.getFileVersion());
		}
		documentDAO.saveDocumentHistory(document, delHistory);

		versions = versionDAO.findByDocId(versionToDelete.getDocId());

		Version lastVersion = getLastVersion(versions, versionToDelete);

		/*
		 * Downgrade the document version in case the deleted version is the
		 * current one
		 */
		downgradeDocumentVersion(document, versionToDeleteSpec, transaction, lastVersion);

		return lastVersion;
	}

	private Version getLastVersion(List<Version> versions, Version versionToDelete) {
		Version lastVersion = null;
		for (Version version : versions) {
			if (version.getDeleted() == 0 && version.getId() != versionToDelete.getId()) {
				lastVersion = version;
				break;
			}
		}
		return lastVersion;
	}

	private void downgradeDocumentVersion(Document document, String versionToDeleteSpec, DocumentHistory transaction,
			Version lastVersion) throws PersistenceException {
		String currentVersion = document.getVersion();
		if (currentVersion.equals(versionToDeleteSpec) && lastVersion != null) {
			documentDAO.initialize(document);
			document.setVersion(lastVersion.getVersion());
			document.setFileVersion(lastVersion.getFileVersion());

			if (transaction != null) {
				transaction.setEvent(DocumentEvent.CHANGED.toString());
				transaction.setComment(
						"Version changed to " + document.getVersion() + " (" + document.getFileVersion() + ")");
			}

			documentDAO.store(document, transaction);
		}
	}

	public void setDocumentNoteDAO(DocumentNoteDAO documentNoteDAO) {
		this.documentNoteDAO = documentNoteDAO;
	}

	@Override
	public long archiveFolder(long folderId, DocumentHistory transaction) throws PersistenceException {
		List<Long> docIds = new ArrayList<Long>();
		Folder root = folderDAO.findFolder(folderId);

		Collection<Long> folderIds = folderDAO.findFolderIdByUserIdAndPermission(transaction.getUserId(),
				Permission.ARCHIVE, root.getId(), true);
		for (Long fid : folderIds) {
			String where = " where ld_deleted=0 and not ld_status=" + AbstractDocument.DOC_ARCHIVED
					+ " and ld_folderid=" + fid;
			@SuppressWarnings("unchecked")
			List<Long> ids = (List<Long>) documentDAO.queryForList("select ld_id from ld_document " + where,
					Long.class);
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
	public void archiveDocuments(long[] docIds, DocumentHistory transaction) throws PersistenceException {
		assert (transaction.getUser() != null);
		List<Long> idsList = new ArrayList<Long>();
		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Collection<Long> folderIds = folderDAO.findFolderIdByUserIdAndPermission(transaction.getUserId(),
				Permission.ARCHIVE, null, true);

		for (long id : docIds) {
			Document doc = dao.findById(id);

			// Skip documents in folders without Archive permission
			if (!(transaction.getUser().isMemberOf(Group.GROUP_ADMIN)
					|| transaction.getUser().getUsername().equals("_retention"))
					&& !folderIds.contains(doc.getFolder().getId()))
				continue;

			// Create the document history event
			DocumentHistory t = new DocumentHistory(transaction);
			dao.archive(id, t);
			idsList.add(id);
		}

		// Remove all corresponding hits from the index
		SearchEngine engine = (SearchEngine) Context.get().getBean(SearchEngine.class);
		engine.deleteHits(idsList);

		log.info("Archived documents {}", idsList);
	}

	@Override
	public Ticket createDownloadTicket(long docId, String suffix, Integer expireHours, Date expireDate,
			Integer maxDownloads, String urlPrefix, DocumentHistory transaction)
			throws PersistenceException, PermissionException {
		validateTransaction(transaction);

		Document document = documentDAO.findById(docId);
		if (document == null)
			throw new PersistenceException("Unexisting document " + docId);

		if (!folderDAO.isDownloadEnabled(document.getFolder().getId(), transaction.getUserId()))
			throw new PermissionException(transaction.getUsername(), "Folder " + document.getFolder().getId(),
					Permission.DOWNLOAD);

		Ticket ticket = prepareTicket(docId, transaction.getUser());
		ticket.setSuffix(suffix);
		ticket.setEnabled(1);
		ticket.setMaxCount(maxDownloads);

		Calendar cal = Calendar.getInstance();
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
		Ticket ticket = new Ticket();
		ticket.setDocId(docId);
		ticket.setUserId(user.getId());
		return ticket;
	}

	private String composeTicketUrl(Ticket ticket, String urlPrefix) {
		if (StringUtils.isEmpty(urlPrefix))
			urlPrefix = config.getProperty("server.url");
		if (!urlPrefix.endsWith("/"))
			urlPrefix += "/";
		String address = urlPrefix + "download-ticket?ticketId=" + ticket.getTicketId();
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
		try {
			Document doc = documentDAO.findDocument(docId);
			if (!doc.isPasswordProtected())
				return true;

			boolean granted = doc.isGranted(password);
			if (granted)
				session.getUnprotectedDocs().put(docId, password);
			return granted;
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return false;
		}
	}

	@Override
	public void promoteVersion(long docId, String version, DocumentHistory transaction)
			throws PersistenceException, IOException {
		validateTransaction(transaction);

		transaction.setComment(String.format("promoted version %s", version));

		// identify the document and folder
		Document document = documentDAO.findDocument(docId);
		if (document.getImmutable() == 0 && document.getStatus() == AbstractDocument.DOC_UNLOCKED) {
			Version ver = versionDAO.findByVersion(document.getId(), version);
			if (ver == null)
				throw new PersistenceException(String.format("Unexisting version %s of document %d", version, docId));
			versionDAO.initialize(ver);

			transaction.setEvent(DocumentEvent.CHECKEDOUT.toString());
			checkout(document.getId(), transaction);

			// Write the version file into a temporary file
			File tmp = FileUtil.createTempFile("promotion", "");
			try {
				Folder originalFolder = document.getFolder();
				Version docVO = new Version(ver);
				docVO.setFolder(originalFolder);
				docVO.setCustomId(ver.getCustomId());
				docVO.setId(0L);

				if (ver.getTemplateId() != null)
					docVO.setTemplate(templateDAO.findById(ver.getTemplateId()));

				if (StringUtils.isNotEmpty(ver.getTgs())) {
					Set<String> tags = Arrays.asList(ver.getTgs().split(",")).stream().collect(Collectors.toSet());
					docVO.setTagsFromWords(tags);
				}

				storer.writeToFile(document.getId(), storer.getResourceName(document, ver.getFileVersion(), null), tmp);
				DocumentHistory checkinTransaction = new DocumentHistory(transaction);
				checkinTransaction.setDate(new Date());
				checkin(document.getId(), tmp, ver.getFileName(), false, docVO, checkinTransaction);

				log.debug("Promoted version {} of document {}", version, docId);
			} finally {
				FileUtils.deleteQuietly(tmp);
			}
		}
	}

	@Override
	public int enforceFilesIntoFolderStorage(long rootFolderId, DocumentHistory transaction)
			throws PersistenceException, IOException {
		Folder rootFolder = folderDAO.findFolder(rootFolderId);
		if (rootFolder == null)
			throw new PersistenceException("Unexisting folder ID  " + rootFolderId);

		if (transaction != null)
			transaction.setEvent(DocumentEvent.CHANGED.toString());

		int totalMovedFiles = 0;

		// Traverse the tree
		Collection<Long> folderIds = folderDAO.findFolderIdInTree(rootFolderId, false);
		for (Long folderId : folderIds) {
			Folder folder = folderDAO.findById(folderId);
			if (folder == null || folder.getFoldRef() != null)
				continue;

			folderDAO.initialize(folder);

			// Retrieve the storage specification from the current folder
			int targetStorage = getStorage(folder);

			log.info("Move the files of all the documents inside the folder {} into the target storage {}", rootFolder,
					targetStorage);

			List<Document> documents = documentDAO.findByFolder(folderId, null);
			for (Document document : documents) {
				int movedFiles = storer.moveResourcesToStore(document.getId(), targetStorage);
				if (movedFiles > 0) {
					totalMovedFiles += movedFiles;
					try {
						DocumentHistory storedTransaction = new DocumentHistory(transaction);
						storedTransaction
								.setComment(String.format("%d files moved to storage %d", movedFiles, targetStorage));
						documentDAO.saveDocumentHistory(document, transaction);
					} catch (Throwable t) {
						log.warn("Cannot record history for document {}", document, t);
					}
				}
			}
		}

		return totalMovedFiles;
	}

	private int getStorage(Folder folder) {
		int targetStorage = config.getInt("store.write", 1);
		if (folder.getStorage() != null)
			targetStorage = folder.getStorage().intValue();
		else {
			try {
				// Check if one of the parent folders references the storer
				List<Folder> parents = folderDAO.findParents(folder.getId());
				Collections.reverse(parents);

				for (Folder parentFolder : parents) {
					folderDAO.initialize(parentFolder);
					if (parentFolder.getStorage() != null) {
						targetStorage = parentFolder.getStorage().intValue();
						break;
					}
				}
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
		}
		return targetStorage;
	}

	@Override
	public Document merge(Collection<Document> documents, long targetFolderId, String fileName,
			DocumentHistory transaction) throws IOException, PersistenceException {
		List<Long> docIds = documents.stream().map(d -> d.getId()).collect(Collectors.toList());
		File tempDir = null;
		File bigPdf = null;
		try {
			tempDir = preparePdfs(transaction != null ? transaction.getUser() : null, docIds);

			// Now collect and sort each PDF
			File[] pdfs = tempDir.listFiles();

			Arrays.sort(pdfs, new Comparator<File>() {
				@Override
				public int compare(File o1, File o2) {
					return o1.getName().compareTo(o2.getName());
				}
			});

			// Merge all the PDFs
			bigPdf = mergePdf(pdfs);

			// Add an history entry to track the export of the document
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			if (transaction != null)
				for (Long id : docIds) {
					DocumentHistory trans = new DocumentHistory(transaction);
					trans.setEvent(DocumentEvent.EXPORTPDF.toString());
					docDao.saveDocumentHistory(docDao.findById(id), trans);
				}

			Document docVO = new Document();
			docVO.setFileName(fileName.toLowerCase().endsWith(".pdf") ? fileName : fileName + ".pdf");
			FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			docVO.setFolder(folderDao.findById(targetFolderId));

			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			return manager.create(bigPdf, docVO, transaction);
		} finally {
			FileUtil.strongDelete(bigPdf);
			FileUtil.strongDelete(tempDir);
		}
	}

	/**
	 * Convert a selection of documents into PDF and stores them in a temporary
	 * folder
	 * 
	 * @param user The current user
	 * @param docIds List of documents to be converted
	 * 
	 * @return The temporary folder
	 * 
	 * @throws IOException
	 */
	private File preparePdfs(User user, List<Long> docIds) throws IOException {
		Path tempPath = Files.createTempDirectory(MERGE);
		File tempDir = tempPath.toFile();

		DecimalFormat nf = new DecimalFormat("00000000");
		int i = 0;
		for (long docId : docIds) {
			try {
				i++;
				DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
				Document document = docDao.findDocument(docId);

				if (document != null && user != null && !user.isMemberOf(Group.GROUP_ADMIN)
						&& !user.isMemberOf("publisher") && !document.isPublishing())
					continue;

				FormatConverterManager manager = (FormatConverterManager) Context.get()
						.getBean(FormatConverterManager.class);
				manager.convertToPdf(document, null);

				File pdf = new File(tempDir, nf.format(i) + ".pdf");

				manager.writePdfToFile(document, null, pdf, null);
			} catch (Throwable t) {
				log.error(t.getMessage(), t);
			}
		}
		return tempDir;
	}

	/**
	 * Merges different PDFs into a single PDF-
	 * 
	 * @param pdfs ordered array of pdf files to be merged
	 * @return The merged Pdf file
	 * 
	 * @throws IOException
	 */
	private File mergePdf(File[] pdfs) throws IOException {
		File tempDir = null;
		try {
			Path tempPath = Files.createTempDirectory(MERGE);
			tempDir = tempPath.toFile();

			File dst = FileUtil.createTempFile(MERGE, ".pdf");

			PDFMergerUtility merger = new PDFMergerUtility();
			for (File file : pdfs) {
				merger.addSource(file);
			}

			merger.setDestinationFileName(dst.getAbsolutePath());
			MemoryUsageSetting memoryUsage = MemoryUsageSetting.setupTempFileOnly();
			memoryUsage.setTempDir(tempDir);
			merger.mergeDocuments(memoryUsage);

			return dst;
		} finally {
			if (tempDir != null && tempDir.exists())
				FileUtil.strongDelete(tempDir);
		}
	}
}