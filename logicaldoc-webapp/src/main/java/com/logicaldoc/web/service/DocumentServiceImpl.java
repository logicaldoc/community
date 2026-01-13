package com.logicaldoc.web.service;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.security.NoSuchAlgorithmException;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.bouncycastle.cms.CMSException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.automation.Automation;
import com.logicaldoc.core.automation.AutomationException;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailAttachment;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.communication.MailUtil;
import com.logicaldoc.core.communication.Message;
import com.logicaldoc.core.communication.MessageTemplate;
import com.logicaldoc.core.communication.MessageTemplateDAO;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.communication.SystemMessage;
import com.logicaldoc.core.communication.SystemMessageDAO;
import com.logicaldoc.core.contact.Contact;
import com.logicaldoc.core.contact.ContactDAO;
import com.logicaldoc.core.conversion.FormatConversionManager;
import com.logicaldoc.core.document.Bookmark;
import com.logicaldoc.core.document.BookmarkDAO;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentAccessControlEntry;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentHistoryDAO;
import com.logicaldoc.core.document.DocumentLink;
import com.logicaldoc.core.document.DocumentLinkDAO;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.document.DocumentNote;
import com.logicaldoc.core.document.DocumentNoteDAO;
import com.logicaldoc.core.document.DocumentStatus;
import com.logicaldoc.core.document.IndexingStatus;
import com.logicaldoc.core.document.NoteAccessControlEntry;
import com.logicaldoc.core.document.Rating;
import com.logicaldoc.core.document.RatingDAO;
import com.logicaldoc.core.document.Version;
import com.logicaldoc.core.document.VersionDAO;
import com.logicaldoc.core.document.thumbnail.ThumbnailManager;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.imaging.ImageUtil;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.metadata.validation.Validator;
import com.logicaldoc.core.parser.ParsingException;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.authentication.PasswordWeakException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.store.Store;
import com.logicaldoc.core.store.StoreResource;
import com.logicaldoc.core.ticket.Ticket;
import com.logicaldoc.core.ticket.TicketDAO;
import com.logicaldoc.core.transfer.InMemoryZipImport;
import com.logicaldoc.core.transfer.ZipExport;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.gui.common.client.AccessDeniedException;
import com.logicaldoc.gui.common.client.InvalidSessionServerException;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIBookmark;
import com.logicaldoc.gui.common.client.beans.GUIContact;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.beans.GUIEmail;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIRating;
import com.logicaldoc.gui.common.client.beans.GUIVersion;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.util.MimeType;
import com.logicaldoc.util.StringUtil;
import com.logicaldoc.util.concurrent.FutureElaboration;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.html.HTMLSanitizer;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.spring.Context;
import com.logicaldoc.web.UploadServlet;
import com.logicaldoc.web.websockets.WebsocketTool;

import jakarta.mail.MessagingException;
import jakarta.servlet.http.HttpServletRequest;

/**
 * The document service for the operations on the documents done through the
 * GUI.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentServiceImpl extends AbstractRemoteService implements DocumentService {

	private static final String ERROR = "error";

	private static final String DOCUMENT_STR = "Document ";

	private static final String UNEXISTING_DOCUMENT = "Unexisting document";

	private static final String DOWNLOAD_TICKET = "downloadTicket";

	private static final String MESSAGE = "message";

	private static final String SMTP_USERASFROM = ".smtp.userasfrom";

	private static final String DOCUMENT = "document";

	private static final String OUTBOX = "outbox";

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(DocumentServiceImpl.class);

	// Useful method for mocking the email sender inside unit tests
	private static EMailSender emailSender;

	@Override
	public void addBookmarks(List<Long> ids, int type) throws ServerException {
		Session session = validateSession();

		BookmarkDAO bookmarkDao = BookmarkDAO.get();
		FolderDAO fdao = FolderDAO.get();
		DocumentDAO dao = DocumentDAO.get();
		for (Long id : ids) {
			try {
				Bookmark bookmark = null;
				if (bookmarkDao.findByUserIdAndDocId(session.getUserId(), id) != null) {
					// The bookmark already exists
				} else {
					bookmark = new Bookmark();
					bookmark.setTenantId(session.getTenantId());
					bookmark.setType(type);
					bookmark.setTargetId(id);
					bookmark.setUserId(session.getUserId());

					if (type == Bookmark.TYPE_DOCUMENT) {
						Document doc = dao.findById(id);
						if (doc == null)
							throw new ServerException("Unexisting document " + id);
						bookmark.setTitle(doc.getFileName());
						bookmark.setFileType(doc.getType());
					} else {
						Folder f = fdao.findById(id);
						bookmark.setTitle(f.getName());
					}

					bookmarkDao.store(bookmark);
				}
			} catch (PersistenceException e) {
				throwServerException(session, log, e);
			}
		}
	}

	private void index(Long[] docIds, Session session) throws PersistenceException, ParsingException {
		if (docIds == null || docIds.length < 1)
			return;
		else if (log.isInfoEnabled())
			log.info("Indexing documents {}",
					Stream.of(docIds).map(id -> Long.toString(id)).collect(Collectors.joining(", ")));

		DocumentManager documentManager = DocumentManager.get();
		for (Long id : docIds) {
			if (id != null) {
				DocumentHistory transaction = new DocumentHistory();
				transaction.setSession(session);
				documentManager.index(id, null, transaction);
			}
		}
	}

	@Override
	public void indexDocuments(List<Long> docIds) throws ServerException {
		Session session = validateSession();
		executeLongRunningOperation("Index Documents", () -> {
			try {
				index(docIds.toArray(new Long[0]), session);
			} catch (ParsingException | PersistenceException e) {
				log.error(e.getMessage(), e);
			}
			return null;
		}, session);
	}

	private void indexAddedDocs(List<Long> docIdsToIndex, final Session session)
			throws PersistenceException, ParsingException {
		if (!docIdsToIndex.isEmpty())
			index(docIdsToIndex.toArray(new Long[0]), session);
	}

	@Override
	public void destroyDocuments(List<Long> docIds) throws ServerException {
		final Session session = validateSession();
		checkMenu(Menu.DESTROY_DOCUMENTS);

		log.info("User {} requested the permanent deletion of documents {}", session.getUsername(), docIds);

		DocumentManager manager = DocumentManager.get();

		executeLongRunningOperation("Destroy Documents", () -> {
			try {
				for (Long docId : docIds) {
					FolderHistory transaction = new FolderHistory();
					transaction.setSession(session);
					manager.destroyDocument(docId, transaction);
				}
			} catch (PersistenceException | PermissionException e) {
				log.error(e.getMessage(), e);
			}
			return null;
		}, session);
	}

	@Override
	public List<GUIDocument> addDocuments(boolean importZip, String charset, boolean immediateIndexing,
			final GUIDocument metadata) throws ServerException {
		final Session session = validateSession();

		Map<String, File> uploadedFilesMap = getUploadedFiles(session.getSid());

		List<GUIDocument> createdDocs = new ArrayList<>();

		if (log.isDebugEnabled())
			log.debug("Uploading {} files", uploadedFilesMap.size());
		if (uploadedFilesMap.isEmpty())
			throw new ServerException("No file uploaded");

		if (executeLongRunningOperation("Add Documents", () -> {
			try {
				addDocuments(importZip, charset, immediateIndexing, metadata, session, createdDocs);
			} catch (ServerException | PersistenceException | ParsingException | IOException e) {
				log.error(e.getMessage(), e);
				new WebsocketTool().showMessage(session, e.getMessage(), ERROR);
			}
			return null;
		}, session)) {
			return createdDocs;
		} else {
			return new ArrayList<>();
		}
	}

	private void addDocuments(boolean importZip, String charset, boolean immediateIndexing, final GUIDocument metadata,
			final Session session, List<GUIDocument> createdDocs) throws PersistenceException, ServerException,
			ParsingException, IOException, InterruptedException, ExecutionException {

		checkWritePermission(metadata, session);

		Map<String, File> uploadedFilesMap = getUploadedFiles(session.getSid());

		List<Document> docs = new ArrayList<>();
		DocumentManager documentManager = DocumentManager.get();

		FolderDAO folderDao = FolderDAO.get();
		Folder parent;
		parent = folderDao.findFolder(metadata.getFolder().getId());

		List<Long> docIdsToIndex = new ArrayList<>();
		for (Entry<String, File> entry : uploadedFilesMap.entrySet()) {
			String filename = entry.getKey();
			File file = entry.getValue();
			try {
				if (filename.toLowerCase().endsWith(".zip") && importZip) {
					// Make a copy of the zip file in order to avoid it's
					// deletion during import
					File tempZip = FileUtil.createTempFile("upload-", ".zip");
					FileUtils.copyFile(file, tempZip);

					// Prepare the import thread
					Thread zipImporter = new Thread(() -> importZip(charset, metadata, session, parent, tempZip));

					// And launch it
					zipImporter.start();
				} else {
					// Create the document history event
					DocumentHistory transaction = new DocumentHistory();
					transaction.setSession(session);
					transaction.setEvent(DocumentEvent.STORED);
					transaction.setComment(HTMLSanitizer.sanitizeSimpleText(metadata.getComment()));

					/*
					 * Prepare the Master document used to create the new one
					 */
					Document doc = toDocument(metadata);
					doc.setTenantId(session.getTenantId());
					doc.setCreation(new Date());
					doc.setFileName(filename);

					// Create the new document
					FutureElaboration<Document, Document> elaboration = documentManager.create(file, doc, transaction);
					doc = elaboration.get();

					if (immediateIndexing && doc.getIndexed() == IndexingStatus.TO_INDEX)
						docIdsToIndex.add(doc.getId());

					createdDocs.add(fromDocument(doc, metadata.getFolder(), null));
					docs.add(doc);
				}
			} finally {
				FileUtil.delete(file);
			}
		}

		cleanUploadedFiles(session);

		indexAddedDocs(docIdsToIndex, session);

		/*
		 * We have to notify the specified users in a separate thread
		 */
		notifyUsersInNewThread(docs, metadata, "newdoc", session);
	}

	private void checkWritePermission(final GUIDocument metadata, final Session session)
			throws PersistenceException, ServerException {
		FolderDAO fdao = FolderDAO.get();
		if (!fdao.isWriteAllowed(metadata.getFolder().getId(), session.getUserId()))
			throw new ServerException("The user doesn't have the write permission on the current folder");
	}

	private void cleanUploadedFiles(Session session) {
		Map<String, File> uploadedFilesMap = getUploadedFiles(session.getSid());
		for (File uploadedEntry : uploadedFilesMap.values())
			FileUtil.delete(uploadedEntry);
	}

	private void importZip(String charset, final GUIDocument metadata, final Session session, Folder parent,
			final File zipFile) {
		/*
		 * Prepare the Master document used to create the new one
		 */
		try {
			log.debug("zip file = {}", zipFile);

			Document doc = toDocument(metadata);
			doc.setTenantId(session.getTenantId());
			doc.setCreation(new Date());

			InMemoryZipImport importer = new InMemoryZipImport(doc, charset);
			importer.process(zipFile, parent, session.getUserId(), session.getSid());
		} catch (PersistenceException e) {
			log.error("Unable to delete temporary file", e);
		} finally {
			FileUtil.delete(zipFile);
		}
	}

	private void notifyUsersInNewThread(List<Document> docs, final GUIDocument metadata, final String templateName,
			final Session session) {
		if (!metadata.getNotifyUsers().isEmpty()) {
			new Thread(() -> notifyDocuments(docs, templateName, metadata.getNotifyMessage(), metadata.getNotifyUsers(),
					session)).start();
		}
	}

	@Override
	public GUIDocument checkin(GUIDocument document, boolean major) throws ServerException {
		Session session = validateSession();

		Map<String, File> uploadedFilesMap = getUploadedFiles(session.getSid());
		File file = uploadedFilesMap.values().iterator().next();
		String fileName = uploadedFilesMap.keySet().iterator().next();

		if (file == null)
			return null;

		if (log.isDebugEnabled())
			log.debug("Checking in file {}", fileName);

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSession(session);
		transaction.setEvent(DocumentEvent.CHECKEDIN);
		transaction.setComment(HTMLSanitizer.sanitizeSimpleText(document.getComment()));

		Document doc;
		try {
			doc = retrieveDocument(document.getId());
		} catch (PersistenceException e1) {
			return throwServerException(session, log, e1);
		}

		// checkin the document; throws an exception if
		// something goes wrong
		try (FileInputStream fis = new FileInputStream(file)) {
			DocumentManager.get().checkin(doc.getId(), fis, fileName, major, toDocument(document), transaction);
		} catch (IOException | PersistenceException e) {
			return throwServerException(session, log, e);
		}

		UploadServlet.cleanUploads(session.getSid());
		GUIDocument checkedInDocument = getById(doc.getId());

		/*
		 * We have to notify the specified users in a separate thread
		 */
		notifyUsersInNewThread(Arrays.asList(doc), document, "checkin", session);

		return checkedInDocument;
	}

	private void notifyDocuments(List<Document> docs, String messageTemplate, String notificationMessage,
			List<Long> recipientIds, Session session) {

		// Prepare the tile of the first uploaded file, by default we use a
		// single transparent pixel
		String tile = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==";
		try {
			tile = prepareTileAsString(session, docs.get(0));
		} catch (Exception e) {
			log.warn("Cannot prepare the tile image", e);
		}

		try {
			SystemMessageDAO systemMessageDao = SystemMessageDAO.get();

			Map<Locale, Set<Recipient>> emailRecipientsMap = new HashMap<>();
			Map<Locale, Set<Recipient>> systemRecipientsMap = new HashMap<>();
			prepareRecipients(recipientIds, emailRecipientsMap, systemRecipientsMap);

			for (Entry<Locale, Set<Recipient>> entry : emailRecipientsMap.entrySet()) {
				Locale locale = entry.getKey();
				Set<Recipient> recipients = entry.getValue();

				EMail mail = new EMail();
				mail.setHtml(true);
				mail.setTenantId(session.getTenantId());
				mail.setAccountId(-1);
				mail.setAuthor(session.getUser().getUsername());

				ContextProperties config = Context.get().getProperties();
				if (config.getBoolean(session.getTenantName() + SMTP_USERASFROM, true))
					mail.setAuthorAddress(session.getUser().getEmail());

				mail.setFolder(OUTBOX);
				mail.setSentDate(new Date());
				mail.setUsername(session.getUsername());
				mail.setRecipients(recipients);

				MessageTemplateDAO tDao = MessageTemplateDAO.get();
				MessageTemplate template = tDao.findByNameAndLanguage(messageTemplate, locale.toString(),
						mail.getTenantId());

				Map<String, Object> dictionary = new HashMap<>();
				dictionary.put("user", session.getUser());
				dictionary.put("creator", session.getUser());
				dictionary.put("documents", docs);
				dictionary.put(DOCUMENT, docs.get(0));
				dictionary.put(MESSAGE, notificationMessage);
				dictionary.put(Automation.LOCALE, locale);
				dictionary.put("tile", tile);

				mail.setSubject(template.getFormattedSubject(dictionary));
				mail.setMessageText("<html><body>" + template.getFormattedBody(dictionary) + "</html></body>");

				if (mail != null && !mail.getRecipients().isEmpty())
					log.info("Notify the new documents {} to {}", docs, mail.getRecipients());

				getEmailSender(session).send(mail);

				/*
				 * Save also as system message
				 */
				SystemMessage sys = new SystemMessage();
				sys.setType(Message.TYPE_SYSTEM);
				sys.setAuthor(mail.getAuthor());
				sys.setSentDate(new Date());
				sys.setMessageText(mail.getMessageText());
				sys.setSubject(mail.getSubject());
				sys.setRecipients(systemRecipientsMap.get(locale));
				sys.setHtml(true);
				sys.setTenantId(mail.getTenantId());

				systemMessageDao.store(sys);
			}
		} catch (PersistenceException | MessagingException | AutomationException e) {
			log.warn(e.getMessage(), e);
		}
	}

	private Document retrieveDocument(long docId) throws PersistenceException {
		DocumentDAO dao = DocumentDAO.get();
		return dao.findDocument(docId);
	}

	private void prepareRecipients(List<Long> notifyUserids, Map<Locale, Set<Recipient>> emailRecipientsMap,
			Map<Locale, Set<Recipient>> systemRecipientsMap) throws PersistenceException {
		String idsString = notifyUserids.stream().map(id -> Long.toString(id)).collect(Collectors.joining(","));
		UserDAO uDao = UserDAO.get();
		List<User> users = uDao.findByWhere("_entity.id in (" + idsString + ")", null, null);

		for (User user : users) {
			if (user.getEmail() != null && StringUtils.isNotEmpty(user.getEmail().trim())) {
				Recipient recipient = new Recipient();
				recipient.setName(user.getName());
				recipient.setAddress(user.getEmail());
				recipient.setType(Recipient.TYPE_EMAIL);
				recipient.setMode(Recipient.MODE_EMAIL_BCC);
				recipient.setRead(1);

				// Add the recipient to the recipients list according to the
				// user Locale
				if (emailRecipientsMap.containsKey(user.getLocale())) {
					emailRecipientsMap.get(user.getLocale()).add(recipient);
				} else {
					Set<Recipient> recipients = new HashSet<>();
					recipients.add(recipient);
					emailRecipientsMap.put(user.getLocale(), recipients);
				}
			}

			// Populate also the recipients fieldsMap for system messages
			Recipient rec = new Recipient();
			rec.setName(user.getUsername());
			rec.setAddress(user.getEmail());
			rec.setType(Recipient.TYPE_SYSTEM);
			if (systemRecipientsMap.containsKey(user.getLocale())) {
				systemRecipientsMap.get(user.getLocale()).add(rec);
			} else {
				Set<Recipient> recipients = new HashSet<>();
				recipients.add(rec);
				systemRecipientsMap.put(user.getLocale(), recipients);
			}
		}
	}

	@Override
	public List<GUIDocument> addDocuments(String language, long folderId, boolean importZip, String charset,
			boolean immediateIndexing, final Long templateId) throws ServerException {
		Session session = validateSession();
		FolderDAO fdao = FolderDAO.get();
		try {
			if (folderId == fdao.findRoot(session.getTenantId()).getId())
				throw new PermissionException("Cannot add documents in the root");

			GUIDocument metadata = new GUIDocument();
			metadata.setLanguage(language);
			metadata.setFolder(new GUIFolder(folderId));
			metadata.setTemplateId(templateId);
			return addDocuments(importZip, charset, immediateIndexing, metadata);
		} catch (PermissionException | PersistenceException | ServerException e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public GUIDocument promoteVersion(long docId, String version) throws ServerException {
		Session session = validateSession();
		log.debug("Promoting version {} of document {}", version, docId);

		try {
			Document doc = retrieveDocument(docId);
			if (doc == null)
				throw new ServerException(UNEXISTING_DOCUMENT);

			FolderDAO fDao = FolderDAO.get();
			if (!fDao.isWriteAllowed(doc.getFolder().getId(), session.getUserId()))
				throw new PermissionException(session.getUsername(), DOCUMENT_STR + docId, Permission.WRITE);

			if (doc.getStatus() != DocumentStatus.UNLOCKED)
				throw new PermissionException("The document " + docId + " is locked");

			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			transaction.setDocument(doc);

			DocumentManager.get().promoteVersion(doc.getId(), version, transaction);

			return getById(doc.getId());
		} catch (PersistenceException | ServerException | IOException | PermissionException e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public void checkout(List<Long> docIds) throws ServerException {
		Session session = validateSession();

		// Checkout the document; throws an exception if something
		// goes wrong
		DocumentManager documentManager = DocumentManager.get();
		DocumentDAO dao = DocumentDAO.get();

		try {
			// Create the document history event
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			transaction.setEvent(DocumentEvent.CHECKEDOUT);
			for (long id : docIds) {
				Document doc = dao.findDocument(id);
				if (doc != null)
					documentManager.checkout(doc.getId(), new DocumentHistory(transaction));
			}
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void lock(List<Long> docIds, String comment) throws ServerException {
		Session session = validateSession();

		// Unlock the document; throws an exception if something
		// goes wrong
		DocumentManager documentManager = DocumentManager.get();
		DocumentDAO dao = DocumentDAO.get();

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSession(session);
		transaction.setEvent(DocumentEvent.LOCKED);
		transaction.setComment(HTMLSanitizer.sanitizeSimpleText(comment));

		try {
			for (long id : docIds) {
				Document doc = dao.findDocument(id);
				if (doc != null)
					documentManager.lock(doc.getId(), DocumentStatus.LOCKED, new DocumentHistory(transaction));
			}
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void delete(List<Long> ids) throws ServerException {
		Session session = validateSession();

		for (long docId : ids) {
			try {
				deleteDocument(docId, session);
			} catch (PersistenceException e) {
				throwServerException(session, log, e);
			}
		}
	}

	private void deleteDocument(long docId, Session session) throws PersistenceException {
		DocumentDAO dao = DocumentDAO.get();
		Document doc = dao.findById(docId);
		if (doc == null)
			return;

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSession(session);
		transaction.setEvent(DocumentEvent.DELETED);
		transaction.setComment("");

		// If it is a shortcut, we delete only the shortcut
		if (doc.getDocRef() != null || (doc.isImmutable() && !transaction.getUser().isMemberOf(Group.GROUP_ADMIN))) {
			transaction.setEvent(DocumentEvent.SHORTCUT_DELETED);
			dao.delete(doc.getId(), transaction);
			return;
		}

		// The document of the selected documentRecord must be
		// not immutable
		if (doc.isImmutable() && !transaction.getUser().isMemberOf(Group.GROUP_ADMIN)) {
			log.debug("Document {} was not deleted because immutable", docId);
			return;
		}

		// The document must be not locked
		if (doc.getStatus() == DocumentStatus.LOCKED) {
			log.debug("Document {} was not deleted because locked", docId);
			return;
		}

		// Check if there are some shortcuts associated to the
		// deleting document. All the shortcuts must be deleted.
		for (Long shortcutId : dao.findAliasIds(doc.getId())) {
			dao.delete(shortcutId);
		}

		dao.delete(doc.getId(), transaction);
	}

	@Override
	public void deleteBookmarks(List<Long> bookmarkIds) throws ServerException {
		validateSession();
		BookmarkDAO dao = BookmarkDAO.get();
		for (long id : bookmarkIds) {
			try {
				dao.delete(id);
			} catch (Exception e) {
				throw new ServerException("Bookmarks have not been deleted", e);
			}
		}
	}

	@Override
	public void deleteLinks(List<Long> ids) throws ServerException {
		validateSession();

		DocumentLinkDAO dao = DocumentLinkDAO.get();
		for (long id : ids) {
			try {
				dao.delete(id);
			} catch (Exception e) {
				throw new ServerException("Bookmarks have not been deleted", e);
			}
		}
	}

	@Override
	public GUIDocument getById(long docId) throws ServerException {
		Session session = validateSession();

		try {
			return getDocument(session, docId);
		} catch (InvalidSessionServerException | PermissionException | PersistenceException e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public boolean isPasswordProtected(long docId) throws ServerException {
		Session session = validateSession();

		try {
			return DocumentDAO.get().queryForInt(
					"select count(ld_id) from ld_document where ld_deleted=0 and not ld_password = null and ld_id="
							+ docId) > 0;
		} catch (PersistenceException e) {
			return throwServerException(session, log, e);
		}
	}

	public GUIDocument getDocument(Session session, long docId)
			throws InvalidSessionServerException, PersistenceException, PermissionException {
		if (session != null)
			validateSession(session.getSid());

		DocumentDAO docDao = DocumentDAO.get();
		Document document = docDao.findById(docId);

		GUIDocument guiDocument = null;
		GUIFolder folder = null;

		if (document != null) {
			FolderDAO fDao = FolderDAO.get();
			fDao.initialize(document.getFolder());
			folder = new FolderServiceImpl().fromFolder(document.getFolder(), false);

			if (session != null)
				checkPublished(session.getUser(), document);

			docDao.initialize(document);

			guiDocument = fromDocument(document, folder, session != null ? session.getUser() : null);

			setAllowedPermissions(session, docId, guiDocument);

			if (session != null && folder != null) {
				Set<Permission> permissions = FolderDAO.get().getAllowedPermissions(document.getFolder().getId(),
						session.getUserId());
				List<String> permissionsList = new ArrayList<>();
				for (Permission permission : permissions)
					permissionsList.add(permission.name().toLowerCase());
				folder.setAllowedPermissions(new GUIAccessControlEntry(permissionsList.toArray(new String[0])));
			}
		}

		return guiDocument;
	}

	public GUIDocument fromDocument(Document doc, GUIFolder folder, User sessionUser) throws PersistenceException {
		boolean isFolder = doc.getType() != null && doc.getType().startsWith("folder");
		DocumentDAO docDao = DocumentDAO.get();
		if (doc.getId() != 0L && !isFolder)
			docDao.initialize(doc);

		Document realDoc = doc;

		GUIDocument guiDocument = new GUIDocument();
		guiDocument.setId(doc.getId());
		guiDocument.setDocRef(doc.getDocRef());
		guiDocument.setTenantId(doc.getTenantId());

		if (!isFolder && doc.getDocRef() != null && doc.getDocRef().longValue() != 0) {
			realDoc = docDao.findById(doc.getDocRef());
			docDao.initialize(realDoc);
			guiDocument.setDocRef(doc.getDocRef());
			guiDocument.setDocRefType(doc.getDocRefType());
		}

		guiDocument.setCustomId(realDoc.getCustomId());
		guiDocument.setRevision(realDoc.getRevision());
		guiDocument.setTags(new ArrayList<>(realDoc.getTagsAsWords()));
		guiDocument.setType(doc.getType());
		guiDocument.setFileName(doc.getFileName());
		guiDocument.setColor(doc.getColor());
		guiDocument.setVersion(realDoc.getVersion());
		guiDocument.setCreation(realDoc.getCreation());
		guiDocument.setCreator(realDoc.getCreator());
		guiDocument.setCreatorId(realDoc.getCreatorId());
		guiDocument.setDate(realDoc.getDate());
		guiDocument.setPublisher(realDoc.getPublisher());
		guiDocument.setPublisherId(realDoc.getPublisherId());
		guiDocument.setFileVersion(realDoc.getFileVersion());
		guiDocument.setLanguage(realDoc.getLanguage());
		guiDocument.setTemplateId(realDoc.getTemplateId());
		guiDocument.setLastModified(realDoc.getLastModified());
		guiDocument.setLockUserId(realDoc.getLockUserId());
		guiDocument.setLockUser(realDoc.getLockUser());
		guiDocument.setComment(realDoc.getComment());
		guiDocument.setLastNote(realDoc.getLastNote());
		guiDocument.setStatus(realDoc.getStatus().ordinal());
		guiDocument.setWorkflowStatus(realDoc.getWorkflowStatus());
		guiDocument.setWorkflowStatusDisplay(realDoc.getWorkflowStatusDisplay());
		guiDocument.setImmutable(realDoc.isImmutable());
		guiDocument.setFileSize(realDoc.getFileSize());
		guiDocument.setStartPublishing(realDoc.getStartPublishing());
		guiDocument.setStopPublishing(realDoc.getStopPublishing());
		guiDocument.setPublished(realDoc.isPublished());
		guiDocument.setSigned(realDoc.isSigned());
		guiDocument.setStamped(realDoc.isStamped());
		guiDocument.setIndexed(realDoc.getIndexed().ordinal());
		guiDocument.setEmbedded(realDoc.getEmbeddingStatus().ordinal());
		guiDocument.setExtResId(realDoc.getExtResId());
		guiDocument.setPages(realDoc.getPages());
		guiDocument.setPreviewPages(realDoc.getPreviewPages());
		guiDocument.setNature(realDoc.getNature());
		guiDocument.setFormId(realDoc.getFormId());
		guiDocument.setIcon(FileUtil.getBaseName(doc.getIcon()));
		guiDocument.setPasswordProtected(realDoc.isPasswordProtected());
		guiDocument.setLinks(realDoc.getLinks());
		guiDocument.setDocAttrs(realDoc.getDocAttrs());
		guiDocument.setOcrd(realDoc.isOcrd());
		guiDocument.setOcrTemplateId(realDoc.getOcrTemplateId());
		guiDocument.setBarcoded(realDoc.isBarcoded());
		guiDocument.setBarcodeTemplateId(realDoc.getBarcodeTemplateId());

		if (realDoc.getRating() != null)
			guiDocument.setRating(realDoc.getRating());

		if (realDoc.getCustomId() != null)
			guiDocument.setCustomId(realDoc.getCustomId());
		else
			guiDocument.setCustomId("");

		if (realDoc.getTemplate() != null) {
			guiDocument.setTemplate(realDoc.getTemplate().getName());
			guiDocument.setTemplateId(realDoc.getTemplate().getId());
		}

		setBookmarked(guiDocument, isFolder, sessionUser);

		List<GUIAttribute> attributes = new TemplateServiceImpl().prepareGUIAttributes(realDoc.getTemplate(), realDoc,
				sessionUser);
		guiDocument.setAttributes(attributes);

		if (folder != null) {
			guiDocument.setFolder(folder);
		} else {
			GUIFolder f = new GUIFolder(doc.getFolder().getId());
			f.setName(doc.getFolder().getName());
			guiDocument.setFolder(f);
		}

		FolderDAO fdao = FolderDAO.get();
		guiDocument.setPathExtended(fdao.computePathExtended(guiDocument.getFolder().getId()));

		return guiDocument;
	}

	private void setBookmarked(GUIDocument document, boolean isFolder, User sessionUser) throws PersistenceException {
		if (sessionUser != null && !isFolder) {
			BookmarkDAO bDao = BookmarkDAO.get();
			document.setBookmarked(bDao.isDocBookmarkedByUser(document.getId(), sessionUser.getId()));
			if (document.getDocRef() != null)
				document.setBookmarked(bDao.isDocBookmarkedByUser(document.getDocRef(), sessionUser.getId()));
		}
	}

	@Override
	public List<GUIVersion> getVersionsById(long id1, long id2) throws ServerException {
		Session session = validateSession();

		VersionDAO versDao = VersionDAO.get();
		Version docVersion;
		try {
			docVersion = versDao.findById(id1);
			if (docVersion != null)
				versDao.initialize(docVersion);
		} catch (Exception e) {
			return super.<List<GUIVersion>> throwServerException(session, log, e);
		}

		GUIVersion version1 = null;
		if (docVersion != null) {
			version1 = new GUIVersion();
			version1.setDocId(docVersion.getDocId());
			version1.setUsername(docVersion.getUsername());
			version1.setComment(docVersion.getComment());
			version1.setId(id1);
			version1.setCustomId(docVersion.getCustomId());
			version1.setRevision(docVersion.getRevision());
			version1.setTagsString(docVersion.getTgs());
			version1.setType(docVersion.getType());
			version1.setFileName(docVersion.getFileName());
			version1.setCreation(docVersion.getCreation());
			version1.setCreator(docVersion.getCreator());
			version1.setDate(docVersion.getDate());
			version1.setPublisher(docVersion.getPublisher());
			version1.setVersion(docVersion.getVersion());
			version1.setFileVersion(docVersion.getFileVersion());
			version1.setLanguage(docVersion.getLanguage());
			version1.setTemplateId(docVersion.getTemplateId());
			version1.setFileSize(docVersion.getFileSize());
			version1.setWorkflowStatus(docVersion.getWorkflowStatus());
			version1.setWorkflowStatusDisplay(docVersion.getWorkflowStatusDisplay());
			version1.setColor(docVersion.getColor());
			version1.setStartPublishing(docVersion.getStartPublishing());
			version1.setStopPublishing(docVersion.getStopPublishing());
			version1.setPublished(docVersion.isPublished());
			version1.setPages(docVersion.getPages());
			version1.setOcrd(docVersion.isOcrd());
			version1.setOcrTemplateId(docVersion.getOcrTemplateId());

			setGUIExtendedAttributes(docVersion, version1);

			GUIFolder folder1 = new GUIFolder();
			folder1.setName(docVersion.getFolderName());
			folder1.setId(docVersion.getFolderId());
			version1.setFolder(folder1);
		}

		try {
			docVersion = versDao.findById(id2);
			if (docVersion != null)
				versDao.initialize(docVersion);
		} catch (Exception e) {
			return super.<List<GUIVersion>> throwServerException(session, log, e);
		}

		GUIVersion version2 = null;
		if (docVersion != null) {
			version2 = new GUIVersion();
			version2.setDocId(docVersion.getDocId());
			version2.setUsername(docVersion.getUsername());
			version2.setComment(docVersion.getComment());
			version2.setId(id1);
			version2.setCustomId(docVersion.getCustomId());
			version2.setRevision(docVersion.getRevision());
			version2.setTagsString(docVersion.getTgs());
			version2.setType(docVersion.getType());
			version2.setFileName(docVersion.getFileName());
			version2.setCreation(docVersion.getCreation());
			version2.setCreator(docVersion.getCreator());
			version2.setDate(docVersion.getDate());
			version2.setPublisher(docVersion.getPublisher());
			version2.setVersion(docVersion.getVersion());
			version2.setFileVersion(docVersion.getFileVersion());
			version2.setLanguage(docVersion.getLanguage());
			version2.setFileSize(docVersion.getFileSize());
			version2.setWorkflowStatus(docVersion.getWorkflowStatus());
			version2.setColor(docVersion.getColor());
			version2.setStartPublishing(docVersion.getStartPublishing());
			version2.setStopPublishing(docVersion.getStopPublishing());
			version2.setPublished(docVersion.isPublished());
			version2.setPages(docVersion.getPages());
			version2.setOcrd(docVersion.isOcrd());
			version2.setOcrTemplateId(docVersion.getOcrTemplateId());
			version2.setBarcodeTemplateId(docVersion.getBarcodeTemplateId());

			setGUIExtendedAttributes(docVersion, version2);

			GUIFolder folder2 = new GUIFolder();
			folder2.setName(docVersion.getFolderName());
			folder2.setId(docVersion.getFolderId());
			version2.setFolder(folder2);
		}

		List<GUIVersion> versions = new ArrayList<>();
		if (version1 != null && version2 != null) {
			versions.add(version1);
			versions.add(version2);
		} else if (version1 != null) {
			versions.add(version1);
		} else if (version2 != null) {
			versions.add(version2);
		}

		return versions;
	}

	private void setGUIExtendedAttributes(Version docVersion, GUIVersion guiVersion) throws ServerException {
		VersionDAO versDao = VersionDAO.get();
		guiVersion.setTemplate(docVersion.getTemplateName());
		guiVersion.setTemplateId(docVersion.getTemplateId());

		try {
			versDao.initialize(docVersion);
		} catch (PersistenceException e) {
			throw new ServerException(e.getMessage(), e);
		}

		for (String attrName : docVersion.getAttributeNames()) {
			Attribute extAttr = docVersion.getAttributes().get(attrName);
			GUIAttribute att = new GUIAttribute();
			att.setName(attrName);
			att.setSetId(extAttr.getSetId());
			att.setPosition(extAttr.getPosition());
			att.setLabel(extAttr.getLabel());
			att.setMandatory(extAttr.isMandatory());
			att.setHidden(extAttr.isHidden());
			att.setReadonly(extAttr.isReadonly());
			att.setMultiple(extAttr.isMultiple());
			att.setParent(extAttr.getParent());
			att.setStringValues(extAttr.getStringValues());
			att.setEditor(extAttr.getEditor());
			att.setStringValue(extAttr.getStringValue());
			att.setIntValue(extAttr.getIntValue());
			att.setBooleanValue(extAttr.getBooleanValue());
			att.setDoubleValue(extAttr.getDoubleValue());
			att.setDateValue(extAttr.getDateValue());
			att.setType(extAttr.getType());
			guiVersion.addAttribute(att);
		}
	}

	@Override
	public void linkDocuments(List<Long> inDocIds, List<Long> outDocIds) throws ServerException {
		Session session = validateSession();

		DocumentLinkDAO linkDao = DocumentLinkDAO.get();
		DocumentDAO docDao = DocumentDAO.get();
		for (Long inDocId : inDocIds) {
			for (Long outDocId : outDocIds) {
				try {
					DocumentLink link = linkDao.findByDocIdsAndType(inDocId, outDocId, "default");
					if (link == null) {
						// The link doesn't exist and must be created
						link = new DocumentLink();
						link.setTenantId(session.getTenantId());

						link.setDocument1(docDao.findById(inDocId));
						link.setDocument2(docDao.findById(outDocId));
						link.setType("default");
						linkDao.store(link);
					}
				} catch (PersistenceException e) {
					throwServerException(session, log, e);
				}
			}
		}
	}

	@Override
	public void makeImmutable(List<Long> docIds, String comment) throws ServerException {
		Session session = validateSession();

		DocumentDAO docDao = DocumentDAO.get();
		DocumentManager manager = DocumentManager.get();
		try {
			for (long id : docIds) {
				Document doc = docDao.findById(id);

				if (!doc.isImmutable()) {
					// The document of the selected documentRecord must be
					// not locked
					if (doc.getStatus() != DocumentStatus.UNLOCKED) {
						continue;
					}

					// Create the document history event
					DocumentHistory transaction = new DocumentHistory();
					transaction.setSession(session);
					transaction.setComment(HTMLSanitizer.sanitizeSimpleText(comment));

					manager.makeImmutable(id, transaction);
				}
			}
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void markHistoryAsRead(String event) throws ServerException {
		Session session = validateSession();
		try {
			DocumentHistoryDAO.get().markHistoriesAsRead(event, session.getUserId());
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void markIndexable(List<Long> docIds, int policy) throws ServerException {
		Session session = validateSession();

		DocumentManager manager = DocumentManager.get();
		DocumentDAO docDao = DocumentDAO.get();
		for (long id : docIds)
			try {
				manager.changeIndexingStatus(docDao.findById(id), IndexingStatus.values()[policy]);
			} catch (PersistenceException e) {
				throwServerException(session, log, e);
			}

	}

	@Override
	public void markUnindexable(List<Long> docIds) throws ServerException {
		Session session = validateSession();

		DocumentDAO docDao = DocumentDAO.get();
		for (long id : docIds)
			try {
				DocumentManager.get().changeIndexingStatus(docDao.findById(id), IndexingStatus.SKIP);
			} catch (PersistenceException e) {
				throwServerException(session, log, e);
			}
	}

	@Override
	public void restore(List<Long> docIds, long folderId) throws ServerException {
		Session session = validateSession();

		for (Long docId : docIds) {
			if (docId == null)
				continue;
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);

			try {
				DocumentDAO.get().restore(docId, folderId, transaction);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
		}
	}

	@Override
	public void validate(GUIDocument document) throws ServerException {
		Session session = validateSession();

		try {
			Document object = toDocument(document);

			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			transaction.setEvent(document.getId() == 0L ? DocumentEvent.CHANGED : DocumentEvent.STORED);
			transaction.setComment(HTMLSanitizer.sanitizeSimpleText(document.getComment()));

			Validator validator = new Validator();
			validator.validate(object, object.getTemplate(), transaction);
		} catch (PersistenceException | AutomationException e) {
			throwServerException(session, log, e);
		}

	}

	@Override
	public GUIDocument save(GUIDocument guiDocument) throws ServerException {
		Session session = validateSession();
		if (guiDocument.getId() == 0L)
			return null;

		try {
			DocumentDAO docDao = DocumentDAO.get();
			Document document = docDao.findById(guiDocument.getId());
			docDao.initialize(document);

			if (guiDocument.getDocRef() != null) {
				/*
				 * We are saving an alias
				 */
				document.setFileName(guiDocument.getFileName());
				document.setColor(guiDocument.getColor());
				document.setType(FileUtil.getExtension(document.getFileName()).toLowerCase());
				docDao.store(document);

				// Load the real target document for further updates
				document = docDao.findById(guiDocument.getDocRef());
				docDao.initialize(document);
			}

			// Fix the name of multiple attributes
			for (GUIAttribute att : guiDocument.getAttributes().stream().filter(att -> att.isMultiple()).toList()) {
				NumberFormat nf = new DecimalFormat("0000");
				List<GUIAttribute> values = guiDocument.getValues(att.getName());
				int index = 0;
				for (GUIAttribute val : values) {
					// Skip first element
					if (index > 0)
						val.setName(att.getName() + "-" + nf.format(index));
					index++;
				}
			}

			Document docVO = toDocument(guiDocument);
			if (guiDocument.getDocRef() != null) {
				docVO.setDocRef(null);
				docVO.setFileName(document.getFileName());
				docVO.setColor(document.getColor());
				docVO.setType(FileUtil.getExtension(document.getFileName()).toLowerCase());
			}
			docVO.setTenantId(session.getTenantId());

			// Make sure to maintain relevant flags from real document
			docVO.setOcrd(document.isOcrd());
			docVO.setBarcoded(document.isBarcoded());

			// Create the document history event
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			transaction.setEvent(DocumentEvent.CHANGED);
			transaction.setComment(HTMLSanitizer.sanitizeSimpleText(guiDocument.getComment()));

			DocumentManager.get().update(document, docVO, transaction);
			return getById(guiDocument.getId());
		} catch (PersistenceException | ServerException e) {
			return throwServerException(session, log, e);
		}

	}

	private static void setAllowedPermissions(Session session, long documentId, GUIDocument guiDocument)
			throws PersistenceException {
		if (session != null) {
			DocumentDAO dao = DocumentDAO.get();
			Set<Permission> permissions = dao.getAllowedPermissions(documentId, session.getUserId());
			guiDocument.setAllowedPermissions(new GUIAccessControlEntry(
					permissions.stream().map(p -> p.name().toLowerCase()).toList().toArray(new String[0])));
		}
	}

	/**
	 * Produces a plain new Document from a GUIDocument
	 * 
	 * @param guiDocument the GUI document
	 * 
	 * @return the core Document
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public static Document toDocument(GUIDocument guiDocument) throws PersistenceException {
		Document docVO = new Document();
		docVO.setTagsFromWords(new HashSet<>(guiDocument.getTags()));
		docVO.setCustomId(HTMLSanitizer.sanitizeSimpleText(guiDocument.getCustomId()));
		docVO.setRevision(HTMLSanitizer.sanitizeSimpleText(guiDocument.getRevision()));
		docVO.setFileName(HTMLSanitizer.sanitizeSimpleText(guiDocument.getFileName()));
		docVO.setVersion(guiDocument.getVersion());
		docVO.setCreation(guiDocument.getCreation());
		docVO.setCreator(guiDocument.getCreator());
		docVO.setDate(guiDocument.getDate());
		docVO.setPublisher(guiDocument.getPublisher());
		docVO.setFileVersion(guiDocument.getFileVersion());
		docVO.setLanguage(guiDocument.getLanguage());
		docVO.setFileSize(guiDocument.getFileSize());

		docVO.setRating(guiDocument.getRating());
		docVO.setComment(HTMLSanitizer.sanitizeSimpleText(guiDocument.getComment()));
		docVO.setWorkflowStatus(guiDocument.getWorkflowStatus());
		docVO.setWorkflowStatusDisplay(guiDocument.getWorkflowStatusDisplay());
		docVO.setColor(guiDocument.getColor());
		docVO.setStartPublishing(guiDocument.getStartPublishing());
		docVO.setStopPublishing(guiDocument.getStopPublishing());
		docVO.setPublished(guiDocument.isPublished());
		docVO.setBarcoded(guiDocument.isBarcoded());
		docVO.setExtResId(guiDocument.getExtResId());
		docVO.setPages(guiDocument.getPages());
		docVO.setPreviewPages(guiDocument.getPreviewPages());
		docVO.setNature(guiDocument.getNature());
		docVO.setFormId(guiDocument.getFormId());
		docVO.setOcrTemplateId(guiDocument.getOcrTemplateId());
		docVO.setBarcodeTemplateId(guiDocument.getBarcodeTemplateId());

		if (guiDocument.getTemplateId() != null) {
			docVO.setTemplateId(guiDocument.getTemplateId());
			TemplateDAO templateDao = TemplateDAO.get();
			Template template = templateDao.findById(guiDocument.getTemplateId());
			templateDao.initialize(template);
			docVO.setTemplate(template);

			if (CollectionUtils.isNotEmpty(guiDocument.getAttributes()))
				toAttributes(guiDocument, docVO, template);
		}

		docVO.setStatus(guiDocument.getStatus());
		FolderDAO fdao = FolderDAO.get();
		if (guiDocument.getFolder() != null)
			docVO.setFolder(fdao.findById(guiDocument.getFolder().getId()));
		return docVO;
	}

	private static void toAttributes(GUIDocument guiDocument, Document docVO, Template template) {
		for (GUIAttribute attr : guiDocument.getAttributes()) {
			Attribute templateAttribute = template.getAttributes()
					.get(attr.getParent() != null ? attr.getParent() : attr.getName());
			// This control is necessary because, changing
			// the template, the values of the old template
			// attributes keys remains on the form value
			// manager,
			// so the GUIDocument contains also the old
			// template attributes keys that must be
			// skipped.
			if (templateAttribute == null)
				continue;

			Attribute extAttr = new Attribute();
			int templateType = templateAttribute.getType();
			int extAttrType = attr.getType();

			if (templateType != extAttrType) {
				updateAttributeValue1(attr, extAttr, templateType);
			} else {
				updateAttributeValue2(attr, extAttr, templateType);
			}

			extAttr.setParent(attr.getParent());
			extAttr.setDependsOn(attr.getDependsOn());
			extAttr.setStringValues(attr.getStringValues());
			extAttr.setLabel(templateAttribute.getLabel());
			extAttr.setType(templateType);
			extAttr.setPosition(attr.getPosition());
			extAttr.setMandatory(templateAttribute.isMandatory());
			extAttr.setHidden(templateAttribute.isHidden());
			extAttr.setStringValues(attr.getStringValues());
			if (attr.getParent() == null)
				extAttr.setMultiple(templateAttribute.isMultiple());
			extAttr.setSetId(templateAttribute.getSetId());

			docVO.getAttributes().put(attr.getName(), extAttr);
		}
	}

	private static void updateAttributeValue2(GUIAttribute attr, Attribute extAttr, int templateType) {
		if (templateType == Attribute.TYPE_INT) {
			setIntValue(attr, extAttr);
		} else if (templateType == Attribute.TYPE_BOOLEAN) {
			setBooleanValue(attr, extAttr);
		} else if (templateType == Attribute.TYPE_DOUBLE) {
			setDoubleValue(attr, extAttr);
		} else if (templateType == Attribute.TYPE_DATE) {
			setDateValue(attr, extAttr);
		} else if (templateType == Attribute.TYPE_STRING) {
			setStringValue(attr, extAttr);
		} else if (templateType == Attribute.TYPE_USER || templateType == Attribute.TYPE_FOLDER
				|| templateType == Attribute.TYPE_DOCUMENT) {
			setUserValue(attr, extAttr, templateType);
		}
	}

	private static void setUserValue(GUIAttribute attr, Attribute extAttr, int templateType) {
		if (attr.getValue() != null) {
			extAttr.setIntValue(attr.getIntValue());
			extAttr.setStringValue(attr.getStringValue());
		} else {
			extAttr.setIntValue(null);
			extAttr.setStringValue(null);
		}
		extAttr.setType(templateType);
	}

	private static void setStringValue(GUIAttribute attr, Attribute extAttr) {
		if (attr.getValue() != null)
			extAttr.setStringValue(HTMLSanitizer.sanitizeSimpleText((String) attr.getValue()));
		else
			extAttr.setStringValue(null);
	}

	private static void setDateValue(GUIAttribute attr, Attribute extAttr) {
		if (attr.getValue() != null) {
			extAttr.setDateValue(fixDateForDB((Date) attr.getValue()));
		} else
			extAttr.setDateValue(null);
	}

	private static void setDoubleValue(GUIAttribute attr, Attribute extAttr) {
		if (attr.getValue() != null)
			extAttr.setDoubleValue((Double) attr.getValue());
		else
			extAttr.setDoubleValue(null);
	}

	private static void setBooleanValue(GUIAttribute attr, Attribute extAttr) {
		if (attr.getBooleanValue() != null)
			extAttr.setValue(attr.getBooleanValue());
		else
			extAttr.setBooleanValue(null);
	}

	private static void setIntValue(GUIAttribute attr, Attribute extAttr) {
		if (attr.getValue() != null)
			extAttr.setIntValue((Long) attr.getValue());
		else
			extAttr.setIntValue(null);
	}

	private static void updateAttributeValue1(GUIAttribute attr, Attribute extAttr, int templateType) {
		// This check is useful to avoid errors
		// related to the old template
		// attributes keys that remains on the form
		// value manager
		if (attr.getValue() != null && attr.getValue().toString().trim().isEmpty() && templateType != 0) {
			if (templateType == Attribute.TYPE_INT || templateType == Attribute.TYPE_BOOLEAN) {
				extAttr.setIntValue(null);
			} else if (templateType == Attribute.TYPE_DOUBLE) {
				extAttr.setDoubleValue(null);
			} else if (templateType == Attribute.TYPE_DATE) {
				extAttr.setDateValue(null);
			}
		} else if (templateType == GUIAttribute.TYPE_DOUBLE) {
			extAttr.setValue(Double.parseDouble(attr.getValue().toString()));
		} else if (templateType == GUIAttribute.TYPE_INT) {
			extAttr.setValue(Long.parseLong(attr.getValue().toString()));
		} else if (templateType == GUIAttribute.TYPE_BOOLEAN) {
			extAttr.setValue(attr.getBooleanValue());
			extAttr.setType(Attribute.TYPE_BOOLEAN);
		} else if (templateType == GUIAttribute.TYPE_USER || templateType == GUIAttribute.TYPE_FOLDER
				|| templateType == GUIAttribute.TYPE_DOCUMENT) {
			extAttr.setIntValue(attr.getIntValue());
			extAttr.setStringValue(attr.getStringValue());
			extAttr.setType(templateType);
		}
	}

	@Override
	public String sendAsEmail(GUIEmail guiMail, String locale) throws ServerException {
		Session session = validateSession();
		DocumentDAO documentDao = DocumentDAO.get();

		EMail mail = new EMail();
		mail.setHtml(true);
		mail.setTenantId(session.getTenantId());
		mail.setAccountId(-1);
		mail.setFolder(OUTBOX);
		mail.setSentDate(new Date());
		mail.setUsername(session.getUsername());

		setAuthorAddress(mail, guiMail, session);

		mail.parseRecipientsBCC(guiMail.getBccs().stream().map(c -> c.getEmail()).collect(Collectors.joining(",")));
		mail.parseRecipientsCC(guiMail.getCcs().stream().map(c -> c.getEmail()).collect(Collectors.joining(",")));
		mail.parseRecipients(guiMail.getTos().stream().map(c -> c.getEmail()).collect(Collectors.joining(",")));

		List<Document> attachedDocs = documentDao.findByIds(guiMail.getDocIds().stream().collect(Collectors.toSet()),
				null);
		for (Document document : attachedDocs)
			documentDao.initialize(document);

		/*
		 * Subject and email are processed by the scripting engine
		 */
		Automation engine = new Automation("sendmail", LocaleUtil.toLocale(locale), session.getTenantId());
		Map<String, Object> dictionary = new HashMap<>();
		dictionary.put(Automation.LOCALE, LocaleUtil.toLocale(locale));
		dictionary.put("sender", session.getUser());
		dictionary.put("documents", attachedDocs);
		dictionary.put(DOCUMENT, attachedDocs.get(0));

		try {
			prepareDownloadTicket(guiMail, locale, session, dictionary);

			String message = engine.evaluate(guiMail.getMessage(), dictionary);
			mail.setSubject(engine.evaluate(guiMail.getSubject(), dictionary));

			if (guiMail.isSendAsTicket()) {
				if (!guiMail.getMessage().contains(DOWNLOAD_TICKET))
					message += "<br/><br/>" + dictionary.get(DOWNLOAD_TICKET);

				Document doc = documentDao.findDocument(guiMail.getDocIds().get(0));

				if (doc.getDocRef() != null)
					doc = documentDao.findById(doc.getDocRef());

				writeMessageWithThumbnail(mail, doc, message, session);
			} else {
				if (guiMail.isZipCompression()) {
					prepareZipAttachment(mail, guiMail.getDocIds(), guiMail.isPdfConversion(), session);
				} else {
					for (long id : guiMail.getDocIds())
						createAttachment(mail, id, guiMail.isPdfConversion(), session.getSid());
				}

				mail.setMessageText("<html><head><meta charset='utf-8' /></head><body>" + message + "</body></html>");
			}

			return sendEmail(mail, session, attachedDocs);
		} catch (PermissionException | PersistenceException | IOException | AutomationException e) {
			log.warn(e.getMessage(), e);
			return ERROR;
		}
	}

	private void setAuthorAddress(EMail mail, GUIEmail guiMail, Session session) {
		mail.setAuthor(session.getUser().getUsername());
		if (Context.get().getProperties().getBoolean(session.getTenantName() + SMTP_USERASFROM, true)) {
			if (guiMail.getFrom() != null)
				mail.setAuthorAddress(guiMail.getFrom().getEmail());
			else
				mail.setAuthorAddress(session.getUser().getEmail());
		}
	}

	private void prepareZipAttachment(EMail mail, List<Long> docIds, boolean pdfConversion, Session session)
			throws IOException {
		/*
		 * Create a temporary archive for sending it as unique attachment
		 */
		File zipFile = FileUtil.createTempFile("email", "zip");
		try (OutputStream out = new FileOutputStream(zipFile);) {
			// Create the document history event
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			transaction.setEvent(DocumentEvent.DOWNLOADED);

			ZipExport export = new ZipExport();
			export.process(docIds.toArray(new Long[0]), out, pdfConversion, transaction);
			createAttachment(mail, zipFile);
		} catch (IOException | PersistenceException t) {
			log.error(t.getMessage(), t);
		} finally {
			FileUtil.delete(zipFile);
		}
	}

	private void writeMessageWithThumbnail(EMail mail, Document doc, String message, Session session) {
		File thumbnailFile = null;
		try {
			thumbnailFile = createTile(doc, session.getSid());
			if (thumbnailFile != null && thumbnailFile.length() > 0) {
				message += "<p><img src='data:image/png;base64," + ImageUtil.encode(thumbnailFile) + "'/></p>";
			}
			mail.setMessageText("<html><head><meta charset='utf-8' /></head><body>" + message + "<rl /></body></html>");
		} catch (IOException | PersistenceException ioe) {
			log.warn(ioe.getMessage());
		} finally {
			FileUtil.delete(thumbnailFile);
		}
	}

	private void prepareDownloadTicket(GUIEmail email, String locale, Session session, Map<String, Object> dictionary)
			throws PersistenceException, PermissionException {
		if (email.isSendAsTicket()) {
			// Prepare a new download ticket
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);

			Document doc = DocumentDAO.get().findDocument(email.getDocIds().get(0));

			Ticket ticket = new Ticket();
			ticket.setTenantId(session.getTenantId());
			ticket.setType(Ticket.DOWNLOAD);
			ticket.setDocId(email.getDocIds().get(0));

			ticket = DocumentManager.get().createTicket(ticket, transaction);
			String ticketDiv = "<div style='margin-top:10px; border-top:1px solid black; background-color:#CCCCCC;'><b>&nbsp;"
					+ I18N.message("clicktodownload", LocaleUtil.toLocale(locale)) + ": <a href='" + ticket.getUrl()
					+ "'>" + doc.getFileName() + "</a></b></div>";
			dictionary.put(DOWNLOAD_TICKET, ticketDiv);
		}
	}

	private String sendEmail(EMail mail, Session session, List<Document> attachedDocs) {
		try {
			DocumentDAO documentDao = DocumentDAO.get();

			// Send the message
			EMailSender sender = getEmailSender(session);
			sender.send(mail);

			FolderDAO fDao = FolderDAO.get();
			for (Document d : attachedDocs) {
				Document doc = d;
				if (doc.getDocRef() != null)
					doc = documentDao.findById(doc.getDocRef());

				// Create the document history event
				DocumentHistory history = new DocumentHistory();
				history.setSession(session);
				history.setDocument(doc);
				history.setEvent(DocumentEvent.SENT);
				history.setComment(
						StringUtils.abbreviate(StringUtil.collectionToString(mail.getRecipients(), ", "), 4000));
				history.setFilename(doc.getFileName());
				history.setVersion(doc.getVersion());
				history.setFileVersion(doc.getFileVersion());
				history.setPath(fDao.computePathExtended(doc.getFolder().getId()));
				documentDao.saveDocumentHistory(doc, history);
			}

			/*
			 * Save the recipients in the user's contacts
			 */
			ContactDAO cdao = ContactDAO.get();
			for (Recipient recipient : mail.getRecipients()) {
				List<Contact> contacts = cdao.findByUser(session.getUserId(), recipient.getAddress());
				if (contacts.isEmpty()) {
					Contact cont = new Contact();
					cont.setUserId(session.getUserId());
					cont.setEmail(recipient.getAddress().trim());
					cdao.store(cont);
				}
			}

			return "ok";
		} catch (Exception ex) {
			log.warn(ex.getMessage(), ex);
			return ERROR;
		}
	}

	private static EMailSender getEmailSender(Session session) {
		if (emailSender != null) {
			emailSender.setTenant(session.getTenantId());
			return emailSender;
		} else
			return new EMailSender(session.getTenantName());
	}

	private File createTile(Document doc, String sid) throws IOException, PersistenceException {
		Store store = Store.get();
		StoreResource tileResource = StoreResource.builder().document(doc).suffixTile().build();

		// In any case try to produce the thumbnail
		if (store.size(tileResource) <= 0L) {
			try {
				ThumbnailManager.get().createTile(doc, doc.getFileVersion(), sid);
			} catch (IOException e) {
				log.error(e.getMessage(), e);
			}
		}

		if (store.exists(tileResource)) {
			File file = FileUtil.createTempFile("tile-", ".png");
			store.writeToFile(tileResource, file);
			return file;
		}

		return null;
	}

	private void createAttachment(EMail email, long docId, boolean pdfConversion, String sid)
			throws IOException, PersistenceException {
		DocumentDAO docDao = DocumentDAO.get();
		Store store = Store.get();
		Document doc = docDao.findDocument(docId);
		StoreResource resource = StoreResource.builder().document(doc).build();

		boolean convertToPdf = pdfConversion;
		if ((doc.getDocRef() != null) && ("pdf".equals(doc.getDocRefType()))) {
			// this is an alias
			doc = docDao.findById(doc.getDocRef());
			convertToPdf = true;
		}

		EMailAttachment att = new EMailAttachment();
		att.setIcon(doc.getIcon());
		att.setFileName(doc.getFileName());
		String extension = doc.getFileExtension();
		att.setMimeType(MimeType.get(extension));

		if (convertToPdf) {
			if (!"pdf".equals(FileUtil.getExtension(doc.getFileName().toLowerCase()))) {
				FormatConversionManager.get().convertToPdf(doc, sid);
				resource = StoreResource.builder().document(doc).suffixPdfConversion().build();
			}
			att.setMimeType(MimeType.get("pdf"));
			att.setFileName(FileUtil.getBaseName(doc.getFileName()) + ".pdf");
		}

		att.setData(store.getBytes(resource));

		email.addAttachment(2 + email.getAttachments().size(), att);
	}

	private void createAttachment(EMail email, File zipFile) {
		EMailAttachment att = new EMailAttachment();
		att.setData(FileUtil.toByteArray(zipFile));
		att.setFileName("doc.zip");
		String extension = "zip";
		att.setMimeType(MimeType.get(extension));

		email.addAttachment(2 + email.getAttachments().size(), att);
	}

	@Override
	public void unlock(List<Long> docIds) throws ServerException {
		Session session = validateSession();

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSession(session);

		try {
			// Unlock the document; throws an exception if something
			// goes wrong
			DocumentManager documentManager = DocumentManager.get();
			for (long id : docIds) {
				documentManager.unlock(id, transaction);
			}
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void updateBookmark(GUIBookmark bookmark) throws ServerException {
		Session session = validateSession();

		BookmarkDAO bookmarkDao = BookmarkDAO.get();
		Bookmark bk;
		try {
			if (bookmark.getId() != 0) {
				bk = bookmarkDao.findById(bookmark.getId());
				bookmarkDao.initialize(bk);
			} else
				return;

			bk.setTitle(bookmark.getName());
			bk.setDescription(bookmark.getDescription());

			bookmarkDao.store(bk);
			bookmark.setId(bk.getId());
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}

	}

	@Override
	public void updateLink(long id, String type) throws ServerException {
		Session session = validateSession();

		DocumentLinkDAO dao = DocumentLinkDAO.get();
		try {
			DocumentLink link = dao.findById(id);
			dao.initialize(link);
			link.setType(type);
			dao.store(link);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void cleanUploadedFileFolder() throws ServerException {
		Session session = validateSession();
		UploadServlet.cleanUploads(session.getSid());
	}

	@Override
	public GUIRating getRating(long docId) throws ServerException {
		Session session = validateSession();

		RatingDAO ratingDao = RatingDAO.get();
		try {
			GUIRating rating = new GUIRating();
			Rating rat = ratingDao.findVotesByDocId(docId);
			if (rat != null) {
				ratingDao.initialize(rat);

				rating.setId(rat.getId());
				rating.setDocId(docId);
				// We use the rating userId value to know in the GUI if the user
				// has already vote this document.
				if (ratingDao.findByDocIdAndUserId(docId, session.getUserId()) != null)
					rating.setUserId(session.getUserId());
				rating.setCount(rat.getCount());
				rating.setAverage(rat.getAverage());
			} else {
				rating.setDocId(docId);
				rating.setCount(0);
				rating.setAverage(0F);
			}

			return rating;
		} catch (PersistenceException e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public int saveRating(GUIRating rating) throws ServerException {
		Session session = validateSession();

		RatingDAO ratingDao = RatingDAO.get();
		try {
			Rating rat = ratingDao.findByDocIdAndUserId(rating.getDocId(), rating.getUserId());
			if (rat == null) {
				rat = new Rating();
				rat.setTenantId(session.getTenantId());
				rat.setDocId(rating.getDocId());
				rat.setUserId(session.getUserId());
				rat.setUsername(session.getUser().getFullName());
			}
			rat.setVote(rating.getVote());

			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);

			ratingDao.store(rat, transaction);

			DocumentDAO docDao = DocumentDAO.get();
			Document doc = docDao.findById(rating.getDocId());
			return doc.getRating();
		} catch (PersistenceException e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public GUIDocumentNote saveNote(GUIDocumentNote guiNote) throws ServerException {
		Session session = validateSession();

		try {
			DocumentDAO docDao = DocumentDAO.get();
			Document document = docDao.findById(guiNote.getDocId());

			return saveNote(session, document, guiNote);
		} catch (PersistenceException | ServerException e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public GUIDocumentNote getNote(long noteId) throws ServerException {
		Session session = validateSession();

		try {
			DocumentNoteDAO noteDao = DocumentNoteDAO.get();
			DocumentNote note = noteDao.findById(noteId);
			if (note != null)
				noteDao.initialize(note);
			else
				return null;
			return toGUINote(note, session);
		} catch (PersistenceException e) {
			return throwServerException(session, log, e);
		}
	}

	private GUIDocumentNote toGUINote(DocumentNote note, Session session) throws PersistenceException {
		GUIDocumentNote guiNote = new GUIDocumentNote(note.getId(), note.getFileVersion());
		guiNote.setId(note.getId());
		guiNote.setDocId(note.getDocId());
		guiNote.setColor(note.getColor());
		guiNote.setDate(note.getDate());
		guiNote.setFileName(note.getFileName());
		guiNote.setHeight(note.getHeight());
		guiNote.setLeft(note.getLeft());
		guiNote.setLineColor(note.getLineColor());
		guiNote.setLineOpacity(note.getLineOpacity());
		guiNote.setLineWidth(note.getLineWidth());
		guiNote.setMessage(note.getMessage());
		guiNote.setOpacity(note.getOpacity());
		guiNote.setPage(note.getPage());
		guiNote.setRecipient(note.getRecipient());
		guiNote.setRecipientEmail(note.getRecipientEmail());
		guiNote.setRotation(note.getRotation());
		guiNote.setShape(note.getShape());
		guiNote.setTop(note.getTop());
		guiNote.setType(note.getType());
		guiNote.setUserId(note.getUserId());
		guiNote.setUsername(note.getUsername());

		for (NoteAccessControlEntry ace : note.getAccessControlList()) {
			GUIAccessControlEntry guiAce = new GUIAccessControlEntry();
			guiAce.setEntityId(ace.getGroupId());
			guiAce.setRead(ace.getRead() == 1);
			guiAce.setWrite(ace.getWrite() == 1);
			guiAce.setDelete(ace.getDelete() == 1);
			guiAce.setSecurity(ace.getSecurity() == 1);
			guiNote.getAccessControlList().add(guiAce);
		}

		Set<Permission> permissions = DocumentNoteDAO.get().getAllowedPermissions(note.getId(), session.getUserId());
		GUIAccessControlEntry allowedPermissions = new GUIAccessControlEntry();
		allowedPermissions.setRead(permissions.contains(Permission.READ));
		allowedPermissions.setWrite(permissions.contains(Permission.WRITE));
		allowedPermissions.setDelete(permissions.contains(Permission.DELETE));
		allowedPermissions.setSecurity(permissions.contains(Permission.SECURITY));
		guiNote.setAllowedPermissions(allowedPermissions);

		return guiNote;
	}

	@Override
	public List<GUIDocumentNote> getNotes(long docId, String fileVersion, Collection<String> types)
			throws ServerException {
		Session session = validateSession();

		try {
			Document document = retrieveDocument(docId);

			if (document == null)
				throw new ServerException(UNEXISTING_DOCUMENT + " " + docId);

			List<GUIDocumentNote> guiNotes = new ArrayList<>();
			DocumentNoteDAO dao = DocumentNoteDAO.get();

			List<DocumentNote> notes = dao.findByDocIdAndTypes(document.getId(), session.getUserId(),
					fileVersion != null ? fileVersion : document.getFileVersion(), types);
			for (DocumentNote note : notes) {
				dao.initialize(note);
				guiNotes.add(toGUINote(note, session));
			}

			return guiNotes;
		} catch (PersistenceException e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public void saveNotes(long docId, String fileVersion, List<GUIDocumentNote> notes, Collection<String> types)
			throws ServerException {
		Session session = validateSession();

		DocumentNoteDAO dao = DocumentNoteDAO.get();
		Document document = null;
		try {
			document = retrieveDocument(docId);
			if (document == null)
				throw new UnexistingResourceException(DOCUMENT_STR + docId);

			/*
			 * Check for deletions
			 */

			// Get all the notes
			List<DocumentNote> documentNotes = dao.findByDocIdAndTypes(document.getId(), User.USERID_ADMIN,
					fileVersion != null ? fileVersion : document.getFileVersion(), types);

			// Maintain just those notes created by current user or all notes in
			// case of admistrator
			List<Long> actualNoteIds = documentNotes.stream()
					.filter(note -> session.getUser().isAdmin() || note.getUserId() == session.getUserId())
					.map(PersistentObject::getId).toList();
			List<Long> noteIds = notes.stream().map(GUIDocumentNote::getId).toList();
			for (Long actualNoteId : actualNoteIds)
				if (!noteIds.contains(actualNoteId))
					dao.delete(actualNoteId);

			/*
			 * Do the updates / inserts
			 */
			for (GUIDocumentNote guiNote : notes)
				saveNote(session, document, guiNote);
		} catch (UnexistingResourceException | PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	private GUIDocumentNote saveNote(Session session, Document document, GUIDocumentNote guiNote)
			throws ServerException, PersistenceException {

		DocumentNoteDAO dao = DocumentNoteDAO.get();

		DocumentNote note = dao.findById(guiNote.getId());
		if (note == null) {
			note = new DocumentNote();
			note.setTenantId(session.getTenantId());
			note.setDocId(document.getId());
			note.setFileVersion(
					guiNote.getFileVersion() != null ? guiNote.getFileVersion() : document.getFileVersion());
			note.setUserId(session.getUserId());
			note.setUsername(session.getUser().getFullName());
			note.setDate(new Date());
			note.setPage(guiNote.getPage());
		} else {
			dao.initialize(note);
		}

		note.setFileName(document.getFileName());
		note.setMessage(guiNote.getMessage());
		note.setColor(guiNote.getColor());
		note.setTop(guiNote.getTop());
		note.setLeft(guiNote.getLeft());
		note.setWidth(guiNote.getWidth());
		note.setHeight(guiNote.getHeight());
		note.setType(guiNote.getType());
		note.setRecipient(guiNote.getRecipient());
		note.setRecipientEmail(guiNote.getRecipientEmail());
		note.setShape(guiNote.getShape());
		note.setLineColor(guiNote.getLineColor());
		note.setLineOpacity(guiNote.getLineOpacity());
		note.setLineWidth(guiNote.getLineWidth());
		note.setRotation(guiNote.getRotation());

		note.getAccessControlList().clear();
		for (GUIAccessControlEntry guiAce : guiNote.getAccessControlList()) {
			NoteAccessControlEntry ace = new NoteAccessControlEntry();
			ace.setGroupId(guiAce.getEntityId());
			ace.setRead(guiAce.isRead() ? 1 : 0);
			ace.setWrite(guiAce.isWrite() ? 1 : 0);
			ace.setDelete(guiAce.isDelete() ? 1 : 0);
			ace.setSecurity(guiAce.isSecurity() ? 1 : 0);
			note.addAccessControlEntry(ace);
		}

		saveNote(note, session);

		/*
		 * If the note specifies a recipient, update the user's address book
		 */
		updateUserAddressBook(session, note);

		return getNote(note.getId());
	}

	private void updateUserAddressBook(Session session, DocumentNote note) throws PersistenceException {
		if (StringUtils.isNotEmpty(note.getRecipientEmail())) {
			List<Contact> contacts = ContactDAO.get().findByUser(session.getUserId(), note.getRecipientEmail());
			if (contacts.isEmpty()) {
				String firstName = note.getRecipient();
				String lastName = null;
				if (firstName.contains(" ")) {
					firstName = firstName.substring(0, note.getRecipient().lastIndexOf(' ')).trim();
					lastName = note.getRecipient().substring(note.getRecipient().lastIndexOf(' ')).trim();
				}

				Contact contact = new Contact();
				contact.setUserId(session.getUserId());
				contact.setFirstName(firstName);
				contact.setLastName(lastName);
				contact.setEmail(note.getRecipientEmail());
				saveContact(contact);
			}
		}
	}

	private void saveNote(DocumentNote note, Session session) throws ServerException {
		try {
			DocumentNoteDAO dao = DocumentNoteDAO.get();
			if (note.getId() == 0L) {
				DocumentHistory transaction = new DocumentHistory();
				transaction.setSession(session);
				dao.store(note, transaction);
			} else {
				dao.store(note);
			}
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	private void saveContact(Contact contact) {
		try {
			ContactDAO.get().store(contact);
		} catch (PersistenceException e) {
			log.warn("Error storing new contact {}", contact.getEmail(), e);
		}
	}

	@Override
	public void deleteNotes(List<Long> ids) throws ServerException {
		validateSession();

		DocumentNoteDAO dao = DocumentNoteDAO.get();
		for (long id : ids)
			try {
				dao.delete(id);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
	}

	@Override
	public List<GUIDocument> bulkUpdate(List<Long> ids, GUIDocument vo, boolean ignoreEmptyFields)
			throws ServerException {
		Session session = validateSession();

		List<GUIDocument> updatedDocs = new ArrayList<>();
		for (long docId : ids) {
			try {
				GUIDocument buf = bulkUpdateDocument(docId, vo, ignoreEmptyFields, session);
				if (buf != null) {
					GUIDocument document = save(buf);
					updatedDocs.add(document);
					document.setAccessControlList(vo.getAccessControlList());
					saveACL(document);
				}
			} catch (ServerException e) {
				log.error(e.getMessage(), e);
			}
		}
		return updatedDocs;
	}

	private GUIDocument bulkUpdateDocument(long docId, GUIDocument model, boolean ignoreEmptyFields, Session session)
			throws ServerException {
		GUIDocument document = getById(docId);

		if (document.isImmutable() || document.getStatus() != DocumentStatus.UNLOCKED.ordinal()) {
			log.warn("Skip document {} because immutable or locked", docId);
			return null;
		}

		try {
			checkPermission(Permission.WRITE, session.getUser(), document.getFolder().getId());
		} catch (AccessDeniedException e) {
			log.warn("Skip document {} because  user {} does not have write permission", docId, session.getUsername());
			return null;
		}

		document.setComment(HTMLSanitizer.sanitizeSimpleText(model.getComment() != null ? model.getComment() : ""));

		document.setPublished(model.isPublished());
		if (model.getStartPublishing() != null)
			document.setStartPublishing(model.getStartPublishing());
		if (model.getStopPublishing() != null)
			document.setStopPublishing(model.getStopPublishing());
		if (StringUtils.isNotEmpty(model.getLanguage()))
			document.setLanguage(model.getLanguage());
		if (StringUtils.isNotEmpty(model.getColor()))
			document.setColor(model.getColor());
		document.setTags(model.getTags());
		if (model.getTemplateId() != null)
			document.setTemplateId(model.getTemplateId());

		setOcrTemplate(model, ignoreEmptyFields, document);

		setBarcodeTemplate(model, ignoreEmptyFields, document);

		setExtendedAttributes(model, ignoreEmptyFields, document);

		return document;
	}

	private void setExtendedAttributes(GUIDocument model, boolean ignoreEmptyFields, GUIDocument document) {
		if (model.getAttributes().isEmpty())
			return;

		if (ignoreEmptyFields) {
			Map<String, GUIAttribute> attributes = new HashMap<>();
			for (GUIAttribute att : document.getAttributes())
				attributes.put(att.getName(), att);

			for (GUIAttribute att : model.getAttributes()) {
				if (att.getValue() != null && StringUtils.isNotEmpty(att.getValue().toString()))
					attributes.put(att.getName(), att);
			}
			document.setAttributes(attributes.values().stream().toList());
		} else {
			document.setAttributes(model.getAttributes());
		}
	}

	private void setBarcodeTemplate(GUIDocument model, boolean ignoreEmptyFields, GUIDocument document) {
		if (model.getBarcodeTemplateId() != null)
			document.setBarcodeTemplateId(model.getBarcodeTemplateId());
		else if (!ignoreEmptyFields)
			document.setBarcodeTemplateId(null);
	}

	private void setOcrTemplate(GUIDocument model, boolean ignoreEmptyFields, GUIDocument document) {
		if (model.getOcrTemplateId() != null)
			document.setOcrTemplateId(model.getOcrTemplateId());
		else if (!ignoreEmptyFields)
			document.setOcrTemplateId(null);
	}

	protected static void checkPublished(User user, Document doc) throws PermissionException {
		if (!user.isMemberOf(Group.GROUP_ADMIN) && !user.isMemberOf("publisher") && !doc.isPublishing())
			throw new PermissionException("Document not published");
	}

	@Override
	public GUIDocument deleteVersions(List<Long> ids) throws ServerException {
		Session session = validateSession();

		long docId = 0;
		DocumentManager manager = DocumentManager.get();
		for (long id : ids) {
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			Version version;
			try {
				version = manager.deleteVersion(id, transaction);
			} catch (Exception e) {
				return throwServerException(session, log, e);
			}
			docId = version.getDocId();
		}

		return getById(docId);
	}

	@Override
	public GUIDocument createWithContent(GUIDocument vo, String content, boolean checkout) throws ServerException {
		Session session = validateSession();

		try {
			FolderDAO fdao = FolderDAO.get();

			if (!fdao.isWriteAllowed(vo.getFolder().getId(), session.getUserId()))
				throw new PermissionException(session.getUsername(), "Folder " + vo.getFolder().getId(),
						Permission.WRITE);

			Document doc = toDocument(vo);
			doc.setId(0L);

			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			transaction.setEvent(DocumentEvent.STORED);
			Document document;
			if (StringUtils.isEmpty(content))
				document = DocumentManager.get()
						.create(IOUtils.toInputStream(" ", StandardCharsets.UTF_8), doc, transaction).getObject();
			else
				document = DocumentManager.get()
						.create(IOUtils.toInputStream(content, StandardCharsets.UTF_8), doc, transaction).getObject();

			if (checkout) {
				// Perform a checkout also
				transaction = new DocumentHistory();
				transaction.setSession(session);
				DocumentManager.get().checkout(document.getId(), transaction);
			}

			return fromDocument(document, vo.getFolder(), null);
		} catch (PermissionException | PersistenceException e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public void deleteFromTrash(List<Long> ids) throws ServerException {
		Session session = validateSession();
		if (ids.isEmpty())
			return;

		String idsStr = Arrays.asList(ids).toString().replace('[', '(').replace(']', ')');
		try {
			DocumentDAO.get().jdbcUpdate("update ld_document set ld_deleted=2 where ld_id in " + idsStr);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}

	}

	@Override
	public void emptyTrash() throws ServerException {
		Session session = validateSession();

		try {
			DocumentDAO.get().jdbcUpdate("update ld_document set ld_deleted=2 where ld_deleted=1 and  ld_deleteuserid="
					+ session.getUserId());

			FolderDAO.get().jdbcUpdate(
					"update ld_folder set ld_deleted=2 where ld_deleted=1 and  ld_deleteuserid=" + session.getUserId());
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}

	}

	@Override
	public void archiveDocuments(List<Long> docIds, String comment) throws ServerException {
		Session session = validateSession();

		DocumentHistory transaction = new DocumentHistory();
		transaction.setSession(session);
		try {
			DocumentManager.get().archiveDocuments(docIds.stream().collect(Collectors.toSet()), transaction);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public long archiveFolder(long folderId, String comment) throws ServerException {
		Session session = validateSession();

		DocumentHistory transaction = new DocumentHistory();
		transaction.setSession(session);
		transaction.setComment(HTMLSanitizer.sanitizeSimpleText(comment));
		try {
			return DocumentManager.get().archiveFolder(folderId, transaction);
		} catch (PersistenceException e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public void unarchiveDocuments(List<Long> docIds) throws ServerException {
		Session session = validateSession();

		DocumentDAO dao = DocumentDAO.get();

		for (long id : docIds) {
			// Create the document history event
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);

			try {
				dao.unarchive(id, transaction);
			} catch (PersistenceException e) {
				throwServerException(session, log, e);
			}
		}
	}

	@Override
	public long countDocuments(List<Long> folderIds, int status) throws ServerException {
		validateSession();
		long count = 0;
		for (long folderId : folderIds)
			count += countDocuments(folderId, status);
		return count;
	}

	private long countDocuments(long folderId, int status) {
		DocumentDAO dao = DocumentDAO.get();
		FolderDAO fdao = FolderDAO.get();

		List<Long> childrenFolderIds = fdao.findIdsByParentId(folderId);
		childrenFolderIds = new ArrayList<>(childrenFolderIds);
		childrenFolderIds.add(folderId);

		StringBuilder query = new StringBuilder(
				"select count(ld_id) from ld_document where ld_deleted=0 and ld_status=" + status);
		query.append(" and ld_folderid in (" + childrenFolderIds.toString().substring(1).replace("]", ")"));

		try {
			return dao.queryForLong(query.toString());
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return 0;
		}
	}

	@Override
	public List<String> setTicketPassword(long ticketId, String password) throws ServerException {
		Session session = validateSession();
		try {
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);

			TicketDAO tDao = TicketDAO.get();
			Ticket ticket = tDao.findById(ticketId);
			ticket.setDecodedPassword(password);
			if (password != null) {
				User dummy = new User();
				dummy.setId(-1);
				dummy.setUsername("dummy");
				dummy.setTenantId(transaction.getTenantId());
				dummy.setLocale(Locale.ENGLISH);
				setDecodedPassword(password, dummy);
				UserDAO.get().checkPasswordCompliance(dummy);
			}

			tDao.store(ticket, transaction);
			return new ArrayList<>();
		} catch (PasswordWeakException pwe) {
			return pwe.getMessages();
		} catch (PersistenceException | NoSuchAlgorithmException e) {
			return throwServerException(session, log, e);
		}
	}

	private void setDecodedPassword(String decodedPassword, User user) {
		try {
			user.setDecodedPassword(decodedPassword);
		} catch (NoSuchAlgorithmException e) {
			throw new PasswordWeakException(List.of(e.getMessage()));
		}
	}

	@Override
	public List<String> createTicket(long docId, int type, String suffix, Integer expireHours, Date expireDate,
			Integer maxDownloads, Integer maxViews, String password) throws ServerException {
		Session session = validateSession();

		String urlPrefix = "";
		HttpServletRequest request = this.getThreadLocalRequest();
		if (request != null)
			urlPrefix = request.getScheme() + "://" + request.getServerName() + ":" + request.getServerPort()
					+ request.getContextPath();

		DocumentHistory transaction = new DocumentHistory();
		transaction.setSession(session);
		try {
			Ticket ticket = new Ticket();
			ticket.setTenantId(session.getTenantId());
			ticket.setType(type);
			ticket.setDocId(docId);
			ticket.setSuffix(suffix);
			ticket.setExpireHours(expireHours);
			ticket.setExpired(expireDate);
			ticket.setMaxCount(maxDownloads);
			ticket.setMaxViews(maxViews);
			ticket.setDecodedPassword(password);

			ticket = DocumentManager.get().createTicket(ticket, transaction);

			List<String> result = new ArrayList<>();
			result.add(ticket.getTicketId());
			result.add(ticket.getUrl());
			result.add(
					new URI(ticket.getUrl().replace(urlPrefix, Context.get().getProperties().getProperty("server.url")))
							.normalize().toString());
			return result;
		} catch (PasswordWeakException pwe) {
			List<String> messages = pwe.getMessages();
			messages.add(0, "passwordweek");
			return messages;
		} catch (PermissionException | PersistenceException | URISyntaxException | NoSuchAlgorithmException e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public void setPassword(long docId, String password) throws ServerException {
		Session session = validateSession();

		DocumentDAO dao = DocumentDAO.get();

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSession(session);
		transaction.setComment("");

		try {
			Document doc = dao.findById(docId);
			checkPermission(Permission.PASSWORD, session.getUser(), doc.getFolder().getId());

			dao.setPassword(docId, password, transaction);
		} catch (AccessDeniedException | PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void unsetPassword(long docId, String currentPassword) throws ServerException {
		Session session = validateSession();

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSession(session);
		transaction.setComment("");

		DocumentDAO dao = DocumentDAO.get();

		try {
			Document doc = dao.findDocument(docId);
			if (session.getUser().isMemberOf(Group.GROUP_ADMIN) || doc.isGranted(currentPassword))
				dao.unsetPassword(docId, transaction);
			else
				throw new ServerException("You cannot access the document");
		} catch (PersistenceException | ServerException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public boolean unprotect(long docId, String password) throws ServerException {
		Session session = validateSession();
		return DocumentManager.get().unprotect(session.getSid(), docId, password);
	}

	@Override
	public String getContentAsString(long docId) throws ServerException {
		Session session = validateSession();

		try {
			DocumentDAO docDao = DocumentDAO.get();
			Document doc = docDao.findById(docId);
			if (doc == null)
				throw new ServerException(UNEXISTING_DOCUMENT);

			FolderDAO fDao = FolderDAO.get();
			if (!fDao.isDownloadllowed(doc.getFolder().getId(), session.getUserId()))
				throw new IOException("You don't have the DOWNLOAD permission");

			/*
			 * In case of alias we have to work on the real document
			 */
			if (doc.getDocRef() != null)
				doc = docDao.findById(doc.getDocRef());

			// Obtain the document's file stream
			return Store.get().getString(StoreResource.builder().document(doc).build());
		} catch (PersistenceException | ServerException | IOException e) {
			return throwServerException(session, log, e);
		}

	}

	@Override
	public GUIDocument checkinContent(long docId, String content) throws ServerException {
		Session session = validateSession();

		try {
			DocumentDAO docDao = DocumentDAO.get();
			Document doc = docDao.findDocument(docId);
			if (doc == null)
				throw new ServerException(UNEXISTING_DOCUMENT);

			docDao.isWriteAllowed(docId, session.getUserId());

			if (doc.getStatus() != DocumentStatus.CHECKEDOUT || doc.getLockUserId() != session.getUserId())
				throw new PermissionException("You have not checked out the file " + docId);

			DocumentHistory transaction = new DocumentHistory();
			transaction.setComment("Text content editing");
			transaction.setSession(session);

			DocumentManager.get().checkin(docId, IOUtils.toInputStream(content, StandardCharsets.UTF_8),
					doc.getFileName(), false, null, transaction);

			return getById(docId);
		} catch (PermissionException | PersistenceException | ServerException | IOException e) {
			return throwServerException(session, log, e);
		}

	}

	@Override
	public void replaceFile(long docId, String fileVersion, String comment) throws ServerException {
		Session session = validateSession();

		Map<String, File> uploadedFilesMap = getUploadedFiles(session.getSid());
		File file = uploadedFilesMap.values().iterator().next();
		if (file == null)
			return;

		try {
			DocumentDAO docDao = DocumentDAO.get();
			Document doc = docDao.findDocument(docId);
			if (doc == null)
				throw new ServerException(UNEXISTING_DOCUMENT);

			docDao.isWriteAllowed(docId, session.getUserId());

			if (doc.getStatus() != DocumentStatus.UNLOCKED)
				throw new IOException("The document is locked");

			DocumentHistory transaction = new DocumentHistory();
			transaction.setComment(HTMLSanitizer.sanitizeSimpleText(comment));
			transaction.setSession(session);

			DocumentManager.get().replaceFile(doc.getId(), fileVersion, file, transaction);

			UploadServlet.cleanUploads(session.getSid());
		} catch (PersistenceException | ServerException | IOException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public GUIDocument createDocument(GUIDocument document, String content) throws ServerException {
		Session session = validateSession();

		try {
			FolderDAO fDao = FolderDAO.get();
			if (!fDao.isWriteAllowed(document.getFolder().getId(), session.getUserId()))
				throw new IOException("You don't have the WRITE permission");

			DocumentHistory transaction = new DocumentHistory();
			transaction.setComment("Text content creation");
			transaction.setSession(session);

			Document doc = DocumentManager.get()
					.create(IOUtils.toInputStream(content, StandardCharsets.UTF_8), toDocument(document), transaction)
					.getObject();

			return getById(doc.getId());
		} catch (PersistenceException | IOException | ServerException e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public GUIRating getUserRating(long docId) throws ServerException {
		Session session = validateSession();

		RatingDAO rDao = RatingDAO.get();
		try {
			GUIRating rating = null;
			Rating rat = rDao.findByDocIdAndUserId(docId, session.getUserId());
			if (rat != null) {
				rDao.initialize(rat);
				rating = new GUIRating();
				rating.setId(rat.getId());
				rating.setDocId(docId);
				rating.setUserId(session.getUserId());
				rating.setUsername(session.getUser().getFullName());
				rating.setVote(rat.getVote());
				rating.setAverage(rat.getAverage());
			}

			return rating;
		} catch (PersistenceException e) {
			return throwServerException(session, log, e);
		}

	}

	@Override
	public Integer deleteRating(long id) throws ServerException {
		Session session = validateSession();

		try {
			RatingDAO rDao = RatingDAO.get();
			Rating rat = rDao.findById(id);
			if (rat == null)
				return 0;

			if (rat.getUserId() != session.getUserId())
				throw new ServerException("Cannot delete the rating left by another user");

			rDao.delete(id);

			return DocumentDAO.get().findById(rat.getDocId()).getRating();
		} catch (PersistenceException | ServerException e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public GUIDocument convert(long docId, String fileVersion, String format) throws ServerException {
		Session session = validateSession();

		try {
			DocumentDAO docDao = DocumentDAO.get();
			Document doc = docDao.findById(docId);
			if (doc == null)
				throw new ServerException(UNEXISTING_DOCUMENT);

			FolderDAO fDao = FolderDAO.get();
			if (!fDao.isWriteAllowed(doc.getFolder().getId(), session.getUserId()))
				throw new PermissionException(session.getUsername(), DOCUMENT_STR + doc.getId(), Permission.WRITE);
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);

			Document conversion = FormatConversionManager.get().convert(doc, fileVersion, format, transaction);
			if (conversion == null)
				throw new ServerException("Unable to convert");
			return getById(conversion.getId());
		} catch (Exception e) {
			return throwServerException(session, log, e);
		}

	}

	@Override
	public GUIEmail extractEmail(long docId, String fileVersion) throws ServerException {
		Session session = validateSession();

		DocumentDAO dao = DocumentDAO.get();
		Document emailDocument = null;
		try {
			emailDocument = dao.findDocument(docId);
			if (!emailDocument.getFileName().toLowerCase().endsWith(".eml")
					&& !emailDocument.getFileName().toLowerCase().endsWith(".msg"))
				throw new ServerException("Not an email file");
		} catch (PersistenceException e1) {
			return throwServerException(session, log, e1);
		}
		GUIDocument guiDocument = getById(docId);

		try (InputStream is = Store.get()
				.getStream(StoreResource.builder().document(emailDocument).fileVersion(fileVersion).build())) {
			GUIEmail guiMail = new GUIEmail();
			EMail email = readEmail(is, emailDocument.getId(), guiDocument);
			if (email != null) {
				if (email.getFrom() != null)
					guiMail.setFrom(new GUIContact(email.getFrom().getName(), null, email.getFrom().getAddress()));

				guiMail.setSent(email.getSentDate());
				guiMail.setReceived(
						email.getReceivedDate() != null ? email.getReceivedDate() : emailDocument.getCreation());
				guiMail.setSubject(email.getSubject());
				guiMail.setMessage(
						email.isHtml() ? HTMLSanitizer.sanitize(email.getMessageText()) : email.getMessageText());

				guiMail.setSigned(email.isSigned());

				setEmailRecipients(email, guiMail);

				setEmailAttachments(guiDocument, email, guiMail);
			}
			return guiMail;
		} catch (IOException | PersistenceException e1) {
			return throwServerException(session, log, e1);
		}
	}

	private EMail readEmail(InputStream is, long docId, GUIDocument emailDocument) {
		EMail email = null;
		try {
			if (emailDocument.getFileName().toLowerCase().endsWith(".eml"))
				email = MailUtil.messageToMail(is, true);
			else
				email = MailUtil.msgToMail(is, true);
		} catch (MessagingException | IOException | CMSException e) {
			log.warn("Cannot render the email document {}", docId);
		}
		return email;
	}

	private void setEmailAttachments(GUIDocument emailDocument, EMail email, GUIEmail guiMail) {
		List<GUIDocument> attachments = new ArrayList<>();
		for (int i = 1; i <= email.getAttachmentsCount(); i++) {
			EMailAttachment att = email.getAttachment(i);
			GUIDocument d = new GUIDocument();
			d.setFileName(att.getFileName());
			d.setFileSize(att.getSize());
			d.setIcon(IconSelector.selectIcon(att.getFileName()));
			d.setFolder(emailDocument.getFolder());
			attachments.add(d);
		}
		guiMail.setAttachments(attachments);
	}

	private void setEmailRecipients(EMail email, GUIEmail guiMail) {
		Set<Recipient> recipients = email.getRecipients();
		List<GUIContact> contacts = new ArrayList<>();
		for (Recipient rec : recipients)
			contacts.add(new GUIContact(rec.getName(), null, rec.getAddress()));
		guiMail.setTos(contacts);

		recipients = email.getRecipientsCC();
		contacts = new ArrayList<>();
		for (Recipient rec : recipients)
			contacts.add(new GUIContact(rec.getName(), null, rec.getAddress()));
		guiMail.setCcs(contacts);

		recipients = email.getRecipientsBCC();
		contacts = new ArrayList<>();
		for (Recipient rec : recipients)
			contacts.add(new GUIContact(rec.getName(), null, rec.getAddress()));
		guiMail.setBccs(contacts);

		recipients = email.getReplyTo();
		contacts = new ArrayList<>();
		for (Recipient rec : recipients)
			contacts.add(new GUIContact(rec.getName(), null, rec.getAddress()));
		guiMail.setReplyTo(contacts);
	}

	@Override
	public GUIDocument saveEmailAttachment(long docId, String fileVersion, String attachmentFileName)
			throws ServerException {
		Session session = validateSession();

		InputStream is = null;
		File tmp = null;
		try {
			tmp = FileUtil.createTempFile("attcopy", null);
			GUIDocument doc = getById(docId);
			if (!doc.getFileName().toLowerCase().endsWith(".eml") && !doc.getFileName().toLowerCase().endsWith(".msg"))
				throw new ServerException("Not an email file");
			checkPermission(Permission.WRITE, session.getUser(), doc.getFolder().getId());

			is = Store.get().getStream(StoreResource.builder().docId(docId).fileVersion(fileVersion).build());

			EMail email = MailUtil.messageToMail(is, true);
			EMailAttachment attachment = null;
			if (email.getAttachments().size() > 0) {
				for (EMailAttachment att : email.getAttachments().values()) {
					if (attachmentFileName.equals(att.getFileName())) {
						attachment = att;
						break;
					}
				}
			}

			if (attachment == null)
				throw new IOException("Attachment not found");

			FileUtils.writeByteArrayToFile(tmp, attachment.getData());

			Document docVO = new Document();
			docVO.setFileName(attachmentFileName);
			docVO.setFileSize(attachment.getSize());
			docVO.setFolder(FolderDAO.get().findById(doc.getFolder().getId()));

			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			Document d = DocumentManager.get().create(tmp, docVO, transaction).getObject();
			return getById(d.getId());
		} catch (IOException | PersistenceException | MessagingException e) {
			return throwServerException(session, log, e);
		} finally {
			IOUtils.closeQuietly(is);
			FileUtil.delete(tmp);
		}
	}

	@Override
	public GUIDocument replaceAlias(long aliasId) throws ServerException {
		Session session = validateSession();

		DocumentHistory transaction = new DocumentHistory();
		transaction.setSession(session);
		try {
			Document doc = DocumentManager.get().replaceAlias(aliasId, transaction).getObject();
			return getDocument(session, doc.getId());
		} catch (InvalidSessionServerException | PermissionException | PersistenceException e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public void deDuplicate(Long folderId, boolean retainNewest) throws ServerException {
		Session session = validateSession();
		checkMenu(getThreadLocalRequest(), Menu.REPORTS);

		try {
			// First of all, find all duplicates digests
			DocumentDAO docDao = DocumentDAO.get();
			List<String> digests = docDao.findDuplicatedDigests(session.getTenantId(), folderId);
			log.info("Found {} duplicated digests", digests.size());

			StringBuilder duplicationsQuery = new StringBuilder(
					"select ld_id, ld_digest, ld_date, ld_folderid, ld_filename, ld_version from ld_document where ld_deleted=0 ");
			duplicationsQuery.append(" and ld_tenantid = ");
			duplicationsQuery.append(Long.toString(session.getTenantId()));
			duplicationsQuery.append(" and ld_docref is null and ld_digest in ('");
			duplicationsQuery.append(String.join("','", digests));
			duplicationsQuery.append("') order by ld_digest asc, ld_date ");
			if (retainNewest)
				duplicationsQuery.append(" desc ");
			else
				duplicationsQuery.append(" asc ");

			FolderDAO folderDao = FolderDAO.get();

			List<Document> duplications = docDao.query(duplicationsQuery.toString(), new RowMapper<Document>() {
				public Document mapRow(ResultSet rs, int rowNum) throws SQLException {
					Document doc = new Document();
					doc.setTenantId(session.getTenantId());
					doc.setId(rs.getLong(1));
					doc.setDigest(rs.getString(2));
					doc.setDate(new Date(rs.getTimestamp(3).getTime()));
					doc.setFileName(rs.getString(5));
					doc.setVersion(rs.getString(6));

					Folder folder = new Folder();
					folder.setId(rs.getLong(4));
					folder.setTenantId(session.getTenantId());
					doc.setFolder(folder);
					try {
						folder.setPathExtended(folderDao.computePathExtended(folder.getId()));
					} catch (PersistenceException e) {
						log.error(e.getMessage(), e);
					}

					return doc;
				}
			}, null);
			log.info("Found {} duplicated files to deduplicate", duplications.size());

			List<Document> currentDuplications = new ArrayList<>();
			String currentDigest = null;
			for (Document doc : duplications) {
				if (currentDigest != null && !currentDigest.equals(doc.getDigest()))
					deduplicateDocuments(session, currentDuplications);
				currentDuplications.add(doc);
				currentDigest = doc.getDigest();
			}

			if (!currentDuplications.isEmpty())
				deduplicateDocuments(session, currentDuplications);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	private void deduplicateDocuments(Session session, List<Document> duplications) throws PersistenceException {
		DocumentDAO docDao = DocumentDAO.get();
		Document maintainedDoc = docDao.findById(duplications.get(0).getId());
		log.info("Process digest {}, retain document {} dated {}", maintainedDoc.getDigest(), maintainedDoc,
				maintainedDoc.getDate());

		duplications.remove(0);

		log.debug("Delete {} documents", duplications.size());
		for (Document doc : duplications) {
			if (doc.getId() == maintainedDoc.getId())
				continue;

			doc.setDeleted(1);

			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			transaction.setDocument(doc);
			transaction.setComment("Deleted by deduplication");
			transaction.setEvent(DocumentEvent.DELETED);
			transaction.setFilename(doc.getFileName());
			transaction.setVersion(doc.getVersion());
			transaction.setFileVersion(doc.getFileVersion());
			transaction.setPath(doc.getFolder().getPathExtended());
			docDao.delete(doc.getId(), transaction);
		}

		// Create the aliases
		FolderDAO fDao = FolderDAO.get();
		DocumentManager manager = DocumentManager.get();
		for (Document duplicate : duplications) {
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			transaction.setComment("Created by deduplication");
			Document alias = manager.createAlias(maintainedDoc, fDao.findById(duplicate.getFolder().getId()), null,
					transaction);
			log.info("Created new alias {}", alias);
		}

		duplications.clear();
	}

	@Override
	public void deleteTicket(long ticketId) throws ServerException {
		Session session = validateSession();

		TicketDAO dao = TicketDAO.get();
		try {
			dao.delete(ticketId);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void enableTicket(long ticketId) throws ServerException {
		Session session = validateSession();

		TicketDAO dao = TicketDAO.get();
		try {
			Ticket ticket = dao.findById(ticketId);
			ticket.setEnabled(true);
			dao.store(ticket);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void disableTicket(long ticketId) throws ServerException {
		Session session = validateSession();

		TicketDAO dao = TicketDAO.get();
		try {
			Ticket ticket = dao.findById(ticketId);
			ticket.setEnabled(false);
			dao.store(ticket);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void enforceFilesIntoFolderStore(long folderId) throws ServerException {
		Session session = validateSession();
		executeLongRunningOperation("Enforce Files Into Folder Store", () -> {
			User user = session.getUser();
			int movedFiles = 0;

			FolderDAO fDao = FolderDAO.get();
			String treePath = null;
			try {
				treePath = fDao.computePathExtended(folderId);
				DocumentHistory transaction = new DocumentHistory();
				transaction.setSession(session);
				movedFiles = DocumentManager.get().enforceFilesIntoFolderStore(folderId, transaction);

				log.info("Notify the move of {} files to the right store in the tree {}", movedFiles, treePath);

				notifyEnforcement(session, I18N.message("enforcementofstoragereport", user.getLocale(),
						new Object[] { movedFiles, treePath }));

			} catch (Exception t) {
				log.error("Error enforcing files store into tree {}", treePath);
				log.error(t.getMessage(), t);
				try {
					notifyEnforcement(session, I18N.message("enforcementofstorageerror", user.getLocale(),
							new Object[] { movedFiles, treePath, t.getMessage() }));
				} catch (Exception e) {
					log.warn(e.getMessage(), e);
				}
			}
			return null;
		}, session);
	}

	private void notifyEnforcement(Session session, String message) {
		User user = session.getUser();

		// Prepare the system message
		Recipient sysRecipient = new Recipient();
		sysRecipient.setName(user.getUsername());
		sysRecipient.setAddress(user.getEmail());
		sysRecipient.setType(Recipient.TYPE_SYSTEM);
		sysRecipient.setMode(MESSAGE);

		SystemMessage sys = new SystemMessage();
		sys.setTenantId(user.getTenantId());
		sys.setType(Message.TYPE_SYSTEM);
		sys.setHtml(false);
		sys.setAuthor("SYSTEM");
		sys.setSentDate(new Date());
		sys.setNotify(true);
		sys.getRecipients().add(sysRecipient);
		sys.setMessageText(message);
		sys.setSubject(I18N.message("enforcementofstorage", user.getLocale()));

		try {
			SystemMessageDAO sDao = SystemMessageDAO.get();
			sDao.store(sys);

			// Prepare the email
			Recipient emailRecipient = new Recipient();
			emailRecipient.setName(user.getUsername());
			emailRecipient.setAddress(user.getEmail());
			emailRecipient.setType(Recipient.TYPE_EMAIL);
			emailRecipient.setMode(Recipient.MODE_EMAIL_TO);
			emailRecipient.setRead(1);

			EMail mail = new EMail();
			mail.setHtml(false);
			mail.setTenantId(user.getTenantId());
			mail.setAccountId(-1);
			mail.setAuthor(user.getUsername());
			mail.setAuthorAddress(user.getEmail());
			mail.setFolder(OUTBOX);
			mail.setSentDate(new Date());
			mail.setUsername(user.getUsername());
			mail.getRecipients().add(emailRecipient);
			mail.setSubject(sys.getSubject());
			mail.setMessageText(message);

			EMailSender sender = getEmailSender(session);
			sender.send(mail);
		} catch (PersistenceException | MessagingException e) {
			log.warn(e.getMessage(), e);
		}
	}

	@Override
	public GUIDocument merge(List<Long> docIds, long targetFolderId, String fileName) throws ServerException {
		final Session session = validateSession();

		DocumentHistory transaction = new DocumentHistory();
		transaction.setSession(session);

		try {
			DocumentDAO docDao = DocumentDAO.get();
			List<Document> docs = new ArrayList<>();
			for (long docId : docIds)
				docs.add(docDao.findDocument(docId));

			Document doc = DocumentManager
					.get().merge(docs, targetFolderId,
							fileName.toLowerCase().endsWith(".pdf") ? fileName : fileName + ".pdf", transaction)
					.getObject();
			return getDocument(session, doc.getId());
		} catch (InvalidSessionServerException | PermissionException | PersistenceException | IOException e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public int updatePages(long docId) throws ServerException {
		final Session session = validateSession();

		DocumentDAO docDao = DocumentDAO.get();
		try {
			Document doc = docDao.findDocument(docId);
			if (doc != null) {
				docDao.initialize(doc);
				int pages = DocumentManager.get().countPages(doc);
				doc.setPages(pages);
				return pages;
			}
			return 1;
		} catch (PersistenceException e) {
			return throwServerException(session, log, e);
		}

	}

	@Override
	public GUIDocument rename(long documentId, String name) throws ServerException {
		final Session session = validateSession();

		User user = session.getUser();
		DocumentDAO docDao = DocumentDAO.get();
		try {
			Document doc = docDao.findById(documentId);
			checkPermission(Permission.RENAME, user, doc.getFolder().getId());
			checkPublished(user, doc);

			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			transaction.setUser(user);
			DocumentManager.get().rename(documentId, name, transaction);

			return getDocument(session, documentId);
		} catch (AccessDeniedException | PermissionException | InvalidSessionServerException | PersistenceException e) {
			return throwServerException(session, log, e);
		}
	}

	private String prepareTileAsString(Session session, Document doc) {
		String tile = null;
		File tileFile = null;
		try {
			tileFile = createTile(doc, session.getSid());
			tile = ImageUtil.encode(tileFile);
			if (tile != null)
				tile = "data:image/png;base64," + tile;
		} catch (IOException | PersistenceException e) {
			log.warn("Cannot generate tile of document {}", doc, e);
		} finally {
			FileUtil.delete(tileFile);
		}
		return tile;
	}

	/**
	 * Useful method for unit testing
	 * 
	 * @param emailSender The email sender to inject
	 */
	static void setEmailSender(EMailSender emailSender) {
		DocumentServiceImpl.emailSender = emailSender;
	}

	private int booleanToInt(boolean bool) {
		return bool ? 1 : 0;
	}

	@Override
	public void saveACL(GUIDocument guiDocument) throws ServerException {
		Session session = validateSession();

		DocumentDAO docDao = DocumentDAO.get();
		try {
			Document document = docDao.findById(guiDocument.getId());
			docDao.initialize(document);

			log.info("Applying {} security policies to document {}", guiDocument.getAccessControlList().size(),
					guiDocument.getId());

			Set<DocumentAccessControlEntry> acl = new HashSet<>();
			for (GUIAccessControlEntry guiAce : guiDocument.getAccessControlList()) {
				DocumentAccessControlEntry ace = new DocumentAccessControlEntry();
				ace.setGroupId(guiAce.getEntityId());
				ace.setRead(booleanToInt(guiAce.isRead()));
				ace.setPreview(booleanToInt(guiAce.isPreview()));
				ace.setPrint(booleanToInt(guiAce.isPrint()));
				ace.setWrite(booleanToInt(guiAce.isWrite()));
				ace.setSecurity(booleanToInt(guiAce.isSecurity()));
				ace.setImmutable(booleanToInt(guiAce.isImmutable()));
				ace.setDelete(booleanToInt(guiAce.isDelete()));
				ace.setRename(booleanToInt(guiAce.isRename()));
				ace.setArchive(booleanToInt(guiAce.isArchive()));
				ace.setWorkflow(booleanToInt(guiAce.isWorkflow()));
				ace.setSign(booleanToInt(guiAce.isSign()));
				ace.setDownload(booleanToInt(guiAce.isDownload()));
				ace.setCalendar(booleanToInt(guiAce.isCalendar()));
				ace.setSubscription(booleanToInt(guiAce.isSubscription()));
				ace.setPassword(booleanToInt(guiAce.isPassword()));
				ace.setMove(booleanToInt(guiAce.isMove()));
				ace.setEmail(booleanToInt(guiAce.isEmail()));
				ace.setAutomation(booleanToInt(guiAce.isAutomation()));
				ace.setReadingreq(booleanToInt(guiAce.isReadingreq()));
				ace.setCustomid(booleanToInt(guiAce.isCustomid()));
				ace.setRevision(booleanToInt(guiAce.isRevision()));
				acl.add(ace);
			}

			document.getAccessControlList().clear();
			document.getAccessControlList().addAll(acl);

			// Add a document history entry
			DocumentHistory history = new DocumentHistory();
			history.setEvent(DocumentEvent.PERMISSION);
			history.setSession(session);
			docDao.store(document, history);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void applyParentFolderSecurity(long docId) throws ServerException {
		Session session = validateSession();

		DocumentDAO docDao = DocumentDAO.get();
		try {
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			docDao.applyParentFolderSecurity(docId, transaction);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public GUIAccessControlEntry getAllowedPermissions(List<Long> docIds) throws ServerException {
		Session session = validateSession();

		try {
			Set<Permission> commonPermissions = Permission.all();
			if (!session.getUser().isAdmin()) {
				DocumentDAO docDao = DocumentDAO.get();
				for (long docId : docIds) {
					Set<Permission> docPermissions = docDao.getAllowedPermissions(docId, session.getUserId());
					for (Permission permission : Permission.all()) {
						if (!docPermissions.contains(permission))
							commonPermissions.remove(permission);
					}
				}
			}

			return new GUIAccessControlEntry(commonPermissions.stream().map(p -> p.name().toLowerCase())
					.collect(Collectors.toSet()).toArray(new String[0]));
		} catch (PersistenceException e) {
			return throwServerException(session, log, e);
		}
	}
}