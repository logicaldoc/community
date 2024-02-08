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
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.mail.MessagingException;
import javax.servlet.http.HttpServletRequest;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.bouncycastle.cms.CMSException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.automation.Automation;
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
import com.logicaldoc.core.conversion.FormatConverterManager;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Bookmark;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentLink;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.document.DocumentNote;
import com.logicaldoc.core.document.Rating;
import com.logicaldoc.core.document.Version;
import com.logicaldoc.core.document.dao.BookmarkDAO;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.document.dao.DocumentHistoryDAO;
import com.logicaldoc.core.document.dao.DocumentLinkDAO;
import com.logicaldoc.core.document.dao.DocumentNoteDAO;
import com.logicaldoc.core.document.dao.RatingDAO;
import com.logicaldoc.core.document.dao.VersionDAO;
import com.logicaldoc.core.document.thumbnail.ThumbnailManager;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.imaging.ImageUtil;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.metadata.validation.Validator;
import com.logicaldoc.core.parser.ParseException;
import com.logicaldoc.core.security.AccessControlEntry;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.store.Storer;
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
import com.logicaldoc.util.Context;
import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.util.MimeType;
import com.logicaldoc.util.StringUtil;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.html.HTMLSanitizer;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.web.UploadServlet;

/**
 * The document service for the operations on the documents done through the
 * GUI.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentServiceImpl extends AbstractRemoteService implements DocumentService {

	private static final String DOCUMENT_STR = "Document ";

	private static final String UNEXISTING_DOCUMENT = "Unexisting document";

	private static final String DOWNLOAD_TICKET = "downloadTicket";

	private static final String MESSAGE = "message";

	private static final String SMTP_USERASFROM = ".smtp.userasfrom";

	private static final String DOCUMENT = "document";

	private static final String OUTBOX = "outbox";

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(DocumentServiceImpl.class);

	// Useful method for mocking the email sender inside unit tests
	private static EMailSender emailSender;

	@Override
	public void addBookmarks(Long[] ids, int type) throws ServerException {
		Session session = validateSession();

		BookmarkDAO bookmarkDao = (BookmarkDAO) Context.get().getBean(BookmarkDAO.class);
		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		for (long id : ids) {
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

	private void index(Long[] docIds, Session session) throws PersistenceException, ParseException {
		if (docIds == null || docIds.length < 1)
			return;
		else if (log.isInfoEnabled())
			log.info("Indexing documents {}",
					Stream.of(docIds).map(id -> Long.toString(id)).collect(Collectors.joining(", ")));

		DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		for (Long id : docIds) {
			if (id != null) {
				DocumentHistory transaction = new DocumentHistory();
				transaction.setSession(session);
				documentManager.index(id, null, transaction);
			}
		}
	}

	@Override
	public void indexDocuments(Long[] docIds) throws ServerException {
		Session session = validateSession();

		executeLongRunningOperation("Index Documents", () -> {
			try {
				index(docIds, session);
			} catch (ParseException | PersistenceException e) {
				log.error(e.getMessage(), e);
			}
		}, session);
	}

	private void indexAddedDocs(List<Long> docIdsToIndex, final Session session)
			throws PersistenceException, ParseException {
		if (!docIdsToIndex.isEmpty())
			index(docIdsToIndex.toArray(new Long[0]), session);
	}

	@Override
	public GUIDocument[] addDocuments(boolean importZip, String charset, boolean immediateIndexing,
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
			} catch (ServerException | PersistenceException | ParseException | IOException e) {
				log.error(e.getMessage(), e);
			}
		}, session)) {
			return createdDocs.toArray(new GUIDocument[0]);
		} else {
			return new GUIDocument[0];
		}
	}

	private void addDocuments(boolean importZip, String charset, boolean immediateIndexing, final GUIDocument metadata,
			final Session session, List<GUIDocument> createdDocs)
			throws PersistenceException, ServerException, ParseException, IOException {

		checkWritePermission(metadata, session);

		Map<String, File> uploadedFilesMap = getUploadedFiles(session.getSid());

		List<Document> docs = new ArrayList<>();
		DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);

		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
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
					transaction.setEvent(DocumentEvent.STORED.toString());
					transaction.setComment(HTMLSanitizer.sanitizeSimpleText(metadata.getComment()));

					/*
					 * Prepare the Master document used to create the new one
					 */
					Document doc = toDocument(metadata);
					doc.setTenantId(session.getTenantId());
					doc.setCreation(new Date());
					doc.setFileName(filename);

					// Create the new document
					doc = documentManager.create(file, doc, transaction);

					if (immediateIndexing && doc.getIndexed() == AbstractDocument.INDEX_TO_INDEX)
						docIdsToIndex.add(doc.getId());

					createdDocs.add(fromDocument(doc, metadata.getFolder(), null));
					docs.add(doc);
				}
			} finally {
				FileUtil.strongDelete(file);
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
		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		if (!fdao.isWriteEnabled(metadata.getFolder().getId(), session.getUserId()))
			throw new ServerException("The user doesn't have the write permission on the current folder");
	}

	private void cleanUploadedFiles(Session session) {
		Map<String, File> uploadedFilesMap = getUploadedFiles(session.getSid());
		for (File uploadedEntry : uploadedFilesMap.values())
			FileUtil.strongDelete(uploadedEntry);
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
			FileUtil.strongDelete(zipFile);
		}
	}

	private void notifyUsersInNewThread(List<Document> docs, final GUIDocument metadata, final String templateName,
			final Session session) {
		if (metadata.getNotifyUsers() != null && metadata.getNotifyUsers().length > 0) {
			Thread notifier = new Thread(() -> notifyDocuments(docs, templateName, metadata.getNotifyMessage(),
					metadata.getNotifyUsers(), session));
			notifier.start();
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
		transaction.setEvent(DocumentEvent.CHECKEDIN.toString());
		transaction.setComment(HTMLSanitizer.sanitizeSimpleText(document.getComment()));

		Document doc;
		try {
			doc = retrieveDocument(document.getId());
		} catch (PersistenceException e1) {
			return (GUIDocument) throwServerException(session, log, e1);
		}

		// checkin the document; throws an exception if
		// something goes wrong
		DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		try (FileInputStream fis = new FileInputStream(file)) {
			documentManager.checkin(doc.getId(), fis, fileName, major, toDocument(document), transaction);
		} catch (IOException | PersistenceException e) {
			return (GUIDocument) throwServerException(session, log, e);
		}

		UploadServlet.cleanReceivedFiles(session.getSid());
		GUIDocument checkedInDocument = getById(doc.getId());

		/*
		 * We have to notify the specified users in a separate thread
		 */
		notifyUsersInNewThread(Arrays.asList(doc), document, "checkin", session);

		return checkedInDocument;
	}

	private void notifyDocuments(List<Document> docs, String messageTemplate, String notificationMessage,
			long[] recipientIds, Session session) {

		// Prepare the tile of the first uploaded file, by default we use a
		// single transparent pixel
		String tile = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==";
		try {
			tile = prepareTileAsString(session, docs.get(0));
		} catch (Exception e) {
			log.warn("Cannot prepare the tile image", e);
		}

		try {
			SystemMessageDAO systemMessageDao = (SystemMessageDAO) Context.get().getBean(SystemMessageDAO.class);

			Map<Locale, Set<Recipient>> emailRecipientsMap = new HashMap<>();
			Map<Locale, Set<Recipient>> systemRecipientsMap = new HashMap<>();
			prepareRecipients(recipientIds, emailRecipientsMap, systemRecipientsMap);

			for (Entry<Locale, Set<Recipient>> entry : emailRecipientsMap.entrySet()) {
				Locale locale = entry.getKey();
				Set<Recipient> recipients = entry.getValue();

				EMail mail = new EMail();
				mail.setHtml(1);
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

				MessageTemplateDAO tDao = (MessageTemplateDAO) Context.get().getBean(MessageTemplateDAO.class);
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

				EMailSender sender = getEmailSender(session);
				sender.send(mail);

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
				sys.setHtml(1);
				sys.setTenantId(mail.getTenantId());

				systemMessageDao.store(sys);
			}
		} catch (PersistenceException | MessagingException e) {
			log.warn(e.getMessage(), e);
		}
	}

	private Document retrieveDocument(long docId) throws PersistenceException {
		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		return dao.findDocument(docId);
	}

	private void prepareRecipients(long[] notifyUserids, Map<Locale, Set<Recipient>> emailRecipientsMap,
			Map<Locale, Set<Recipient>> systemRecipientsMap) throws PersistenceException {
		if (notifyUserids == null || notifyUserids.length < 1)
			return;

		String idsString = StringUtils.join(Arrays.stream(notifyUserids).boxed().toArray(Long[]::new), ",");
		UserDAO uDao = (UserDAO) Context.get().getBean(UserDAO.class);
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
	public GUIDocument[] addDocuments(String language, long folderId, boolean importZip, String charset,
			boolean immediateIndexing, final Long templateId) throws ServerException {
		Session session = validateSession();
		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		try {
			if (folderId == fdao.findRoot(session.getTenantId()).getId())
				throw new PermissionException("Cannot add documents in the root");

			GUIDocument metadata = new GUIDocument();
			metadata.setLanguage(language);
			metadata.setFolder(new GUIFolder(folderId));
			metadata.setTemplateId(templateId);
			return addDocuments(importZip, charset, immediateIndexing, metadata);
		} catch (PermissionException | PersistenceException | ServerException e) {
			return (GUIDocument[]) throwServerException(session, log, e);
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

			FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			if (!fDao.isWriteEnabled(doc.getFolder().getId(), session.getUserId()))
				throw new PermissionException(session.getUsername(), DOCUMENT_STR + docId, Permission.WRITE);

			if (doc.getStatus() != AbstractDocument.DOC_UNLOCKED)
				throw new PermissionException("The document " + docId + " is locked");

			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			transaction.setDocument(doc);

			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			manager.promoteVersion(doc.getId(), version, transaction);

			return getById(doc.getId());
		} catch (PersistenceException | ServerException | IOException | PermissionException e) {
			return (GUIDocument) throwServerException(session, log, e);
		}
	}

	@Override
	public void checkout(Long[] docIds) throws ServerException {
		Session session = validateSession();

		// Checkout the document; throws an exception if something
		// goes wrong
		DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

		try {
			// Create the document history event
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			transaction.setEvent(DocumentEvent.CHECKEDOUT.toString());
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
	public void lock(Long[] docIds, String comment) throws ServerException {
		Session session = validateSession();

		// Unlock the document; throws an exception if something
		// goes wrong
		DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSession(session);
		transaction.setEvent(DocumentEvent.LOCKED.toString());
		transaction.setComment(HTMLSanitizer.sanitizeSimpleText(comment));

		try {
			for (long id : docIds) {
				Document doc = dao.findDocument(id);
				if (doc != null)
					documentManager.lock(doc.getId(), AbstractDocument.DOC_LOCKED, new DocumentHistory(transaction));
			}
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void delete(Long[] ids) throws ServerException {
		Session session = validateSession();

		if (ids.length > 0) {
			for (long docId : ids) {
				try {
					deleteDocument(docId, session);
				} catch (PersistenceException e) {
					throwServerException(session, log, e);
				}
			}
		}
	}

	private void deleteDocument(long docId, Session session) throws PersistenceException {
		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = dao.findById(docId);
		if (doc == null)
			return;

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSession(session);
		transaction.setEvent(DocumentEvent.DELETED.toString());
		transaction.setComment("");

		// If it is a shortcut, we delete only the shortcut
		if (doc.getDocRef() != null
				|| (doc.getImmutable() == 1 && !transaction.getUser().isMemberOf(Group.GROUP_ADMIN))) {
			transaction.setEvent(DocumentEvent.SHORTCUT_DELETED.toString());
			dao.delete(doc.getId(), transaction);
			return;
		}

		// The document of the selected documentRecord must be
		// not immutable
		if (doc.getImmutable() == 1 && !transaction.getUser().isMemberOf(Group.GROUP_ADMIN)) {
			log.debug("Document {} was not deleted because immutable", docId);
			return;
		}

		// The document must be not locked
		if (doc.getStatus() == AbstractDocument.DOC_LOCKED) {
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
	public void deleteBookmarks(long[] bookmarkIds) throws ServerException {
		validateSession();
		BookmarkDAO dao = (BookmarkDAO) Context.get().getBean(BookmarkDAO.class);
		for (long id : bookmarkIds) {
			try {
				dao.delete(id);
			} catch (Exception e) {
				throw new ServerException("Bookmarks have not been deleted", e);
			}
		}
	}

	@Override
	public void deleteLinks(long[] ids) throws ServerException {
		validateSession();

		DocumentLinkDAO dao = (DocumentLinkDAO) Context.get().getBean(DocumentLinkDAO.class);
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
			return (GUIDocument) throwServerException(session, log, e);
		}
	}

	public GUIDocument getDocument(Session session, long docId)
			throws InvalidSessionServerException, PersistenceException, PermissionException {
		if (session != null)
			validateSession(session.getSid());

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document document = docDao.findById(docId);

		GUIDocument guiDocument = null;
		GUIFolder folder = null;

		if (document != null) {
			FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			fDao.initialize(document.getFolder());
			folder = new FolderServiceImpl().fromFolder(document.getFolder(), false);

			if (session != null)
				checkPublished(session.getUser(), document);

			docDao.initialize(document);

			guiDocument = fromDocument(document, folder, session != null ? session.getUser() : null);

			setAllowedPermissions(session, docId, guiDocument);

			if (session != null && folder != null) {
				FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
				Set<Permission> permissions = fdao.getEnabledPermissions(document.getFolder().getId(),
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
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		if (doc.getId() != 0L && !isFolder)
			docDao.initialize(doc);

		Document realDoc = doc;

		GUIDocument document = new GUIDocument();
		document.setId(doc.getId());
		document.setDocRef(doc.getDocRef());
		document.setTenantId(doc.getTenantId());

		if (!isFolder && doc.getDocRef() != null && doc.getDocRef().longValue() != 0) {
			realDoc = docDao.findById(doc.getDocRef());
			docDao.initialize(realDoc);
			document.setDocRef(doc.getDocRef());
			document.setDocRefType(doc.getDocRefType());
		}

		document.setCustomId(realDoc.getCustomId());
		if (!realDoc.getTags().isEmpty())
			document.setTags(realDoc.getTagsAsWords().toArray(new String[realDoc.getTags().size()]));
		else
			document.setTags(new String[0]);
		document.setType(doc.getType());
		document.setFileName(doc.getFileName());
		document.setColor(doc.getColor());
		document.setVersion(realDoc.getVersion());
		document.setCreation(realDoc.getCreation());
		document.setCreator(realDoc.getCreator());
		document.setCreatorId(realDoc.getCreatorId());
		document.setDate(realDoc.getDate());
		document.setPublisher(realDoc.getPublisher());
		document.setPublisherId(realDoc.getPublisherId());
		document.setFileVersion(realDoc.getFileVersion());
		document.setLanguage(realDoc.getLanguage());
		document.setTemplateId(realDoc.getTemplateId());
		document.setLastModified(realDoc.getLastModified());
		document.setLockUserId(realDoc.getLockUserId());
		document.setLockUser(realDoc.getLockUser());
		document.setComment(realDoc.getComment());
		document.setStatus(realDoc.getStatus());
		document.setWorkflowStatus(realDoc.getWorkflowStatus());
		document.setWorkflowStatusDisplay(realDoc.getWorkflowStatusDisplay());
		document.setImmutable(realDoc.getImmutable());
		document.setFileSize(realDoc.getFileSize());
		document.setStartPublishing(realDoc.getStartPublishing());
		document.setStopPublishing(realDoc.getStopPublishing());
		document.setPublished(realDoc.getPublished());
		document.setSigned(realDoc.getSigned());
		document.setStamped(realDoc.getStamped());
		document.setIndexed(realDoc.getIndexed());
		document.setExtResId(realDoc.getExtResId());
		document.setPages(realDoc.getPages());
		document.setPreviewPages(realDoc.getPreviewPages());
		document.setNature(realDoc.getNature());
		document.setFormId(realDoc.getFormId());
		document.setIcon(FileUtil.getBaseName(doc.getIcon()));
		document.setPasswordProtected(realDoc.isPasswordProtected());
		document.setLinks(realDoc.getLinks());
		document.setDocAttrs(realDoc.getDocAttrs());
		document.setOcrd(realDoc.getOcrd());
		document.setOcrTemplateId(realDoc.getOcrTemplateId());
		document.setBarcoded(realDoc.getBarcoded());
		document.setBarcodeTemplateId(realDoc.getBarcodeTemplateId());

		if (realDoc.getRating() != null)
			document.setRating(realDoc.getRating());

		if (realDoc.getCustomId() != null)
			document.setCustomId(realDoc.getCustomId());
		else
			document.setCustomId("");

		if (realDoc.getTemplate() != null) {
			document.setTemplate(realDoc.getTemplate().getName());
			document.setTemplateId(realDoc.getTemplate().getId());
		}

		setBookmarked(document, isFolder, sessionUser);

		GUIAttribute[] attributes = new TemplateServiceImpl().prepareGUIAttributes(realDoc.getTemplate(), realDoc,
				sessionUser);
		document.setAttributes(attributes);

		if (folder != null) {
			document.setFolder(folder);
		} else {
			GUIFolder f = new GUIFolder(doc.getFolder().getId());
			f.setName(doc.getFolder().getName());
			document.setFolder(f);
		}

		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		document.setPathExtended(fdao.computePathExtended(document.getFolder().getId()));

		return document;
	}

	private void setBookmarked(GUIDocument document, boolean isFolder, User sessionUser) {
		if (sessionUser != null && !isFolder) {
			BookmarkDAO bDao = (BookmarkDAO) Context.get().getBean(BookmarkDAO.class);
			document.setBookmarked(bDao.isDocBookmarkedByUser(document.getId(), sessionUser.getId()));
			if (document.getDocRef() != null)
				document.setBookmarked(bDao.isDocBookmarkedByUser(document.getDocRef(), sessionUser.getId()));
		}
	}

	@Override
	public GUIVersion[] getVersionsById(long id1, long id2) throws ServerException {
		Session session = validateSession();

		VersionDAO versDao = (VersionDAO) Context.get().getBean(VersionDAO.class);
		Version docVersion;
		try {
			docVersion = versDao.findById(id1);
		} catch (PersistenceException e) {
			return (GUIVersion[]) throwServerException(session, null, e);
		}
		if (docVersion != null)
			versDao.initialize(docVersion);

		GUIVersion version1 = null;
		if (docVersion != null) {
			version1 = new GUIVersion();
			version1.setDocId(docVersion.getDocId());
			version1.setUsername(docVersion.getUsername());
			version1.setComment(docVersion.getComment());
			version1.setId(id1);
			version1.setCustomId(docVersion.getCustomId());
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
			if (docVersion.getRating() != null)
				version1.setRating(docVersion.getRating());
			version1.setStartPublishing(docVersion.getStartPublishing());
			version1.setStopPublishing(docVersion.getStopPublishing());
			version1.setPublished(docVersion.getPublished());
			version1.setPages(docVersion.getPages());
			version1.setOcrd(docVersion.getOcrd());
			version1.setOcrTemplateId(docVersion.getOcrTemplateId());

			setGUIExtendedAttributes(docVersion, version1);

			GUIFolder folder1 = new GUIFolder();
			folder1.setName(docVersion.getFolderName());
			folder1.setId(docVersion.getFolderId());
			version1.setFolder(folder1);
		}

		try {
			docVersion = versDao.findById(id2);
		} catch (PersistenceException e) {
			return (GUIVersion[]) throwServerException(session, null, e);
		}
		if (docVersion != null)
			versDao.initialize(docVersion);

		GUIVersion version2 = null;
		if (docVersion != null) {
			version2 = new GUIVersion();
			version2.setDocId(docVersion.getDocId());
			version2.setUsername(docVersion.getUsername());
			version2.setComment(docVersion.getComment());
			version2.setId(id1);
			version2.setCustomId(docVersion.getCustomId());
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
			if (docVersion.getRating() != null)
				version2.setRating(docVersion.getRating());
			version2.setWorkflowStatus(docVersion.getWorkflowStatus());
			version2.setColor(docVersion.getColor());
			version2.setStartPublishing(docVersion.getStartPublishing());
			version2.setStopPublishing(docVersion.getStopPublishing());
			version2.setPublished(docVersion.getPublished());
			version2.setPages(docVersion.getPages());
			version2.setOcrd(docVersion.getOcrd());
			version2.setOcrTemplateId(docVersion.getOcrTemplateId());
			version2.setBarcodeTemplateId(docVersion.getBarcodeTemplateId());

			setGUIExtendedAttributes(docVersion, version2);

			GUIFolder folder2 = new GUIFolder();
			folder2.setName(docVersion.getFolderName());
			folder2.setId(docVersion.getFolderId());
			version2.setFolder(folder2);
		}

		GUIVersion[] versions = null;
		if (version1 != null && version2 != null) {
			versions = new GUIVersion[2];
			versions[0] = version1;
			versions[1] = version2;
		} else if (version1 != null) {
			versions = new GUIVersion[1];
			versions[0] = version1;
		} else if (version2 != null) {
			versions = new GUIVersion[1];
			versions[0] = version2;
		} else
			return new GUIVersion[0];

		return versions;
	}

	private void setGUIExtendedAttributes(Version docVersion, GUIVersion guiVersion) {
		VersionDAO versDao = (VersionDAO) Context.get().getBean(VersionDAO.class);
		guiVersion.setTemplate(docVersion.getTemplateName());
		guiVersion.setTemplateId(docVersion.getTemplateId());
		versDao.initialize(docVersion);
		for (String attrName : docVersion.getAttributeNames()) {
			Attribute extAttr = docVersion.getAttributes().get(attrName);
			GUIAttribute att = new GUIAttribute();
			att.setName(attrName);
			att.setSetId(extAttr.getSetId());
			att.setPosition(extAttr.getPosition());
			att.setLabel(extAttr.getLabel());
			att.setMandatory(extAttr.getMandatory() == 1);
			att.setHidden(extAttr.getHidden() == 1);
			att.setReadonly(extAttr.getReadonly() == 1);
			att.setMultiple(extAttr.getMultiple() == 1);
			att.setParent(extAttr.getParent());
			att.setStringValues(extAttr.getStringValues());
			att.setEditor(extAttr.getEditor());
			att.setStringValue(extAttr.getStringValue());
			att.setIntValue(extAttr.getIntValue());
			att.setBooleanValue(extAttr.getBooleanValue());
			att.setDoubleValue(extAttr.getDoubleValue());
			att.setType(extAttr.getType());
			guiVersion.addAttribute(att);
		}
	}

	@Override
	public void linkDocuments(Long[] inDocIds, Long[] outDocIds) throws ServerException {
		Session session = validateSession();

		DocumentLinkDAO linkDao = (DocumentLinkDAO) Context.get().getBean(DocumentLinkDAO.class);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		if (outDocIds.length > 0) {

			for (int i = 0; i < inDocIds.length; i++) {
				for (int j = 0; j < outDocIds.length; j++) {
					DocumentLink link = linkDao.findByDocIdsAndType(inDocIds[i], outDocIds[j], "default");
					if (link == null) {
						// The link doesn't exist and must be created
						link = new DocumentLink();
						link.setTenantId(session.getTenantId());
						try {
							link.setDocument1(docDao.findById(inDocIds[i]));
							link.setDocument2(docDao.findById(outDocIds[j]));
							link.setType("default");
							linkDao.store(link);
						} catch (PersistenceException e) {
							throwServerException(session, log, e);
						}
					}
				}
			}

		}
	}

	@Override
	public void makeImmutable(Long[] docIds, String comment) throws ServerException {
		Session session = validateSession();

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		try {
			for (long id : docIds) {
				Document doc = docDao.findById(id);

				if (doc.getImmutable() == 0) {
					// The document of the selected documentRecord must be
					// not locked
					if (doc.getStatus() != AbstractDocument.DOC_UNLOCKED) {
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
		DocumentHistoryDAO dao = (DocumentHistoryDAO) Context.get().getBean(DocumentHistoryDAO.class);
		dao.markHistoriesAsRead(event, session.getUserId());
	}

	@Override
	public void markIndexable(Long[] docIds, int policy) throws ServerException {
		Session session = validateSession();

		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		for (long id : docIds)
			try {
				manager.changeIndexingStatus(docDao.findById(id), policy);
			} catch (PersistenceException e) {
				throwServerException(session, log, e);
			}

	}

	@Override
	public void markUnindexable(Long[] docIds) throws ServerException {
		Session session = validateSession();

		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		for (long id : docIds)
			try {
				manager.changeIndexingStatus(docDao.findById(id), AbstractDocument.INDEX_SKIP);
			} catch (PersistenceException e) {
				throwServerException(session, log, e);
			}
	}

	@Override
	public void restore(Long[] docIds, long folderId) throws ServerException {
		Session session = validateSession();

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

		for (Long docId : docIds) {
			if (docId == null)
				continue;
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);

			try {
				docDao.restore(docId, folderId, transaction);
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
			transaction.setEvent(
					document.getId() == 0L ? DocumentEvent.CHANGED.toString() : DocumentEvent.STORED.toString());
			transaction.setComment(HTMLSanitizer.sanitizeSimpleText(document.getComment()));

			Validator validator = new Validator();
			validator.validate(object, object.getTemplate(), transaction);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}

	}

	@Override
	public GUIDocument save(GUIDocument guiDocument) throws ServerException {
		Session session = validateSession();
		if (guiDocument.getId() == 0L)
			return null;

		try {
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
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

			Document docVO = toDocument(guiDocument);
			if (guiDocument.getDocRef() != null) {
				docVO.setDocRef(null);
				docVO.setFileName(document.getFileName());
				docVO.setColor(document.getColor());
				docVO.setType(FileUtil.getExtension(document.getFileName()).toLowerCase());
			}
			docVO.setTenantId(session.getTenantId());

			// Fix the name of multiple attributes
			if (guiDocument.getAttributes() != null)
				for (GUIAttribute att : guiDocument.getAttributes()) {
					if (att.isMultiple()) {
						NumberFormat nf = new DecimalFormat("0000");
						List<GUIAttribute> values = guiDocument.getValues(att.getName());
						values.remove(0);
						int index = 1;
						for (GUIAttribute val : values) {
							val.setName(att.getName() + "-" + nf.format(index++));
						}
					}
				}

			// Create the document history event
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			transaction.setEvent(DocumentEvent.CHANGED.toString());
			transaction.setComment(HTMLSanitizer.sanitizeSimpleText(guiDocument.getComment()));

			DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			documentManager.update(document, docVO, transaction);
			return getById(guiDocument.getId());
		} catch (PersistenceException | ServerException e) {
			return (GUIDocument) throwServerException(session, log, e);
		}

	}

	private static void setAllowedPermissions(Session session, long documentId, GUIDocument guiDocument)
			throws PersistenceException {
		if (session != null) {
			DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Set<Permission> permissions = dao.getEnabledPermissions(documentId, session.getUserId());
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
		if (guiDocument.getTags() != null && guiDocument.getTags().length > 0)
			docVO.setTagsFromWords(new HashSet<>(Arrays.asList(guiDocument.getTags())));

		docVO.setCustomId(HTMLSanitizer.sanitizeSimpleText(guiDocument.getCustomId()));
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
		docVO.setPublished(guiDocument.getPublished());
		docVO.setBarcoded(guiDocument.getBarcoded());
		docVO.setExtResId(guiDocument.getExtResId());
		docVO.setPages(guiDocument.getPages());
		docVO.setPreviewPages(guiDocument.getPreviewPages());
		docVO.setNature(guiDocument.getNature());
		docVO.setFormId(guiDocument.getFormId());
		docVO.setOcrTemplateId(guiDocument.getOcrTemplateId());
		docVO.setBarcodeTemplateId(guiDocument.getBarcodeTemplateId());

		if (guiDocument.getTemplateId() != null) {
			docVO.setTemplateId(guiDocument.getTemplateId());
			TemplateDAO templateDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
			Template template = templateDao.findById(guiDocument.getTemplateId());
			templateDao.initialize(template);
			docVO.setTemplate(template);

			if (guiDocument.getAttributes() != null && guiDocument.getAttributes().length > 0) {
				toAttributes(guiDocument, docVO, template);
			}
		}

		docVO.setStatus(guiDocument.getStatus());
		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
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
			extAttr.setMandatory(templateAttribute.getMandatory());
			extAttr.setHidden(templateAttribute.getHidden());
			extAttr.setStringValues(attr.getStringValues());
			if (attr.getParent() == null)
				extAttr.setMultiple(templateAttribute.getMultiple());
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
		DocumentDAO documentDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

		EMail mail = new EMail();
		mail.setHtml(1);
		mail.setTenantId(session.getTenantId());
		mail.setAccountId(-1);
		mail.setFolder(OUTBOX);
		mail.setSentDate(new Date());
		mail.setUsername(session.getUsername());

		setAuthorAddress(mail, guiMail, session);

		setRecipients(mail, guiMail);

		setReceipientsCC(mail, guiMail);

		setRecipientsBCC(mail, guiMail);

		List<Document> attachedDocs = documentDao.findByIds(guiMail.getDocIds(), null);
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

				Document doc = documentDao.findDocument(guiMail.getDocIds()[0]);

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
		} catch (PermissionException | PersistenceException | IOException e) {
			log.warn(e.getMessage(), e);
			return "error";
		}
	}

	private void setRecipientsBCC(EMail mail, GUIEmail guiMail) {
		if (guiMail.getBccs() != null && guiMail.getBccs().length > 0) {
			StringBuilder sb = new StringBuilder();
			for (GUIContact contact : guiMail.getBccs()) {
				if (sb.length() > 0)
					sb.append(",");
				sb.append(contact.getEmail());
			}
			mail.parseRecipientsBCC(sb.toString());
		}
	}

	private void setReceipientsCC(EMail mail, GUIEmail guiMail) {
		if (guiMail.getCcs() != null && guiMail.getCcs().length > 0) {
			StringBuilder sb = new StringBuilder();
			for (GUIContact contact : guiMail.getCcs()) {
				if (sb.length() > 0)
					sb.append(",");
				sb.append(contact.getEmail());
			}
			mail.parseRecipientsCC(sb.toString());
		}
	}

	private void setRecipients(EMail mail, GUIEmail guiMail) {
		if (guiMail.getTos() != null && guiMail.getTos().length > 0) {
			StringBuilder sb = new StringBuilder();
			for (GUIContact contact : guiMail.getTos()) {
				if (sb.length() > 0)
					sb.append(",");
				sb.append(contact.getEmail());
			}
			mail.parseRecipients(sb.toString());
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

	private void prepareZipAttachment(EMail mail, Long[] docIds, boolean pdfConversion, Session session)
			throws IOException {
		/*
		 * Create a temporary archive for sending it as unique attachment
		 */
		File zipFile = FileUtil.createTempFile("email", "zip");
		try (OutputStream out = new FileOutputStream(zipFile);) {
			// Create the document history event
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			transaction.setEvent(DocumentEvent.DOWNLOADED.toString());

			ZipExport export = new ZipExport();
			export.process(docIds, out, pdfConversion, transaction);
			createAttachment(mail, zipFile);
		} catch (IOException | PersistenceException t) {
			log.error(t.getMessage(), t);
		} finally {
			FileUtil.strongDelete(zipFile);
		}
	}

	private void writeMessageWithThumbnail(EMail mail, Document doc, String message, Session session) {
		File thumbnailFile = null;
		try {
			thumbnailFile = createTile(doc, session.getSid());
			if (thumbnailFile != null && thumbnailFile.length() > 0) {
				message += "<p><img src='data:image/png;base64," + ImageUtil.encodeImage(thumbnailFile) + "'/></p>";
			}
			mail.setMessageText("<html><head><meta charset='utf-8' /></head><body>" + message + "<rl /></body></html>");
		} catch (IOException ioe) {
			log.warn(ioe.getMessage());
		} finally {
			FileUtil.strongDelete(thumbnailFile);
		}
	}

	private void prepareDownloadTicket(GUIEmail email, String locale, Session session, Map<String, Object> dictionary)
			throws PersistenceException, PermissionException {
		if (email.isSendAsTicket()) {
			DocumentDAO documentDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);

			// Prepare a new download ticket
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);

			Document doc = documentDao.findDocument(email.getDocIds()[0]);

			Ticket ticket = new Ticket();
			ticket.setTenantId(session.getTenantId());
			ticket.setType(Ticket.DOWNLOAD);
			ticket.setDocId(email.getDocIds()[0]);

			ticket = manager.createTicket(ticket, transaction);
			String ticketDiv = "<div style='margin-top:10px; border-top:1px solid black; background-color:#CCCCCC;'><b>&nbsp;"
					+ I18N.message("clicktodownload", LocaleUtil.toLocale(locale)) + ": <a href='" + ticket.getUrl()
					+ "'>" + doc.getFileName() + "</a></b></div>";
			dictionary.put(DOWNLOAD_TICKET, ticketDiv);
		}
	}

	private String sendEmail(EMail mail, Session session, List<Document> attachedDocs) {
		try {
			DocumentDAO documentDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

			// Send the message
			EMailSender sender = getEmailSender(session);
			sender.send(mail);

			FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			for (Document d : attachedDocs) {
				Document doc = d;
				if (doc.getDocRef() != null)
					doc = documentDao.findById(doc.getDocRef());

				// Create the document history event
				DocumentHistory history = new DocumentHistory();
				history.setSession(session);
				history.setDocument(doc);
				history.setEvent(DocumentEvent.SENT.toString());
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
			ContactDAO cdao = (ContactDAO) Context.get().getBean(ContactDAO.class);
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
			return "error";
		}
	}

	private static EMailSender getEmailSender(Session session) {
		if (emailSender != null) {
			emailSender.setTenant(session.getTenantId());
			return emailSender;
		} else
			return new EMailSender(session.getTenantName());
	}

	private File createTile(Document doc, String sid) throws IOException {
		Storer storer = (Storer) Context.get().getBean(Storer.class);
		String tileResource = storer.getResourceName(doc, doc.getFileVersion(), ThumbnailManager.SUFFIX_TILE);

		// In any case try to produce the thumbnail
		if (storer.size(doc.getId(), tileResource) <= 0L) {
			ThumbnailManager thumbManager = (ThumbnailManager) Context.get().getBean(ThumbnailManager.class);
			try {
				thumbManager.createTile(doc, doc.getFileVersion(), sid);
			} catch (IOException e) {
				log.error(e.getMessage(), e);
			}
		}

		if (storer.exists(doc.getId(), tileResource)) {
			File file = FileUtil.createTempFile("tile-", ".png");
			storer.writeToFile(doc.getId(), tileResource, file);
			return file;
		}

		return null;
	}

	private void createAttachment(EMail email, long docId, boolean pdfConversion, String sid)
			throws IOException, PersistenceException {
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Storer storer = (Storer) Context.get().getBean(Storer.class);
		Document doc = docDao.findDocument(docId);
		String resource = storer.getResourceName(doc, null, null);

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
				FormatConverterManager manager = (FormatConverterManager) Context.get()
						.getBean(FormatConverterManager.class);
				manager.convertToPdf(doc, sid);
				resource = storer.getResourceName(doc, null, "conversion.pdf");
			}
			att.setMimeType(MimeType.get("pdf"));
			att.setFileName(FileUtil.getBaseName(doc.getFileName()) + ".pdf");
		}

		att.setData(storer.getBytes(doc.getId(), resource));

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
	public void unlock(Long[] docIds) throws ServerException {
		Session session = validateSession();

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSession(session);

		try {
			// Unlock the document; throws an exception if something
			// goes wrong
			DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
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

		BookmarkDAO bookmarkDao = (BookmarkDAO) Context.get().getBean(BookmarkDAO.class);
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

		DocumentLinkDAO dao = (DocumentLinkDAO) Context.get().getBean(DocumentLinkDAO.class);
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
		UploadServlet.cleanReceivedFiles(session.getSid());

		File dir = new File(System.getProperty("java.io.tmpdir") + "/upload/" + session.getSid());
		if (dir.exists())
			FileUtil.strongDelete(dir);
	}

	@Override
	public GUIRating getRating(long docId) throws ServerException {
		Session session = validateSession();

		RatingDAO ratingDao = (RatingDAO) Context.get().getBean(RatingDAO.class);

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
	}

	@Override
	public int saveRating(GUIRating rating) throws ServerException {
		Session session = validateSession();

		RatingDAO ratingDao = (RatingDAO) Context.get().getBean(RatingDAO.class);
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
		try {
			ratingDao.store(rat, transaction);

			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document doc = docDao.findById(rating.getDocId());
			return doc.getRating();
		} catch (PersistenceException e) {
			return (Integer) throwServerException(session, log, e);
		}
	}

	@Override
	public long addNote(long docId, String message) throws ServerException {
		Session session = validateSession();

		try {
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document document = docDao.findDocument(docId);
			if (document == null)
				throw new ServerException(UNEXISTING_DOCUMENT + " " + docId);

			DocumentNote note = new DocumentNote();
			note.setTenantId(session.getTenantId());
			note.setDocId(document.getId());
			note.setUserId(session.getUserId());
			note.setUsername(session.getUser().getFullName());
			note.setDate(new Date());
			note.setMessage(message);
			note.setFileName(document.getFileName());
			note.setFileVersion(document.getFileVersion());

			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);

			DocumentNoteDAO dao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);
			dao.store(note, transaction);

			return note.getId();
		} catch (PersistenceException | ServerException e) {
			return (Long) throwServerException(session, log, e);
		}
	}

	@Override
	public GUIDocumentNote[] getNotes(long docId, String fileVersion, Collection<String> types) throws ServerException {
		Session session = validateSession();

		Document document;
		try {
			document = retrieveDocument(docId);
		} catch (PersistenceException e) {
			return (GUIDocumentNote[]) throwServerException(session, log, e);
		}

		if (document == null)
			throw new ServerException(UNEXISTING_DOCUMENT + " " + docId);

		List<GUIDocumentNote> guiNotes = new ArrayList<>();
		DocumentNoteDAO dao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);

		List<DocumentNote> notes = dao.findByDocIdAndTypes(document.getId(),
				fileVersion != null ? fileVersion : document.getFileVersion(), types);
		for (DocumentNote note : notes) {
			GUIDocumentNote guiNote = new GUIDocumentNote();
			guiNote.setColor(note.getColor());
			guiNote.setDate(note.getDate());
			guiNote.setDocId(document.getId());
			guiNote.setFileName(note.getFileName());
			guiNote.setHeight(note.getHeight());
			guiNote.setId(note.getId());
			guiNote.setLeft(note.getLeft());
			guiNote.setMessage(note.getMessage());
			guiNote.setOpacity(note.getOpacity());
			guiNote.setPage(note.getPage());
			guiNote.setTop(note.getTop());
			guiNote.setUserId(note.getUserId());
			guiNote.setUsername(note.getUsername());
			guiNote.setWidth(note.getWidth());
			guiNote.setFileVersion(note.getFileVersion());
			guiNote.setType(note.getType());
			guiNote.setRecipient(note.getRecipient());
			guiNote.setRecipientEmail(note.getRecipientEmail());
			guiNote.setLineColor(note.getLineColor());
			guiNote.setLineWidth(note.getLineWidth());
			guiNote.setLineOpacity(note.getLineOpacity());
			guiNote.setShape(note.getShape());
			guiNote.setRotation(note.getRotation());
			guiNotes.add(guiNote);
		}

		return guiNotes.toArray(new GUIDocumentNote[0]);
	}

	@Override
	public void saveNotes(long docId, GUIDocumentNote[] notes, Collection<String> types) throws ServerException {
		Session session = validateSession();

		DocumentNoteDAO dao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);
		List<GUIDocumentNote> notesList = new ArrayList<>();
		Document document = null;
		try {
			document = retrieveDocument(docId);
			if (document == null)
				throw new ServerException(UNEXISTING_DOCUMENT + " " + docId);

			if (notes != null && notes.length > 0)
				notesList = Arrays.asList(notes);

			/*
			 * Check for deletions
			 */
			List<DocumentNote> documentNotes = dao.findByDocIdAndTypes(document.getId(), document.getFileVersion(),
					types);
			List<Long> actualNoteIds = documentNotes.stream().map(PersistentObject::getId).toList();
			List<Long> noteIds = notesList.stream().map(GUIDocumentNote::getId).toList();
			for (Long actualNoteId : actualNoteIds)
				if (!noteIds.contains(actualNoteId))
					dao.delete(actualNoteId);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}

		if (document == null)
			throw new ServerException(UNEXISTING_DOCUMENT + " " + docId);

		/*
		 * Do the updates / inserts
		 */
		for (GUIDocumentNote guiNote : notesList) {
			saveNote(session, document, guiNote);
		}

	}

	private void saveNote(Session session, Document document, GUIDocumentNote guiNote) throws ServerException {

		DocumentNoteDAO dao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);

		DocumentNote note = null;
		try {
			note = dao.findById(guiNote.getId());
		} catch (PersistenceException e) {
			// Unexisting note
		}

		if (note == null) {
			note = new DocumentNote();
			note.setTenantId(session.getTenantId());
			note.setDocId(document.getId());
			note.setUserId(session.getUserId());
			note.setUsername(session.getUser().getFullName());
			note.setDate(new Date());
			note.setPage(guiNote.getPage());
		}

		note.setFileName(document.getFileName());
		note.setFileVersion(document.getFileVersion());
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

		saveNote(note, session);

		/*
		 * If the note specifies a recipient, update the user's address book
		 */
		if (StringUtils.isNotEmpty(note.getRecipientEmail())) {
			ContactDAO cDao = (ContactDAO) Context.get().getBean(ContactDAO.class);
			List<Contact> contacts = cDao.findByUser(session.getUserId(), note.getRecipientEmail());
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
			DocumentNoteDAO dao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);
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
			ContactDAO cDao = (ContactDAO) Context.get().getBean(ContactDAO.class);
			cDao.store(contact);
		} catch (PersistenceException e) {
			log.warn("Error storing new contact {}", contact.getEmail(), e);
		}
	}

	@Override
	public void deleteNotes(long[] ids) throws ServerException {
		validateSession();

		DocumentNoteDAO dao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);
		for (long id : ids)
			try {
				dao.delete(id);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
	}

	@Override
	public GUIDocument[] bulkUpdate(Long[] ids, GUIDocument vo, boolean ignoreEmptyFields) throws ServerException {
		Session session = validateSession();

		List<GUIDocument> updatedDocs = new ArrayList<>();
		for (long docId : ids) {
			try {
				GUIDocument buf = bulkUpdateDocument(docId, vo, ignoreEmptyFields, session);
				if (buf != null) {
					GUIDocument document = save(buf);
					updatedDocs.add(document);
					if (vo.getRights() != null && vo.getRights().length > 0) {
						document.setRights(vo.getRights());
						saveACL(document);
					}
				}
			} catch (ServerException e) {
				log.error(e.getMessage(), e);
			}
		}
		return updatedDocs.toArray(new GUIDocument[0]);
	}

	private GUIDocument bulkUpdateDocument(long docId, GUIDocument model, boolean ignoreEmptyFields, Session session)
			throws ServerException {
		GUIDocument document = getById(docId);

		if (document.getImmutable() == 1 || document.getStatus() != AbstractDocument.DOC_UNLOCKED) {
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

		if (model.getPublished() > -1)
			document.setPublished(model.getPublished());
		if (model.getStartPublishing() != null)
			document.setStartPublishing(model.getStartPublishing());
		if (model.getStopPublishing() != null)
			document.setStopPublishing(model.getStopPublishing());
		if (StringUtils.isNotEmpty(model.getLanguage()))
			document.setLanguage(model.getLanguage());
		if (model.getTags() != null && model.getTags().length > 0)
			document.setTags(model.getTags());
		else if (!ignoreEmptyFields)
			document.setTags(null);
		if (model.getTemplateId() != null)
			document.setTemplateId(model.getTemplateId());

		setOcrTemplate(model, ignoreEmptyFields, document);

		setBarcodeTemplate(model, ignoreEmptyFields, document);

		setExtendedAttributes(model, ignoreEmptyFields, document);

		return document;
	}

	private void setExtendedAttributes(GUIDocument model, boolean ignoreEmptyFields, GUIDocument document) {
		if (model.getAttributes() == null || model.getAttributes().length < 1)
			return;

		if (ignoreEmptyFields) {
			Map<String, GUIAttribute> attributes = new HashMap<>();
			for (GUIAttribute att : document.getAttributes())
				attributes.put(att.getName(), att);

			for (GUIAttribute att : model.getAttributes()) {
				if (att.getValue() != null && StringUtils.isNotEmpty(att.getValue().toString()))
					attributes.put(att.getName(), att);
			}
			document.setAttributes(attributes.values().toArray(new GUIAttribute[0]));
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
	public void updateNote(long docId, long noteId, String message) throws ServerException {
		Session session = validateSession();

		try {
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document document = docDao.findDocument(docId);
			if (document == null)
				throw new ServerException(UNEXISTING_DOCUMENT + " " + docId);

			DocumentNoteDAO dao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);
			DocumentNote note = dao.findById(noteId);
			if (note == null) {
				note = new DocumentNote();
				note.setTenantId(session.getTenantId());
				note.setDocId(document.getId());
				note.setUserId(session.getUserId());
				note.setUsername(session.getUser().getFullName());
				note.setColor(null);
			}

			note.setFileName(document.getFileName());
			note.setFileVersion(document.getFileVersion());
			note.setMessage(message);
			note.setUserId(session.getUser().getId());
			note.setUsername(session.getUser().getFullName());
			note.setMessage(message);

			saveNote(note, session);
		} catch (PersistenceException | ServerException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public GUIDocument deleteVersions(long[] ids) throws ServerException {
		Session session = validateSession();

		long docId = 0;
		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		for (long id : ids) {
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			Version version;
			try {
				version = manager.deleteVersion(id, transaction);
			} catch (Exception e) {
				return (GUIDocument) throwServerException(session, log, e);
			}
			docId = version.getDocId();
		}

		return getById(docId);
	}

	@Override
	public GUIDocument createWithContent(GUIDocument vo, String content, boolean checkout) throws ServerException {
		Session session = validateSession();

		try {
			DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);

			if (!fdao.isWriteEnabled(vo.getFolder().getId(), session.getUserId()))
				throw new PermissionException(session.getUsername(), "Folder " + vo.getFolder().getId(),
						Permission.WRITE);

			Document doc = toDocument(vo);
			doc.setId(0L);

			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			transaction.setEvent(DocumentEvent.STORED.toString());
			Document document;
			if (StringUtils.isEmpty(content))
				document = documentManager.create(IOUtils.toInputStream(" ", StandardCharsets.UTF_8), doc, transaction);
			else
				document = documentManager.create(IOUtils.toInputStream(content, StandardCharsets.UTF_8), doc,
						transaction);

			if (checkout) {
				// Perform a checkout also
				transaction = new DocumentHistory();
				transaction.setSession(session);
				documentManager.checkout(document.getId(), transaction);
			}

			return fromDocument(document, vo.getFolder(), null);
		} catch (PermissionException | PersistenceException e) {
			return (GUIDocument) throwServerException(session, log, e);
		}
	}

	@Override
	public void deleteFromTrash(Long[] ids) throws ServerException {
		Session session = validateSession();
		if (ids == null || ids.length < 1)
			return;

		String idsStr = Arrays.asList(ids).toString().replace('[', '(').replace(']', ')');
		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		try {
			dao.bulkUpdate("set ld_deleted=2 where ld_id in " + idsStr, (Map<String, Object>) null);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}

	}

	@Override
	public void emptyTrash() throws ServerException {
		Session session = validateSession();

		try {
			DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			dao.bulkUpdate("set ld_deleted=2 where ld_deleted=1 and  ld_deleteuserid=" + session.getUserId(),
					(Map<String, Object>) null);

			FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			fdao.bulkUpdate("set ld_deleted=2 where ld_deleted=1 and  ld_deleteuserid=" + session.getUserId(),
					(Map<String, Object>) null);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}

	}

	@Override
	public void archiveDocuments(Long[] docIds, String comment) throws ServerException {
		Session session = validateSession();

		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSession(session);
		try {
			manager.archiveDocuments(ArrayUtils.toPrimitive(docIds), transaction);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public long archiveFolder(long folderId, String comment) throws ServerException {
		Session session = validateSession();

		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSession(session);
		transaction.setComment(HTMLSanitizer.sanitizeSimpleText(comment));
		try {
			return manager.archiveFolder(folderId, transaction);
		} catch (PersistenceException e) {
			return (Long) throwServerException(session, log, e);
		}
	}

	@Override
	public void unarchiveDocuments(long[] docIds) throws ServerException {
		Session session = validateSession();

		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

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
	public long countDocuments(long[] folderIds, int status) throws ServerException {
		validateSession();
		long count = 0;
		for (int i = 0; i < folderIds.length; i++) {
			count += countDocuments(folderIds[i], status);
		}
		return count;
	}

	private long countDocuments(long folderId, int status) {
		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);

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
	public String[] createDownloadTicket(long docId, int type, String suffix, Integer expireHours, Date expireDate,
			Integer maxDownloads, Integer maxViews) throws ServerException {
		Session session = validateSession();

		String urlPrefix = "";
		HttpServletRequest request = this.getThreadLocalRequest();
		if (request != null)
			urlPrefix = request.getScheme() + "://" + request.getServerName() + ":" + request.getServerPort()
					+ request.getContextPath();

		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
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

			ticket = manager.createTicket(ticket, transaction);

			String[] result = new String[3];
			result[0] = ticket.getTicketId();
			result[1] = ticket.getUrl();
			result[2] = new URI(
					ticket.getUrl().replace(urlPrefix, Context.get().getProperties().getProperty("server.url")))
							.normalize().toString();
			return result;
		} catch (PermissionException | PersistenceException | URISyntaxException e) {
			return (String[]) throwServerException(session, log, e);
		}
	}

	@Override
	public void setPassword(long docId, String password) throws ServerException {
		Session session = validateSession();

		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

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

		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

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

		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		return manager.unprotect(session.getSid(), docId, password);
	}

	@Override
	public String getContentAsString(long docId) throws ServerException {
		Session session = validateSession();

		try {
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document doc = docDao.findById(docId);
			if (doc == null)
				throw new ServerException(UNEXISTING_DOCUMENT);

			FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			if (!fDao.isDownloadEnabled(doc.getFolder().getId(), session.getUserId()))
				throw new IOException("You don't have the DOWNLOAD permission");

			/*
			 * In case of alias we have to work on the real document
			 */
			if (doc.getDocRef() != null)
				doc = docDao.findById(doc.getDocRef());

			// Obtain the document's file stream
			Storer storer = (Storer) Context.get().getBean(Storer.class);
			String resource = storer.getResourceName(doc, null, null);

			return storer.getString(doc.getId(), resource);
		} catch (PersistenceException | ServerException | IOException e) {
			return (String) throwServerException(session, log, e);
		}

	}

	@Override
	public GUIDocument checkinContent(long docId, String content) throws ServerException {
		Session session = validateSession();

		try {
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document doc = docDao.findById(docId);
			if (doc == null)
				throw new ServerException(UNEXISTING_DOCUMENT);

			FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			if (!fDao.isWriteEnabled(doc.getFolder().getId(), session.getUserId()))
				throw new PermissionException(session.getUsername(), DOCUMENT_STR + docId, Permission.WRITE);

			if (doc.getStatus() != AbstractDocument.DOC_CHECKED_OUT || doc.getLockUserId() != session.getUserId())
				throw new PermissionException("You have not checked out the file " + docId);

			DocumentHistory transaction = new DocumentHistory();
			transaction.setComment("Text content editing");
			transaction.setSession(session);

			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			manager.checkin(docId, IOUtils.toInputStream(content, StandardCharsets.UTF_8), doc.getFileName(), false,
					null, transaction);

			return getById(docId);
		} catch (PermissionException | PersistenceException | ServerException | IOException e) {
			return (GUIDocument) throwServerException(session, log, e);
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
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document doc = docDao.findById(docId);
			if (doc == null)
				throw new ServerException(UNEXISTING_DOCUMENT);

			FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			if (!fDao.isWriteEnabled(doc.getFolder().getId(), session.getUserId()))
				throw new IOException("You don't have the WRITE permission");

			doc = docDao.findDocument(docId);

			if (doc.getStatus() != AbstractDocument.DOC_UNLOCKED)
				throw new IOException("The document is locked");

			DocumentHistory transaction = new DocumentHistory();
			transaction.setComment(HTMLSanitizer.sanitizeSimpleText(comment));
			transaction.setSession(session);

			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			manager.replaceFile(doc.getId(), fileVersion, file, transaction);

			UploadServlet.cleanReceivedFiles(session.getSid());
		} catch (PersistenceException | ServerException | IOException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public GUIDocument createDocument(GUIDocument document, String content) throws ServerException {
		Session session = validateSession();

		try {
			FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			if (!fDao.isWriteEnabled(document.getFolder().getId(), session.getUserId()))
				throw new IOException("You don't have the WRITE permission");

			DocumentHistory transaction = new DocumentHistory();
			transaction.setComment("Text content creation");
			transaction.setSession(session);

			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			Document doc = manager.create(IOUtils.toInputStream(content, StandardCharsets.UTF_8), toDocument(document),
					transaction);

			return getById(doc.getId());
		} catch (PersistenceException | IOException | ServerException e) {
			return (GUIDocument) throwServerException(session, log, e);
		}
	}

	@Override
	public GUIRating getUserRating(long docId) throws ServerException {
		Session session = validateSession();

		RatingDAO rDao = (RatingDAO) Context.get().getBean(RatingDAO.class);
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

	}

	@Override
	public Integer deleteRating(long id) throws ServerException {
		Session session = validateSession();

		try {
			RatingDAO rDao = (RatingDAO) Context.get().getBean(RatingDAO.class);
			Rating rat = rDao.findById(id);
			if (rat == null)
				return 0;

			if (rat.getUserId() != session.getUserId())
				throw new ServerException("Cannot delete the rating left by another user");

			rDao.delete(id);

			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document doc = docDao.findById(rat.getDocId());
			return doc.getRating();
		} catch (PersistenceException | ServerException e) {
			return (Integer) throwServerException(session, log, e);
		}
	}

	@Override
	public GUIDocument convert(long docId, String fileVersion, String format) throws ServerException {
		Session session = validateSession();

		try {
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document doc = docDao.findById(docId);
			if (doc == null)
				throw new ServerException(UNEXISTING_DOCUMENT);

			FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			if (!fDao.isWriteEnabled(doc.getFolder().getId(), session.getUserId()))
				throw new PermissionException(session.getUsername(), DOCUMENT_STR + doc.getId(), Permission.WRITE);
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);

			FormatConverterManager manager = (FormatConverterManager) Context.get()
					.getBean(FormatConverterManager.class);
			Document conversion = manager.convert(doc, fileVersion, format, transaction);
			if (conversion == null)
				throw new ServerException("Unable to convert");
			return getById(conversion.getId());
		} catch (Exception e) {
			return (GUIDocument) throwServerException(session, log, e);
		}

	}

	@Override
	public GUIEmail extractEmail(long docId, String fileVersion) throws ServerException {
		Session session = validateSession();

		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document emailDocument = null;
		try {
			emailDocument = dao.findDocument(docId);
			if (!emailDocument.getFileName().toLowerCase().endsWith(".eml")
					&& !emailDocument.getFileName().toLowerCase().endsWith(".msg"))
				throw new ServerException("Not an email file");
		} catch (PersistenceException e1) {
			return (GUIEmail) throwServerException(session, log, e1);
		}

		Storer storer = (Storer) Context.get().getBean(Storer.class);
		String resource = storer.getResourceName(docId, fileVersion, null);

		GUIDocument guiDocument = getById(docId);

		try (InputStream is = storer.getStream(emailDocument.getId(), resource)) {
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
		} catch (IOException e1) {
			return (GUIEmail) throwServerException(session, log, e1);
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
		guiMail.setAttachments(attachments.toArray(new GUIDocument[0]));
	}

	private void setEmailRecipients(EMail email, GUIEmail guiMail) {
		Set<Recipient> recipients = email.getRecipients();
		List<GUIContact> contacts = new ArrayList<>();
		for (Recipient rec : recipients)
			contacts.add(new GUIContact(rec.getName(), null, rec.getAddress()));
		guiMail.setTos(contacts.toArray(new GUIContact[0]));

		recipients = email.getRecipientsCC();
		contacts = new ArrayList<>();
		for (Recipient rec : recipients)
			contacts.add(new GUIContact(rec.getName(), null, rec.getAddress()));
		guiMail.setCcs(contacts.toArray(new GUIContact[0]));

		recipients = email.getRecipientsBCC();
		contacts = new ArrayList<>();
		for (Recipient rec : recipients)
			contacts.add(new GUIContact(rec.getName(), null, rec.getAddress()));
		guiMail.setBccs(contacts.toArray(new GUIContact[0]));

		recipients = email.getReplyTo();
		contacts = new ArrayList<>();
		for (Recipient rec : recipients)
			contacts.add(new GUIContact(rec.getName(), null, rec.getAddress()));
		guiMail.setReplyTo(contacts.toArray(new GUIContact[0]));
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

			Storer storer = (Storer) Context.get().getBean(Storer.class);
			String resource = storer.getResourceName(docId, fileVersion, null);
			is = storer.getStream(docId, resource);

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
			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);

			Document docVO = new Document();
			docVO.setFileName(attachmentFileName);
			docVO.setFileSize(attachment.getSize());
			docVO.setFolder(fDao.findById(doc.getFolder().getId()));

			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			Document d = manager.create(tmp, docVO, transaction);
			return getById(d.getId());
		} catch (IOException | PersistenceException | MessagingException e) {
			return (GUIDocument) throwServerException(session, log, e);
		} finally {
			IOUtils.closeQuietly(is);
			FileUtil.strongDelete(tmp);
		}
	}

	@Override
	public GUIDocument replaceAlias(long aliasId) throws ServerException {
		Session session = validateSession();

		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSession(session);
		try {
			Document doc = manager.replaceAlias(aliasId, transaction);
			return getDocument(session, doc.getId());
		} catch (InvalidSessionServerException | PermissionException | PersistenceException e) {
			return (GUIDocument) throwServerException(session, log, e);
		}
	}

	@Override
	public void deDuplicate(Long folderId, boolean retainNewest) throws ServerException {
		Session session = validateSession();
		checkMenu(getThreadLocalRequest(), Menu.REPORTS);

		try {
			// First of all, find all duplicates digests
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
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

			FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);

			@SuppressWarnings("unchecked")
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
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document maintainedDoc = docDao.findById(duplications.get(0).getId());
		log.info("Process digest {}, retain document {} dated {}", maintainedDoc.getDigest(), maintainedDoc,
				maintainedDoc.getDate());

		duplications.remove(0);
		List<Long> duplicatedIds = duplications.stream().map(d -> d.getId()).toList();

		log.warn("Deleting the duplicated documents {}", duplicatedIds);
		StringBuilder updateStatement = new StringBuilder("update ld_document set ld_deleted=1 where ");
		if (docDao.isOracle()) {
			/*
			 * In Oracle The limit of 1000 elements applies to sets of single
			 * items: (x) IN ((1), (2), (3), ...). There is no limit if the sets
			 * contain two or more items: (x, 0) IN ((1,0), (2,0), (3,0), ...):
			 */
			updateStatement.append(" (ld_id,0) in ( ");
			boolean firstItem = true;
			for (Long id : duplicatedIds) {
				if (!firstItem)
					updateStatement.append(",");
				updateStatement.append("(");
				updateStatement.append(id);
				updateStatement.append(",0)");
				firstItem = false;
			}
			updateStatement.append(" )");
		} else {
			updateStatement.append(" ld_id in ");
			updateStatement.append(duplicatedIds.toString().replace('[', '(').replace(']', ')'));
		}
		docDao.jdbcUpdate(updateStatement.toString());

		log.debug("Prepare the histories");
		for (Document doc : duplications) {
			if (doc.getId() == maintainedDoc.getId())
				continue;
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			transaction.setDocument(doc);
			transaction.setComment("Deleted by deduplication");
			transaction.setEvent(DocumentEvent.DELETED.toString());
			transaction.setFilename(doc.getFileName());
			transaction.setVersion(doc.getVersion());
			transaction.setFileVersion(doc.getFileVersion());
			transaction.setPath(doc.getFolder().getPathExtended());
			docDao.saveDocumentHistory(doc, transaction);
		}

		// Create the aliases
		FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
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

		TicketDAO dao = (TicketDAO) Context.get().getBean(TicketDAO.class);
		try {
			dao.delete(ticketId);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void enableTicket(long ticketId) throws ServerException {
		Session session = validateSession();

		TicketDAO dao = (TicketDAO) Context.get().getBean(TicketDAO.class);
		try {
			Ticket ticket = dao.findById(ticketId);
			ticket.setEnabled(1);
			dao.store(ticket);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void disableTicket(long ticketId) throws ServerException {
		Session session = validateSession();

		TicketDAO dao = (TicketDAO) Context.get().getBean(TicketDAO.class);
		try {
			Ticket ticket = dao.findById(ticketId);
			ticket.setEnabled(0);
			dao.store(ticket);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void enforceFilesIntoFolderStorage(long folderId) throws ServerException {
		Session session = validateSession();
		User user = session.getUser();

		new Thread(() -> {
			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			int movedFiles = 0;

			FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			String treePath = null;
			try {
				treePath = fDao.computePathExtended(folderId);
				DocumentHistory transaction = new DocumentHistory();
				transaction.setSession(session);
				movedFiles = manager.enforceFilesIntoFolderStorage(folderId, transaction);

				log.info("Notify the move of {} files to the right storage in the tree {}", movedFiles, treePath);

				notifyEnforcement(session, I18N.message("enforcementofstoragereport", user.getLocale(),
						new Object[] { movedFiles, treePath }));

			} catch (Exception t) {
				log.error("Error enforcing files storage into tree {}", treePath, t);
				try {
					notifyEnforcement(session, I18N.message("enforcementofstorageerror", user.getLocale(),
							new Object[] { movedFiles, treePath, t.getMessage() }));
				} catch (Exception e) {
					log.warn(e.getMessage(), e);
				}
			}
		}).start();
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
		sys.setHtml(0);
		sys.setAuthor("SYSTEM");
		sys.setSentDate(new Date());
		sys.setNotify(true);
		sys.getRecipients().add(sysRecipient);
		sys.setMessageText(message);
		sys.setSubject(I18N.message("enforcementofstorage", user.getLocale()));

		try {
			SystemMessageDAO sDao = (SystemMessageDAO) Context.get().getBean(SystemMessageDAO.class);
			sDao.store(sys);

			// Prepare the email
			Recipient emailRecipient = new Recipient();
			emailRecipient.setName(user.getUsername());
			emailRecipient.setAddress(user.getEmail());
			emailRecipient.setType(Recipient.TYPE_EMAIL);
			emailRecipient.setMode(Recipient.MODE_EMAIL_TO);
			emailRecipient.setRead(1);

			EMail mail = new EMail();
			mail.setHtml(0);
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
	public GUIDocument merge(Long[] docIds, long targetFolderId, String fileName) throws ServerException {
		final Session session = validateSession();

		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSession(session);

		try {
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			List<Document> docs = new ArrayList<>();
			for (long docId : docIds)
				docs.add(docDao.findDocument(docId));

			Document doc = manager.merge(docs, targetFolderId,
					fileName.toLowerCase().endsWith(".pdf") ? fileName : fileName + ".pdf", transaction);
			return getDocument(session, doc.getId());
		} catch (InvalidSessionServerException | PermissionException | PersistenceException | IOException e) {
			return (GUIDocument) throwServerException(session, log, e);
		}
	}

	@Override
	public int updatePages(long docId) throws ServerException {
		final Session session = validateSession();

		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		try {
			Document doc = docDao.findDocument(docId);
			if (doc != null) {
				docDao.initialize(doc);
				int pages = manager.countPages(doc);
				doc.setPages(pages);
				return pages;
			}
			return 1;
		} catch (PersistenceException e) {
			return (Integer) throwServerException(session, log, e);
		}

	}

	@Override
	public GUIDocument rename(long documentId, String name) throws ServerException {
		final Session session = validateSession();

		User user = session.getUser();
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		try {
			Document doc = docDao.findById(documentId);
			checkPermission(Permission.RENAME, user, doc.getFolder().getId());
			checkPublished(user, doc);

			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);

			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			transaction.setUser(user);
			manager.rename(documentId, name, transaction);

			return getDocument(session, documentId);
		} catch (AccessDeniedException | PermissionException | InvalidSessionServerException | PersistenceException e) {
			return (GUIDocument) throwServerException(session, log, e);
		}
	}

	private String prepareTileAsString(Session session, Document doc) {
		String tile = null;
		File tileFile = null;
		try {
			tileFile = createTile(doc, session.getSid());
			tile = ImageUtil.encodeImage(tileFile);
			if (tile != null)
				tile = "data:image/png;base64," + tile;
		} catch (IOException e) {
			log.warn("Cannot generate tile of document {}", doc, e);
		} finally {
			FileUtil.strongDelete(tileFile);
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

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		try {
			Document document = docDao.findById(guiDocument.getId());
			docDao.initialize(document);

			log.info("Applying {} security policies to document {}",
					(guiDocument.getRights() != null ? guiDocument.getRights().length : 0), guiDocument.getId());

			Set<AccessControlEntry> acl = new HashSet<>();
			for (GUIAccessControlEntry guiAce : guiDocument.getRights()) {
				AccessControlEntry ace = new AccessControlEntry();
				ace.setGroupId(guiAce.getEntityId());
				acl.add(ace);

				ace.setRead(booleanToInt(guiAce.isRead()));
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
			}

			document.getAccessControlList().clear();
			document.getAccessControlList().addAll(acl);

			// Add a document history entry
			DocumentHistory history = new DocumentHistory();
			history.setEvent(DocumentEvent.PERMISSION.toString());
			history.setSession(session);
			docDao.store(document, history);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void applyParentFolderSecurity(long docId) throws ServerException {
		Session session = validateSession();

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		try {
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			docDao.applyParentFolderSecurity(docId, transaction);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public GUIAccessControlEntry getEnabledPermissions(Long[] docIds) throws ServerException {
		Session session = validateSession();

		try {
			Set<Permission> commonPermissions = Permission.all();
			if (!session.getUser().isMemberOf("admin")) {
				DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
				for (long docId : docIds) {
					Set<Permission> docPermissions = docDao.getEnabledPermissions(docId, session.getUserId());
					for (Permission permission : Permission.all()) {
						if (!docPermissions.contains(permission))
							commonPermissions.remove(permission);
					}
				}
			}

			return new GUIAccessControlEntry(commonPermissions.stream().map(p -> p.name().toLowerCase())
					.collect(Collectors.toSet()).toArray(new String[0]));
		} catch (PersistenceException e) {
			return (GUIAccessControlEntry) throwServerException(session, log, e);
		}
	}
}