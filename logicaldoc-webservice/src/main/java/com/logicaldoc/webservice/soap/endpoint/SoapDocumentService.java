package com.logicaldoc.webservice.soap.endpoint;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.activation.DataHandler;
import javax.mail.MessagingException;

import org.apache.commons.lang.StringUtils;
import org.apache.cxf.jaxrs.ext.multipart.InputStreamDataSource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailAttachment;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.conversion.FormatConverterManager;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentComparator;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentHistoryDAO;
import com.logicaldoc.core.document.DocumentLink;
import com.logicaldoc.core.document.DocumentLinkDAO;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.document.DocumentNote;
import com.logicaldoc.core.document.DocumentNoteDAO;
import com.logicaldoc.core.document.Rating;
import com.logicaldoc.core.document.RatingDAO;
import com.logicaldoc.core.document.Version;
import com.logicaldoc.core.document.VersionDAO;
import com.logicaldoc.core.document.thumbnail.ThumbnailManager;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.parser.ParsingException;
import com.logicaldoc.core.searchengine.SearchEngine;
import com.logicaldoc.core.security.AccessControlEntry;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.store.Store;
import com.logicaldoc.core.ticket.Ticket;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.MimeType;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.webservice.AbstractService;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSAccessControlEntry;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSLink;
import com.logicaldoc.webservice.model.WSNote;
import com.logicaldoc.webservice.model.WSRating;
import com.logicaldoc.webservice.model.WSUtil;
import com.logicaldoc.webservice.soap.DocumentService;

/**
 * Document Web Service Implementation (SOAP)
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
public class SoapDocumentService extends AbstractService implements DocumentService {

	private static final String DOCUMENT_WITH_ID = "Document with ID ";

	private static final String NOT_FOUND_OR_NOT_ACCESSIBLE = " not found or not accessible";

	private static final String IS_LOCKED = " is locked";

	private static final String IS_IMMUTABLE = " is immutable";

	private static final String THE_DOCUMENT = "The document ";

	private static final Logger log = LoggerFactory.getLogger(SoapDocumentService.class);

	@Override
	public WSDocument create(String sid, WSDocument document, DataHandler content) throws IOException,
			AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		return create(sid, document, content.getInputStream());
	}

	public WSDocument create(String sid, WSDocument document, InputStream content)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);

		checkFolderPermission(Permission.WRITE, user, document.getFolderId());

		FolderDAO fdao = Context.get(FolderDAO.class);
		Folder folder = fdao.findById(document.getFolderId());

		long rootId = fdao.findRoot(user.getTenantId()).getId();

		if (folder == null) {
			throw new WebserviceException(String.format("Folder %d not found", document.getFolderId()));
		} else if (folder.getId() == rootId) {
			throw new WebserviceException("Cannot add documents in the root");
		}

		Document doc = WSUtil.toDocument(document);
		doc.setTenantId(user.getTenantId());

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId(sid);
		transaction.setEvent(DocumentEvent.STORED.toString());
		transaction.setComment(document.getComment());
		transaction.setUser(user);

		DocumentManager documentManager = Context.get(DocumentManager.class);
		doc = documentManager.create(content, doc, transaction).getDocument();
		return WSUtil.toWSDocument(doc);
	}

	public void checkinDocument(String sid, long docId, String comment, String filename, boolean release,
			WSDocument docVO, InputStream content) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, IOException {
		checkin(sid, docId, comment, filename, release, docVO, content);
	}

	@Override
	public void checkinDocument(String sid, long docId, String comment, String filename, boolean release,
			WSDocument docVO, DataHandler content) throws IOException, AuthenticationException, PermissionException,
			WebserviceException, PersistenceException {
		checkin(sid, docId, comment, filename, release, docVO, content.getInputStream());
	}

	@Override
	public void checkin(String sid, long docId, String comment, String filename, boolean release, DataHandler content)
			throws IOException, AuthenticationException, PermissionException, WebserviceException,
			PersistenceException {
		checkin(sid, docId, comment, filename, release, null, content.getInputStream());
	}

	public void checkin(String sid, long docId, String comment, String filename, boolean release, WSDocument docVO,
			InputStream content) throws AuthenticationException, WebserviceException, PersistenceException,
			PermissionException, IOException {
		User user = validateSession(sid);
		DocumentDAO ddao = Context.get(DocumentDAO.class);

		Document document = ddao.findById(docId);
		if (document.getImmutable() == 1)
			throw new PermissionException(THE_DOCUMENT + docId + IS_IMMUTABLE);

		checkDocumentPermission(Permission.READ, user, docId);

		document = ddao.findDocument(docId);

		if (document.getStatus() == AbstractDocument.DOC_CHECKED_OUT
				&& (user.getId() == document.getLockUserId() || user.isMemberOf(Group.GROUP_ADMIN))) {
			ddao.initialize(document);

			// Create the document history event
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSessionId(sid);
			transaction.setEvent(DocumentEvent.CHECKEDIN.toString());
			transaction.setUser(user);
			transaction.setComment(comment);

			Document doc = null;
			if (docVO != null) {
				doc = WSUtil.toDocument(docVO);
				doc.setTenantId(user.getTenantId());
			}

			/*
			 * checkin the document; throws an exception if something goes wrong
			 */
			DocumentManager documentManager = Context.get(DocumentManager.class);
			documentManager.checkin(document.getId(), content, filename, release, doc, transaction);

			/* create positive log message */
			log.info("Document {} checked in", document.getId());
		} else {
			throw new WebserviceException("document not checked in");
		}

	}

	@Override
	public void checkout(String sid, long docId)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);
		DocumentDAO docDao = Context.get(DocumentDAO.class);
		Document doc = docDao.findById(docId);
		if (doc.getImmutable() == 1)
			throw new PermissionException(THE_DOCUMENT + docId + IS_IMMUTABLE);

		if (doc.getStatus() != AbstractDocument.DOC_UNLOCKED)
			throw new PermissionException("The document is locked or already checked out");

		checkDocumentPermission(Permission.WRITE, user, docId);
		checkDocumentPermission(Permission.DOWNLOAD, user, docId);

		doc = docDao.findDocument(docId);
		checkPublished(user, doc);

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId(sid);
		transaction.setEvent(DocumentEvent.CHECKEDOUT.toString());
		transaction.setComment("");
		transaction.setUser(user);

		DocumentManager documentManager = Context.get(DocumentManager.class);
		documentManager.checkout(doc.getId(), transaction);
	}

	@Override
	public void delete(String sid, long docId)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);
		DocumentDAO docDao = Context.get(DocumentDAO.class);
		Document doc = docDao.findById(docId);
		checkLocked(user, doc);
		checkFolderPermission(Permission.DELETE, user, doc.getFolder().getId());
		checkPublished(user, doc);

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId(sid);
		transaction.setEvent(DocumentEvent.DELETED.toString());
		transaction.setComment("");
		transaction.setUser(user);

		docDao.delete(docId, transaction);
	}

	private void checkLocked(User user, Document doc) throws PermissionException {
		if (user.isMemberOf(Group.GROUP_ADMIN))
			return;

		if (doc.getImmutable() == 1)
			throw new PermissionException(THE_DOCUMENT + doc.getId() + IS_IMMUTABLE);

		if (doc.getStatus() != AbstractDocument.DOC_UNLOCKED && user.getId() != doc.getLockUserId())
			throw new PermissionException(THE_DOCUMENT + doc.getId() + IS_LOCKED);
	}

	@Override
	public DataHandler getContent(String sid, long docId) throws AuthenticationException, WebserviceException,
			PersistenceException, PermissionException, IOException {
		return getVersionContent(sid, docId, null);
	}

	@Override
	public DataHandler getVersionContent(String sid, long docId, String version) throws AuthenticationException,
			WebserviceException, PersistenceException, PermissionException, IOException {
		String fileVersion = null;
		if (version != null) {
			VersionDAO vDao = Context.get(VersionDAO.class);
			Version v = vDao.findByVersion(docId, version);
			fileVersion = v.getFileVersion();
		}

		return getResource(sid, docId, fileVersion, null);
	}

	@Override
	public DataHandler getResource(String sid, long docId, String fileVersion, String suffix)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException,
			IOException {
		User user = validateSession(sid);

		checkDocumentPermission(Permission.READ, user, docId);
		checkDocumentPermission(Permission.DOWNLOAD, user, docId);

		DocumentDAO docDao = Context.get(DocumentDAO.class);
		Document doc = docDao.findDocument(docId);
		checkPublished(user, doc);

		if (doc.isPasswordProtected()) {
			Session session = SessionManager.get().get(sid);
			if (!session.getUnprotectedDocs().containsKey(doc.getId()))
				throw new PermissionException(String.format("The document is protected by a password %s", doc));
		}

		Store store = Context.get(Store.class);
		String resourceName = store.getResourceName(doc, fileVersion, suffix);

		if (!store.exists(doc.getId(), resourceName)) {
			throw new WebserviceException("Resource " + resourceName + " not found");
		}

		log.debug("Attach file {}", resourceName);

		String fileName = doc.getFileName();
		if (StringUtils.isNotEmpty(suffix))
			fileName = suffix;
		String mime = MimeType.getByFilename(fileName);
		return new DataHandler(new InputStreamDataSource(store.getStream(doc.getId(), resourceName), mime));
	}

	@Override
	public void createPdf(String sid, long docId, String fileVersion) throws AuthenticationException,
			WebserviceException, PersistenceException, PermissionException, IOException {
		User user = validateSession(sid);

		checkDocumentPermission(Permission.READ, user, docId);

		DocumentDAO docDao = Context.get(DocumentDAO.class);
		Document doc = docDao.findDocument(docId);

		FormatConverterManager manager = Context.get(FormatConverterManager.class);
		manager.convertToPdf(doc, fileVersion, sid);
	}

	@Override
	public void createThumbnail(String sid, long docId, String fileVersion, String type)
			throws AuthenticationException, WebserviceException, PersistenceException, IOException {
		validateSession(sid);

		ThumbnailManager manager = Context.get(ThumbnailManager.class);
		Store store = Context.get(Store.class);
		DocumentDAO docDao = Context.get(DocumentDAO.class);
		Document doc = docDao.findDocument(docId);

		if (!type.toLowerCase().endsWith(".png"))
			type += ".png";
		String resource = store.getResourceName(doc, fileVersion, type);
		if (!store.exists(docId, resource)) {
			if (type.equals(ThumbnailManager.SUFFIX_THUMB))
				manager.createTumbnail(doc, fileVersion, sid);
			else if (type.equals(ThumbnailManager.SUFFIX_TILE))
				manager.createTile(doc, fileVersion, sid);
			else if (type.equals(ThumbnailManager.SUFFIX_MOBILE))
				manager.createMobile(doc, fileVersion, sid);
			else if (type.startsWith(ThumbnailManager.THUMB)) {
				/*
				 * In this case the resource is like thumb450.png so we extract
				 * the size from the name
				 */
				String sizeStr = resource.substring(resource.indexOf('-') + 6, resource.lastIndexOf('.'));
				manager.createTumbnail(doc, fileVersion, Integer.parseInt(sizeStr), null, sid);
			}
		}
	}

	@Override
	public void uploadResource(String sid, long docId, String fileVersion, String suffix, DataHandler content)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException,
			IOException {
		User user = validateSession(sid);

		if (StringUtils.isEmpty(suffix))
			throw new WebserviceException("Please provide a suffix");

		DocumentDAO docDao = Context.get(DocumentDAO.class);
		Document doc = docDao.findById(docId);

		checkDocumentPermission(Permission.READ, user, doc.getId());
		checkDocumentPermission(Permission.WRITE, user, doc.getId());

		doc = docDao.findDocument(docId);

		if (doc.getImmutable() == 1)
			throw new WebserviceException("The document is immutable");

		if ("sign.p7m".equalsIgnoreCase(suffix))
			throw new PermissionException("You cannot upload a signature");

		Store store = Context.get(Store.class);
		String resource = store.getResourceName(doc, fileVersion, suffix);

		log.debug("Attach file {}", resource);

		store.store(content.getInputStream(), doc.getId(), resource);
	}

	@Override
	public WSDocument getDocument(String sid, long docId) throws AuthenticationException, WebserviceException,
			PersistenceException, PermissionException, UnexistingResourceException {
		User user = validateSession(sid);
		Document doc = retrieveReadableDocument(docId, user);
		checkPublished(user, doc);
		return getDoc(docId);
	}

	@Override
	public WSDocument getDocumentByCustomId(String sid, String customId)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);
		DocumentDAO docDao = Context.get(DocumentDAO.class);
		Document doc = docDao.findByCustomId(customId, user.getTenantId());
		if (doc == null)
			return null;

		checkDocumentPermission(Permission.READ, user, doc.getId());
		checkPublished(user, doc);

		return getDoc(doc.getId());
	}

	@Override
	public void lock(String sid, long docId) throws AuthenticationException, WebserviceException, PersistenceException,
			PermissionException, UnexistingResourceException {
		User user = validateSession(sid);
		DocumentDAO docDao = Context.get(DocumentDAO.class);
		Document doc = retrieveReadableDocument(docId, user);
		checkLocked(user, doc);

		checkDocumentPermission(Permission.WRITE, user, docId);

		doc = docDao.findDocument(docId);
		checkPublished(user, doc);

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId(sid);
		transaction.setEvent(DocumentEvent.LOCKED.toString());
		transaction.setComment("");
		transaction.setUser(user);

		DocumentManager documentManager = Context.get(DocumentManager.class);
		documentManager.lock(doc.getId(), AbstractDocument.DOC_LOCKED, transaction);
	}

	@Override
	public void move(String sid, long docId, long folderId) throws AuthenticationException, WebserviceException,
			PersistenceException, PermissionException, UnexistingResourceException {
		User user = validateSession(sid);
		FolderDAO fdao = Context.get(FolderDAO.class);
		long rootId = fdao.findRoot(user.getTenantId()).getId();

		if (folderId == rootId)
			throw new PermissionException("Cannot move documents in the root");

		Document doc = retrieveReadableDocument(docId, user);
		checkFolderPermission(Permission.MOVE, user, doc.getFolder().getId());

		DocumentDAO docDao = Context.get(DocumentDAO.class);
		doc = docDao.findDocument(docId);
		checkPublished(user, doc);

		FolderDAO dao = Context.get(FolderDAO.class);
		Folder folder = dao.findById(folderId);
		checkLocked(user, doc);
		checkFolderPermission(Permission.WRITE, user, folder.getId());

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId(sid);
		transaction.setEvent(DocumentEvent.MOVED.toString());
		transaction.setComment("");
		transaction.setUser(user);

		DocumentManager documentManager = Context.get(DocumentManager.class);
		documentManager.moveToFolder(doc, folder, transaction);
	}

	@Override
	public WSDocument copy(String sid, long docId, long folderId, boolean links, boolean notes, boolean security)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException,
			IOException {
		User user = validateSession(sid);
		FolderDAO fdao = Context.get(FolderDAO.class);
		long rootId = fdao.findRoot(user.getTenantId()).getId();

		if (folderId == rootId)
			throw new PermissionException("Cannot create documents in the root");

		checkFolderPermission(Permission.WRITE, user, folderId);

		DocumentDAO docDao = Context.get(DocumentDAO.class);
		Document doc = docDao.findDocument(docId);
		checkPublished(user, doc);

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId(sid);
		transaction.setEvent(DocumentEvent.COPYED.toString());
		transaction.setComment("");
		transaction.setUser(user);

		Folder folder = fdao.findFolder(folderId);

		DocumentManager documentManager = Context.get(DocumentManager.class);
		Document createdDoc = documentManager.copyToFolder(doc, folder, transaction, links, notes, security).getDocument();
		return getDoc(createdDoc.getId());
	}

	@Override
	public void rename(String sid, long docId, String name) throws AuthenticationException, WebserviceException,
			PersistenceException, PermissionException, UnexistingResourceException {
		User user = validateSession(sid);

		Document doc = retrieveReadableDocument(docId, user);
		checkFolderPermission(Permission.RENAME, user, doc.getFolder().getId());
		checkPublished(user, doc);

		DocumentManager manager = Context.get(DocumentManager.class);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId(sid);
		transaction.setUser(user);
		manager.rename(docId, name, transaction);
	}

	@Override
	public void restore(String sid, long docId, long folderId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		DocumentDAO docDao = Context.get(DocumentDAO.class);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setUser(user);
		transaction.setSessionId(sid);
		docDao.restore(docId, folderId, transaction);
	}

	@Override
	public void unlock(String sid, long docId) throws AuthenticationException, WebserviceException,
			PersistenceException, PermissionException, UnexistingResourceException {
		User user = validateSession(sid);

		Document doc = retrieveReadableDocument(docId, user);
		checkLocked(user, doc);

		// Document is already unlocked, no need to do anything else
		if (doc.getStatus() == AbstractDocument.DOC_UNLOCKED)
			return;

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId(sid);
		transaction.setEvent(DocumentEvent.UNLOCKED.toString());
		transaction.setComment("");
		transaction.setUser(user);

		DocumentManager documentManager = Context.get(DocumentManager.class);
		documentManager.unlock(docId, transaction);
	}

	@Override
	public void update(String sid, WSDocument document) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, UnexistingResourceException {
		updateDocument(sid, document);
	}

	private void updateDocument(String sid, WSDocument document) throws AuthenticationException, WebserviceException,
			PersistenceException, PermissionException, UnexistingResourceException {
		User user = validateSession(sid);

		Document doc = retrieveReadableDocument(document.getId(), user);
		checkLocked(user, doc);
		checkDocumentPermission(Permission.WRITE, user, doc.getId());
		checkPublished(user, doc);

		// Initialize the lazy loaded collections
		DocumentDAO docDao = Context.get(DocumentDAO.class);
		docDao.initialize(doc);

		long originalFolderId = doc.getFolder().getId();

		doc.setCustomId(document.getCustomId());

		DocumentManager manager = Context.get(DocumentManager.class);

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId(sid);
		transaction.setEvent(DocumentEvent.CHANGED.toString());
		transaction.setComment(document.getComment());
		transaction.setUser(user);

		manager.update(doc, WSUtil.toDocument(document), transaction);

		// If the folder is different, handle the move
		if (!document.getFolderId().equals(originalFolderId))
			move(sid, document.getId(), document.getFolderId());
	}

	@Override
	public List<WSDocument> listDocuments(String sid, long folderId, String fileName)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		return list(sid, folderId, fileName, "fileName asc", null, null);
	}

	@Override
	public List<WSDocument> list(String sid, long folderId, String fileName, String sort, Integer page, Integer max)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);
		checkFolderPermission(Permission.READ, user, folderId);

		DocumentDAO docDao = Context.get(DocumentDAO.class);

		// Get the documents in folder sorted by file name
		List<Document> docs = docDao.findByFolder(folderId, null);

		// Sort based on the given criteria
		if (StringUtils.isNotEmpty(sort))
			Collections.sort(docs, DocumentComparator.getComparator(sort));

		// Retain just those files accessible by the user that also matches the
		// file name
		docs = docs.stream().filter(doc -> mustList(doc, user, fileName)).collect(Collectors.toList());

		// In case of pagination, extract just the wanted page
		if (max != null && page != null && max < docs.size())
			docs = docs.stream().skip((page - 1) * (long) max).limit(max).collect(Collectors.toList());

		List<WSDocument> wsDocs = new ArrayList<>();
		for (Document doc : docs) {
			docDao.initialize(doc);
			wsDocs.add(WSUtil.toWSDocument(doc));
		}

		return wsDocs;
	}

	private boolean mustList(Document document, User user, String fileName) {
		try {
			checkPublished(user, document);
			checkNotArchived(document);

			if (fileName != null && !FileUtil.matches(document.getFileName(), List.of(fileName), null))
				throw new ParsingException("no match");
			return true;
		} catch (Exception t) {
			return false;
		}
	}

	@Override
	public List<WSDocument> getDocuments(String sid, List<Long> docIds)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		FolderDAO fdao = Context.get(FolderDAO.class);
		Collection<Long> folderIds = fdao.findFolderIdByUserId(user.getId(), null, true);

		DocumentDAO docDao = Context.get(DocumentDAO.class);
		List<Document> docs = docDao.findByIds(Set.copyOf(docIds), null);
		List<WSDocument> wsDocs = new ArrayList<>();
		for (int i = 0; i < docs.size(); i++) {
			try {
				checkPublished(user, docs.get(i));
				checkNotArchived(docs.get(i));
			} catch (Exception t) {
				continue;
			}
			if (user.isMemberOf(Group.GROUP_ADMIN) || folderIds.contains(docs.get(i).getFolder().getId()))
				wsDocs.add(getDoc(docs.get(i).getId()));
		}

		return wsDocs;
	}

	@Override
	public List<WSDocument> getRecentDocuments(String sid, Integer max)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);

		DocumentHistoryDAO dao = Context.get(DocumentHistoryDAO.class);
		StringBuilder query = new StringBuilder(
				"select docId from DocumentHistory where deleted=0 and (docId is not NULL) and userId=" + user.getId());
		query.append(" order by date desc");
		List<Long> records = dao.findByQuery(query.toString(), (Map<String, Object>) null, Long.class, max);

		List<Long> docIds = new ArrayList<>();

		/*
		 * Iterate over records composing the response XML document
		 */
		for (Long id : records) {
			// Discard a gridRecord if already visited
			if (!docIds.contains(id))
				docIds.add(id);
		}

		return getDocuments(sid, docIds);
	}

	@Override
	public void sendEmail(String sid, List<Long> docIds, String recipients, String subject, String message)
			throws AuthenticationException, WebserviceException, PersistenceException, IOException, MessagingException {
		User user = validateSession(sid);

		DocumentDAO docDao = Context.get(DocumentDAO.class);
		FolderDAO folderDao = Context.get(FolderDAO.class);
		ContextProperties config = Context.get().getProperties();
		Session session = SessionManager.get().get(sid);

		EMail mail = new EMail();
		mail.setTenantId(user.getTenantId());
		mail.setAccountId(-1);
		mail.setAuthor(user.getUsername());
		if (config.getBoolean(session.getTenantName() + ".smtp.userasfrom", true))
			mail.setAuthorAddress(user.getEmail());
		mail.parseRecipients(recipients);
		for (Recipient recipient : mail.getRecipients()) {
			recipient.setRead(1);
		}
		mail.setFolder("outbox");
		mail.setMessageText(message);
		mail.setSentDate(new Date());
		mail.setSubject(subject);
		mail.setUsername(user.getUsername());

		/*
		 * Only readable documents can be sent
		 */
		List<Document> docs = new ArrayList<>();
		for (long id : docIds) {
			Document doc = docDao.findById(id);
			if (doc != null && folderDao.isReadAllowed(doc.getFolder().getId(), user.getId())) {
				doc = docDao.findDocument(id);
				createAttachment(mail, doc);
				docs.add(doc);
			}
		}

		// Send the message
		EMailSender sender = new EMailSender(user.getTenantId());
		sender.send(mail);

		for (Document doc : docs) {
			try {
				checkPublished(user, doc);
			} catch (Exception t) {
				continue;
			}

			// Create the document history event
			DocumentHistory history = new DocumentHistory();
			history.setSessionId(sid);
			history.setDocument(doc);
			history.setDocId(doc.getId());
			history.setEvent(DocumentEvent.SENT.toString());
			history.setUser(user);
			history.setComment(StringUtils.abbreviate(recipients, 4000));
			history.setFilename(doc.getFileName());
			history.setVersion(doc.getVersion());
			history.setFileVersion(doc.getFileVersion());
			history.setPath(folderDao.computePathExtended(doc.getFolder().getId()));
			docDao.saveDocumentHistory(doc, history);
		}
	}

	private WSDocument getDoc(long docId) throws PersistenceException {
		DocumentDAO docDao = Context.get(DocumentDAO.class);

		Document doc = docDao.findById(docId);
		Long aliasId = null;
		String aliasFileName = null;

		// Check if it is an alias
		if (doc.getDocRef() != null) {
			aliasFileName = doc.getFileName();
			long id = doc.getDocRef();
			doc = docDao.findById(id);
			aliasId = docId;
		}

		docDao.initialize(doc);
		WSDocument document = WSUtil.toWSDocument(doc);

		if (aliasId != null)
			document.setDocRef(aliasId);
		if (StringUtils.isNotEmpty(aliasFileName))
			document.setFileName(aliasFileName);

		return document;
	}

	private void createAttachment(EMail email, Document doc) throws IOException {
		EMailAttachment att = new EMailAttachment();
		att.setIcon(doc.getIcon());
		Store store = Context.get(Store.class);
		String resource = store.getResourceName(doc, null, null);
		att.setData(store.getBytes(doc.getId(), resource));
		att.setFileName(doc.getFileName());
		String extension = doc.getFileExtension();
		att.setMimeType(MimeType.get(extension));

		email.addAttachment(2 + email.getAttachments().size(), att);
	}

	@Override
	public WSDocument createAlias(String sid, long docId, long folderId, String type)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);

		FolderDAO fdao = Context.get(FolderDAO.class);
		long rootId = fdao.findRoot(user.getTenantId()).getId();

		if (folderId == rootId)
			throw new PermissionException("Cannot create alias in the root");

		DocumentDAO docDao = Context.get(DocumentDAO.class);
		Document originalDoc = docDao.findById(docId);
		checkDocumentPermission(Permission.DOWNLOAD, user, docId);
		checkFolderPermission(Permission.WRITE, user, folderId);

		FolderDAO mdao = Context.get(FolderDAO.class);
		Folder folder = mdao.findById(folderId);
		if (folder == null)
			throw new WebserviceException("error - folder not found");

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId(sid);
		transaction.setEvent(DocumentEvent.SHORTCUT_STORED.toString());
		transaction.setComment("");
		transaction.setUser(user);

		DocumentManager documentManager = Context.get(DocumentManager.class);

		Document doc = documentManager.createAlias(originalDoc, folder, type, transaction);

		checkPublished(user, doc);

		return WSUtil.toWSDocument(doc);
	}

	@Override
	public void reindex(String sid, long docId, String content)
			throws ParsingException, AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		DocumentManager documentManager = Context.get(DocumentManager.class);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId(sid);
		transaction.setUser(user);
		documentManager.index(docId, content, transaction);
	}

	@Override
	public List<WSDocument> getAliases(String sid, long docId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		Collection<Long> folderIds = null;
		if (!user.isMemberOf(Group.GROUP_ADMIN)) {
			FolderDAO fdao = Context.get(FolderDAO.class);
			folderIds = fdao.findFolderIdByUserId(user.getId(), null, true);
		}

		DocumentDAO docDao = Context.get(DocumentDAO.class);
		List<Document> docs = new ArrayList<>();
		if (user.isMemberOf(Group.GROUP_ADMIN))
			docs = docDao.findByWhere("_entity.docRef=" + docId, null, null);
		else if (folderIds != null) {
			String idsStr = folderIds.toString().replace('[', '(').replace(']', ')');
			docs = docDao.findByWhere("_entity.docRef=" + docId + " and _entity.id in " + idsStr, null, null);
		}

		List<WSDocument> wsDocs = new ArrayList<>();
		for (int i = 0; i < docs.size(); i++) {
			docDao.initialize(docs.get(i));
			if (user.isMemberOf(Group.GROUP_ADMIN)
					|| (folderIds != null && folderIds.contains(docs.get(i).getFolder().getId())))
				wsDocs.add(WSUtil.toWSDocument(docs.get(i)));
		}

		return wsDocs;
	}

	@Override
	public long upload(String sid, Long docId, Long folderId, boolean release, String filename, String language,
			DataHandler content) throws AuthenticationException, WebserviceException, PersistenceException,
			PermissionException, IOException {
		validateSession(sid);

		if (docId != null) {
			checkout(sid, docId);
			checkin(sid, docId, "", filename, release, content);
			return docId;
		} else {
			WSDocument doc = new WSDocument();
			doc.setFileName(filename);
			doc.setFolderId(folderId);
			if (StringUtils.isEmpty(language))
				doc.setLanguage("en");
			else
				doc.setLanguage(language);

			return create(sid, doc, content).getId();
		}

	}

	@Override
	public WSLink link(String sid, long doc1, long doc2, String type) throws AuthenticationException,
			WebserviceException, PersistenceException, PermissionException, UnexistingResourceException {
		User user = validateSession(sid);

		DocumentLinkDAO linkDao = Context.get(DocumentLinkDAO.class);
		DocumentLink link = linkDao.findByDocIdsAndType(doc1, doc2, type);

		Document document1 = retrieveReadableDocument(doc1, user);
		Document document2 = retrieveReadableDocument(doc2, user);

		checkDocumentPermission(Permission.WRITE, user, document2.getId());

		if (link == null) {
			// The link doesn't exist and must be created
			link = new DocumentLink();
			link.setTenantId(document1.getTenantId());
			link.setDocument1(document1);
			link.setDocument2(document2);
			link.setType(type);
			linkDao.store(link);

			WSLink lnk = new WSLink();
			lnk.setId(link.getId());
			lnk.setDoc1(doc1);
			lnk.setDoc2(doc2);
			lnk.setType(type);
			return lnk;
		} else {
			throw new WebserviceException("Documents already linked");
		}
	}

	@Override
	public List<WSLink> getLinks(String sid, long docId)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);

		DocumentLinkDAO linkDao = Context.get(DocumentLinkDAO.class);

		checkDocumentPermission(Permission.READ, user, docId);

		List<DocumentLink> links = linkDao.findByDocId(docId);
		List<WSLink> lnks = new ArrayList<>();
		for (DocumentLink link : links) {
			WSLink lnk = new WSLink();
			lnk.setId(link.getId());
			lnk.setDoc1(link.getDocument1().getId());
			lnk.setDoc2(link.getDocument2().getId());
			lnk.setType(link.getType());
			lnks.add(lnk);
		}

		return lnks;
	}

	@Override
	public void deleteLink(String sid, long id)
			throws AuthenticationException, WebserviceException, PersistenceException {
		validateSession(sid);
		DocumentLinkDAO linkDao = Context.get(DocumentLinkDAO.class);
		linkDao.delete(id);
	}

	@Override
	public String getExtractedText(String sid, long docId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		validateSession(sid);
		SearchEngine indexer = Context.get(SearchEngine.class);
		return indexer.getHit(docId).getContent();
	}

	@Override
	public String createDownloadTicket(String sid, long docId, String suffix, Integer expireHours, String expireDate,
			Integer maxDownloads)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		validateSession(sid);

		DocumentManager manager = Context.get(DocumentManager.class);
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSession(SessionManager.get().get(sid));

		Ticket ticket = new Ticket();
		ticket.setType(Ticket.DOWNLOAD);
		ticket.setTenantId(transaction.getTenantId());
		ticket.setDocId(docId);
		ticket.setSuffix(suffix);
		ticket.setExpireHours(expireHours);
		ticket.setExpired(convertStringToDate(expireDate));
		ticket.setMaxCount(maxDownloads);

		ticket = manager.createTicket(ticket, transaction);

		return ticket.getUrl();
	}

	@Override
	public String createViewTicket(String sid, long docId, String suffix, Integer expireHours, String expireDate,
			Integer maxDownloads, Integer maxViews)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		validateSession(sid);

		DocumentManager manager = Context.get(DocumentManager.class);
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSession(SessionManager.get().get(sid));

		Ticket ticket = new Ticket();
		ticket.setType(Ticket.VIEW);
		ticket.setTenantId(transaction.getTenantId());
		ticket.setDocId(docId);
		ticket.setSuffix(suffix);
		ticket.setExpireHours(expireHours);
		ticket.setExpired(convertStringToDate(expireDate));
		ticket.setMaxCount(maxDownloads);
		ticket.setMaxViews(maxViews);

		ticket = manager.createTicket(ticket, transaction);

		return ticket.getUrl();
	}

	@Override
	public void setPassword(String sid, long docId, String password) throws AuthenticationException,
			WebserviceException, PersistenceException, PermissionException, UnexistingResourceException {
		User user = validateSession(sid);

		Document doc = retrieveReadableDocument(docId, user);

		checkFolderPermission(Permission.PASSWORD, user, doc.getFolder().getId());

		DocumentDAO dao = Context.get(DocumentDAO.class);
		doc = dao.findDocument(docId);

		Session session = SessionManager.get().get(sid);

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSession(session);
		transaction.setComment("");

		dao.setPassword(doc.getId(), password, transaction);
	}

	@Override
	public void unsetPassword(String sid, long docId, String currentPassword) throws AuthenticationException,
			WebserviceException, PersistenceException, PermissionException, UnexistingResourceException {
		User user = validateSession(sid);
		Document doc = retrieveReadableDocument(docId, user);

		checkFolderPermission(Permission.PASSWORD, user, doc.getFolder().getId());

		DocumentDAO dao = Context.get(DocumentDAO.class);
		doc = dao.findDocument(docId);

		Session session = SessionManager.get().get(sid);

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSession(session);
		transaction.setComment("");

		if (doc.isGranted(currentPassword))
			dao.unsetPassword(doc.getId(), transaction);
		else
			throw new PermissionException("You cannot access the document");
	}

	@Override
	public boolean unprotect(String sid, long docId, String password)
			throws PersistenceException, AuthenticationException, WebserviceException {
		validateSession(sid);
		DocumentManager manager = Context.get(DocumentManager.class);
		DocumentDAO dao = Context.get(DocumentDAO.class);
		Document doc = dao.findDocument(docId);
		return manager.unprotect(sid, doc.getId(), password);
	}

	@Override
	public WSNote addNote(String sid, long docId, String note) throws AuthenticationException, WebserviceException,
			PersistenceException, PermissionException, UnexistingResourceException {
		User user = validateSession(sid);
		WSDocument document = getDocument(sid, docId);
		if (document == null)
			throw new PermissionException(DOCUMENT_WITH_ID + docId + NOT_FOUND_OR_NOT_ACCESSIBLE);

		DocumentNote newNote = new DocumentNote();
		newNote.setDocId(document.getId());
		newNote.setMessage(note);
		newNote.setUserId(user.getId());
		newNote.setUsername(user.getFullName());

		DocumentDAO ddao = Context.get(DocumentDAO.class);
		Document doc = ddao.findDocument(docId);
		newNote.setFileName(doc.getFileName());

		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId(sid);
		transaction.setUser(user);

		DocumentNoteDAO dao = Context.get(DocumentNoteDAO.class);
		dao.store(newNote, transaction);

		return WSNote.fromDocumentNote(newNote);
	}

	@Override
	public WSNote saveNote(String sid, long docId, WSNote wsNote) throws AuthenticationException, WebserviceException,
			PersistenceException, PermissionException, UnexistingResourceException {
		User user = validateSession(sid);
		WSDocument document = getDocument(sid, docId);
		if (document == null)
			throw new PermissionException(DOCUMENT_WITH_ID + docId + NOT_FOUND_OR_NOT_ACCESSIBLE);

		DocumentNoteDAO dao = Context.get(DocumentNoteDAO.class);
		DocumentNote note = dao.findById(wsNote.getId());
		if (note == null) {
			note = new DocumentNote();
			note.setTenantId(user.getTenantId());
			note.setDocId(docId);
			note.setUserId(user.getId());
			note.setUsername(user.getUsername());
			note.setDate(new Date());
		}

		note.setPage(wsNote.getPage());
		note.setMessage(wsNote.getMessage());
		note.setColor(wsNote.getColor());
		note.setTop(wsNote.getTop());
		note.setLeft(wsNote.getLeft());
		note.setWidth(wsNote.getWidth());
		note.setHeight(wsNote.getHeight());

		DocumentDAO ddao = Context.get(DocumentDAO.class);
		Document doc = ddao.findDocument(docId);
		note.setFileName(doc.getFileName());

		if (note.getId() == 0L) {
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSessionId(sid);
			transaction.setUser(user);
			dao.store(note, transaction);
		} else
			dao.store(note);

		return WSNote.fromDocumentNote(note);
	}

	@Override
	public void deleteNote(String sid, long noteId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		DocumentNoteDAO dao = Context.get(DocumentNoteDAO.class);
		DocumentNote note = dao.findById(noteId);
		if (note == null)
			return;

		if (user.isMemberOf(Group.GROUP_ADMIN) || user.getId() == note.getUserId())
			dao.delete(note.getId());
	}

	@Override
	public String deleteVersion(String sid, long docId, String version)
			throws AuthenticationException, WebserviceException, PersistenceException {
		validateSession(sid);
		VersionDAO dao = Context.get(VersionDAO.class);
		Version ver = dao.findByVersion(docId, version);

		DocumentManager manager = Context.get(DocumentManager.class);

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId(sid);

		Version latestVersion = manager.deleteVersion(ver.getId(), transaction);
		return latestVersion.getVersion();
	}

	@Override
	public List<WSNote> getNotes(String sid, long docId) throws AuthenticationException, WebserviceException,
			PersistenceException, PermissionException, UnexistingResourceException {
		validateSession(sid);
		WSDocument document = getDocument(sid, docId);
		if (document == null)
			throw new WebserviceException(DOCUMENT_WITH_ID + docId + NOT_FOUND_OR_NOT_ACCESSIBLE);

		DocumentNoteDAO dao = Context.get(DocumentNoteDAO.class);
		List<DocumentNote> notes = dao.findByDocId(docId, document.getFileVersion());
		List<WSNote> wsNotes = new ArrayList<>();
		if (notes != null)
			for (DocumentNote note : notes)
				wsNotes.add(WSNote.fromDocumentNote(note));
		return wsNotes;
	}

	@Override
	public WSRating rateDocument(String sid, long docId, int vote) throws AuthenticationException, WebserviceException,
			PersistenceException, PermissionException, UnexistingResourceException {
		User user = validateSession(sid);
		WSDocument document = getDocument(sid, docId);
		if (document == null)
			throw new WebserviceException(DOCUMENT_WITH_ID + docId + NOT_FOUND_OR_NOT_ACCESSIBLE);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId(sid);
		transaction.setUser(user);

		RatingDAO ratingDao = Context.get(RatingDAO.class);

		Rating rating = ratingDao.findByDocIdAndUserId(docId, user.getId());
		if (rating == null) {
			rating = new Rating();
			rating.setDocId(docId);
			rating.setUserId(user.getId());
			rating.setUsername(user.getFullName());
		}
		rating.setVote(vote);

		ratingDao.store(rating, transaction);
		return WSRating.fromRating(rating);
	}

	@Override
	public List<WSRating> getRatings(String sid, long docId) throws AuthenticationException, WebserviceException,
			PersistenceException, PermissionException, UnexistingResourceException {
		validateSession(sid);
		WSDocument document = getDocument(sid, docId);
		if (document == null)
			throw new WebserviceException(DOCUMENT_WITH_ID + docId + NOT_FOUND_OR_NOT_ACCESSIBLE);

		RatingDAO ratingDao = Context.get(RatingDAO.class);
		List<Rating> ratings = ratingDao.findByDocId(docId);
		List<WSRating> wsRatings = new ArrayList<>();
		if (ratings != null)
			for (Rating rating : ratings)
				wsRatings.add(WSRating.fromRating(rating));

		return wsRatings;
	}

	@Override
	public void replaceFile(String sid, long docId, String fileVersion, String comment, DataHandler content)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException, IOException,
			UnexistingResourceException {
		User user = validateSession(sid);

		Document doc = retrieveReadableDocument(docId, user);
		if (doc.getImmutable() == 1)
			throw new PermissionException(THE_DOCUMENT + docId + IS_IMMUTABLE);

		checkDocumentPermission(Permission.WRITE, user, doc.getId());

		DocumentDAO ddao = Context.get(DocumentDAO.class);
		doc = ddao.findDocument(docId);
		if (doc.getStatus() != AbstractDocument.DOC_UNLOCKED)
			throw new PermissionException(THE_DOCUMENT + docId + IS_LOCKED);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setComment(comment);
		transaction.setUser(user);
		transaction.setSession(SessionManager.get().get(sid));

		DocumentManager manager = Context.get(DocumentManager.class);
		manager.replaceFile(doc.getId(), fileVersion, content.getInputStream(), transaction);
		log.info("Replaced fileVersion {} of document {}", fileVersion, doc);
	}

	@Override
	public void promoteVersion(String sid, long docId, String version) throws AuthenticationException,
			WebserviceException, PersistenceException, PermissionException, IOException, UnexistingResourceException {
		User user = validateSession(sid);
		Document doc = retrieveReadableDocument(docId, user);

		checkDocumentPermission(Permission.WRITE, user, doc.getId());

		DocumentDAO ddao = Context.get(DocumentDAO.class);
		doc = ddao.findDocument(docId);
		if (doc.getStatus() != AbstractDocument.DOC_UNLOCKED)
			throw new PermissionException(THE_DOCUMENT + docId + IS_LOCKED);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setUser(user);
		transaction.setSession(SessionManager.get().get(sid));

		DocumentManager manager = Context.get(DocumentManager.class);
		manager.promoteVersion(doc.getId(), version, transaction);

		log.info("Promoted version {} of document {}", version, doc);
	}

	@Override
	public WSDocument getVersion(String sid, long docId, String version) throws AuthenticationException,
			WebserviceException, PersistenceException, PermissionException, UnexistingResourceException {
		User user = validateSession(sid);
		Document doc = retrieveReadableDocument(docId, user);
		checkPublished(user, doc);

		VersionDAO versDao = Context.get(VersionDAO.class);
		Version ver = versDao.findByVersion(docId, version);
		if (ver != null) {
			versDao.initialize(ver);
			WSDocument wsVersion = WSUtil.toWSDocument(ver);
			wsVersion.setComment(ver.getComment());
			return wsVersion;
		}
		return null;
	}

	@Override
	public List<WSDocument> getVersions(String sid, long docId) throws AuthenticationException, WebserviceException,
			PersistenceException, PermissionException, UnexistingResourceException {
		User user = validateSession(sid);
		Document doc = retrieveReadableDocument(docId, user);

		checkPublished(user, doc);

		VersionDAO versDao = Context.get(VersionDAO.class);
		List<Version> versions = versDao.findByDocId(doc.getId());
		List<WSDocument> wsVersions = new ArrayList<>();
		for (Version version : versions) {
			versDao.initialize(version);
			WSDocument wsVersion = WSUtil.toWSDocument(version);
			wsVersion.setComment(version.getComment());
			wsVersions.add(wsVersion);
		}

		return wsVersions;
	}

	private Document retrieveReadableDocument(long docId, User user)
			throws PersistenceException, PermissionException, UnexistingResourceException {
		DocumentDAO docDao = Context.get(DocumentDAO.class);
		Document doc = docDao.findById(docId);
		if (doc == null)
			throw new UnexistingResourceException(user.getUsername(), "Document " + docId);
		checkDocumentPermission(Permission.READ, user, docId);
		doc = docDao.findDocument(docId);
		return doc;
	}

	@Override
	public void setAccessControlList(String sid, long docId, List<WSAccessControlEntry> acl)
			throws PersistenceException, PermissionException, AuthenticationException, WebserviceException {
		User sessionUser = validateSession(sid);

		DocumentDAO documentDao = Context.get(DocumentDAO.class);
		// Check if the session user has the Security Permission of this
		// document
		if (!documentDao.isPermissionAllowed(Permission.SECURITY, docId, sessionUser.getId()))
			throw new PermissionException(sessionUser.getUsername(), "Document " + docId, Permission.SECURITY);

		Document document = documentDao.findById(docId);
		documentDao.initialize(document);
		document.getAccessControlList().clear();
		for (WSAccessControlEntry wsAcwe : acl)
			document.addAccessControlEntry(WSUtil.toAccessControlEntry(wsAcwe));

		DocumentHistory history = new DocumentHistory();
		history.setEvent(DocumentEvent.PERMISSION.toString());
		history.setSession(SessionManager.get().get(sid));
		documentDao.store(document, history);

	}

	@Override
	public List<WSAccessControlEntry> getAccessControlList(String sid, long docId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		validateSession(sid);

		List<WSAccessControlEntry> acl = new ArrayList<>();
		DocumentDAO documentDao = Context.get(DocumentDAO.class);

		Document document = documentDao.findById(docId);
		documentDao.initialize(document);

		for (AccessControlEntry ace : document.getAccessControlList())
			acl.add(WSUtil.toWSAccessControlEntry(ace));

		return acl;
	}

	@Override
	public boolean isRead(String sid, long docId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return isGranted(sid, docId, Permission.READ.getName());
	}

	@Override
	public boolean isWrite(String sid, long docId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return isGranted(sid, docId, Permission.WRITE.getName());
	}

	@Override
	public boolean isDownload(String sid, long docId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return isGranted(sid, docId, Permission.DOWNLOAD.getName());
	}

	@Override
	public boolean isGranted(String sid, long docId, String permission)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		try {
			checkDocumentPermission(Permission.valueOf(permission.toUpperCase()), user, docId);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			return false;
		}
		return true;
	}
}