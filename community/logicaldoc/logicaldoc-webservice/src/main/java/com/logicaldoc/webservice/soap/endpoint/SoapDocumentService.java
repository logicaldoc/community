package com.logicaldoc.webservice.soap.endpoint;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.activation.DataHandler;

import org.apache.commons.lang.StringUtils;
import org.apache.cxf.jaxrs.ext.multipart.InputStreamDataSource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailAttachment;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.conversion.FormatConverterManager;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentLink;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.document.DocumentNote;
import com.logicaldoc.core.document.History;
import com.logicaldoc.core.document.Rating;
import com.logicaldoc.core.document.Version;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.document.dao.DocumentLinkDAO;
import com.logicaldoc.core.document.dao.DocumentNoteDAO;
import com.logicaldoc.core.document.dao.HistoryDAO;
import com.logicaldoc.core.document.dao.RatingDAO;
import com.logicaldoc.core.document.dao.VersionDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.searchengine.SearchEngine;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.core.ticket.Ticket;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.MimeType;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.webservice.AbstractService;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSLink;
import com.logicaldoc.webservice.model.WSNote;
import com.logicaldoc.webservice.model.WSRating;
import com.logicaldoc.webservice.model.WSUtil;
import com.logicaldoc.webservice.soap.DocumentService;

/**
 * Document Web Service Implementation (SOAP)
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 5.2
 */
public class SoapDocumentService extends AbstractService implements DocumentService {

	protected static Logger log = LoggerFactory.getLogger(SoapDocumentService.class);

	@Override
	public WSDocument create(String sid, WSDocument document, DataHandler content) throws Exception {
		User user = validateSession(sid);
		checkWriteEnable(user, document.getFolderId());

		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Folder folder = fdao.findById(document.getFolderId());

		long rootId = fdao.findRoot(user.getTenantId()).getId();

		if (folder == null) {
			log.error("Folder " + document.getFolderId() + " not found");
			throw new Exception("error - folder not found");
		} else if (folder.getId() == rootId) {
			log.error("Cannot add documents in the root");
			throw new Exception("Cannot add documents in the root");
		}
		fdao.initialize(folder);

		Document doc = WSUtil.toDocument(document);
		doc.setTenantId(user.getTenantId());

		// Create the document history event
		History transaction = new History();
		transaction.setSessionId(sid);
		transaction.setEvent(DocumentEvent.STORED.toString());
		transaction.setComment(document.getComment());
		transaction.setUser(user);

		// Get file to upload inputStream
		InputStream stream = content.getInputStream();

		DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		doc = documentManager.create(stream, doc, transaction);
		return WSUtil.toWSDocument(doc);
	}

	@Override
	public void checkin(String sid, long docId, String comment, String filename, boolean release, DataHandler content)
			throws Exception {
		User user = validateSession(sid);
		DocumentDAO ddao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document document = ddao.findById(docId);
		if (document.getImmutable() == 1)
			throw new Exception("The document is immutable");
		Folder folder = document.getFolder();

		checkWriteEnable(user, folder.getId());

		if (document.getStatus() == Document.DOC_CHECKED_OUT
				&& (user.getId() == document.getLockUserId() || user.isMemberOf("admin"))) {
			try {
				// Get file to upload inputStream
				InputStream stream = content.getInputStream();

				// Create the document history event
				History transaction = new History();
				transaction.setSessionId(sid);
				transaction.setEvent(DocumentEvent.CHECKEDIN.toString());
				transaction.setUser(user);
				transaction.setComment(comment);

				// checkin the document; throws an exception if
				// something goes wrong
				DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
				documentManager.checkin(document.getId(), stream, filename, release, null, transaction);

				/* create positive log message */
				log.info("Document " + docId + " checked in");
			} catch (Exception e) {
				log.error(e.getMessage(), e);
				throw new Exception(e);
			}
		} else {
			throw new Exception("document not checked in");
		}
	}

	@Override
	public void checkout(String sid, long docId) throws Exception {
		User user = validateSession(sid);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = docDao.findById(docId);
		if (doc.getImmutable() == 1)
			throw new Exception("The document is immutable");

		if (doc.getStatus() != Document.DOC_UNLOCKED)
			throw new Exception("The document is locked or already checked out");

		checkWriteEnable(user, doc.getFolder().getId());
		checkDownloadEnable(user, doc.getFolder().getId());
		checkPublished(user, doc);

		// Create the document history event
		History transaction = new History();
		transaction.setSessionId(sid);
		transaction.setEvent(DocumentEvent.CHECKEDOUT.toString());
		transaction.setComment("");
		transaction.setUser(user);

		DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		documentManager.checkout(docId, transaction);
	}

	@Override
	public void delete(String sid, long docId) throws Exception {
		User user = validateSession(sid);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = docDao.findById(docId);
		checkLocked(user, doc);
		checkPermission(Permission.DELETE, user, doc.getFolder().getId());
		checkPublished(user, doc);

		// Create the document history event
		History transaction = new History();
		transaction.setSessionId(sid);
		transaction.setEvent(DocumentEvent.DELETED.toString());
		transaction.setComment("");
		transaction.setUser(user);

		docDao.delete(docId, transaction);
	}

	private void checkLocked(User user, Document doc) throws Exception {
		if (user.isMemberOf("admin"))
			return;

		if (doc.getImmutable() == 1)
			throw new Exception("The document " + doc.getId() + " is immutable");

		if (doc.getStatus() != Document.DOC_UNLOCKED && user.getId() != doc.getLockUserId())
			throw new Exception("The document " + doc.getId() + " is locked");
	}

	@Override
	public DataHandler getContent(String sid, long docId) throws Exception {
		return getVersionContent(sid, docId, null);
	}

	@Override
	public DataHandler getVersionContent(String sid, long docId, String version) throws Exception {
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = docDao.findById(docId);

		if (doc.getDocRef() != null)
			doc = docDao.findById(doc.getDocRef());

		String fileVersion = null;
		if (version != null) {
			VersionDAO vDao = (VersionDAO) Context.get().getBean(VersionDAO.class);
			Version v = vDao.findByVersion(docId, version);
			fileVersion = v.getFileVersion();
		}

		return getResource(sid, docId, fileVersion, null);
	}

	@Override
	public DataHandler getResource(String sid, long docId, String fileVersion, String suffix) throws Exception {
		User user = validateSession(sid);
		try {
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document doc = docDao.findById(docId);
			checkReadEnable(user, doc.getFolder().getId());
			checkDownloadEnable(user, doc.getFolder().getId());
			checkPublished(user, doc);

			if (doc.getDocRef() != null)
				doc = docDao.findById(doc.getDocRef());

			if (doc.isPasswordProtected()) {
				Session session = SessionManager.get().get(sid);
				if (!session.getUnprotectedDocs().containsKey(doc.getId()))
					throw new Exception("The document " + doc + " is protected by a password");
			}

			Storer storer = (Storer) Context.get().getBean(Storer.class);
			String resourceName = storer.getResourceName(doc, fileVersion, suffix);

			if (!storer.exists(doc.getId(), resourceName)) {
				throw new FileNotFoundException(resourceName);
			}

			log.debug("Attach file " + resourceName);

			String fileName = doc.getFileName();
			if (StringUtils.isNotEmpty(suffix))
				fileName = suffix;
			String mime = MimeType.getByFilename(fileName);
			DataHandler content = new DataHandler(new InputStreamDataSource(
					storer.getStream(doc.getId(), resourceName), mime));
			return content;
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public void createPdf(String sid, long docId, String fileVersion) throws Exception {
		User user = validateSession(sid);
		try {
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document doc = docDao.findById(docId);
			checkReadEnable(user, doc.getFolder().getId());

			if (doc.getDocRef() != null)
				doc = docDao.findById(doc.getDocRef());

			FormatConverterManager manager = (FormatConverterManager) Context.get().getBean(
					FormatConverterManager.class);
			manager.convertToPdf(doc, fileVersion, sid);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public void uploadResource(String sid, long docId, String fileVersion, String suffix, DataHandler content)
			throws Exception {
		User user = validateSession(sid);

		try {
			if (StringUtils.isEmpty(suffix))
				throw new Exception("Please provide a suffix");

			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document doc = docDao.findById(docId);
			checkReadEnable(user, doc.getFolder().getId());
			checkWriteEnable(user, doc.getFolder().getId());

			if (doc.getDocRef() != null)
				doc = docDao.findById(doc.getDocRef());

			if (doc.getImmutable() == 1)
				throw new Exception("The document is immutable");

			if ("sign.p7m".equals(suffix.toLowerCase()))
				throw new Exception("You cannot upload a signature");

			Storer storer = (Storer) Context.get().getBean(Storer.class);
			String resource = storer.getResourceName(doc, fileVersion, suffix);

			log.debug("Attach file " + resource);

			storer.store(content.getInputStream(), doc.getId(), resource);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public WSDocument getDocument(String sid, long docId) throws Exception {
		User user = validateSession(sid);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = docDao.findById(docId);
		if (doc == null)
			return null;
		checkReadEnable(user, doc.getFolder().getId());
		checkPublished(user, doc);

		docDao.initialize(doc);
		return WSUtil.toWSDocument(doc);
	}

	@Override
	public WSDocument getDocumentByCustomId(String sid, String customId) throws Exception {
		User user = validateSession(sid);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = docDao.findByCustomId(customId, user.getTenantId());
		if (doc == null)
			return null;
		checkReadEnable(user, doc.getFolder().getId());
		checkPublished(user, doc);

		docDao.initialize(doc);
		return WSUtil.toWSDocument(doc);
	}

	@Override
	public boolean isReadable(String sid, long docId) throws Exception {
		User user = validateSession(sid);

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = docDao.findById(docId);
		if (doc == null)
			return false;

		checkReadEnable(user, doc.getFolder().getId());
		return true;
	}

	@Override
	public void lock(String sid, long docId) throws Exception {
		User user = validateSession(sid);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = docDao.findById(docId);
		checkLocked(user, doc);
		checkWriteEnable(user, doc.getFolder().getId());
		checkPublished(user, doc);

		// Create the document history event
		History transaction = new History();
		transaction.setSessionId(sid);
		transaction.setEvent(DocumentEvent.LOCKED.toString());
		transaction.setComment("");
		transaction.setUser(user);

		DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		documentManager.lock(docId, Document.DOC_LOCKED, transaction);
	}

	@Override
	public void move(String sid, long docId, long folderId) throws Exception {
		User user = validateSession(sid);

		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		long rootId = fdao.findRoot(user.getTenantId()).getId();

		if (folderId == rootId) {
			log.error("Cannot move documents in the root");
			throw new Exception("Cannot move documents in the root");
		}

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = docDao.findById(docId);
		checkPermission(Permission.DELETE, user, doc.getFolder().getId());
		checkPublished(user, doc);

		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Folder folder = dao.findById(folderId);
		checkLocked(user, doc);
		checkWriteEnable(user, folder.getId());

		// Create the document history event
		History transaction = new History();
		transaction.setSessionId(sid);
		transaction.setEvent(DocumentEvent.MOVED.toString());
		transaction.setComment("");
		transaction.setUser(user);

		DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		documentManager.moveToFolder(doc, folder, transaction);
	}

	@Override
	public void rename(String sid, long docId, String name) throws Exception {
		User user = validateSession(sid);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = docDao.findById(docId);
		checkPermission(Permission.RENAME, user, doc.getFolder().getId());
		checkPublished(user, doc);

		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);

		History transaction = new History();
		transaction.setSessionId(sid);
		transaction.setUser(user);
		manager.rename(doc, name, transaction);
	}

	@Override
	public void renameFile(String sid, long docId, String name) throws Exception {
		User user = validateSession(sid);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = docDao.findById(docId);
		checkPermission(Permission.RENAME, user, doc.getFolder().getId());
		checkPublished(user, doc);
		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);

		History transaction = new History();
		transaction.setSessionId(sid);
		transaction.setUser(user);
		manager.rename(doc, name, transaction);
	}

	@Override
	public void restore(String sid, long docId, long folderId) throws Exception {
		User user = validateSession(sid);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

		History transaction = new History();
		transaction.setUser(user);
		transaction.setSessionId(sid);
		docDao.restore(docId, folderId, transaction);
	}

	@Override
	public void unlock(String sid, long docId) throws Exception {
		User user = validateSession(sid);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = docDao.findById(docId);
		checkLocked(user, doc);

		// Document is already unlocked, no need to do anything else
		if (doc.getStatus() == Document.DOC_UNLOCKED)
			return;

		// Create the document history event
		History transaction = new History();
		transaction.setSessionId(sid);
		transaction.setEvent(DocumentEvent.UNLOCKED.toString());
		transaction.setComment("");
		transaction.setUser(user);

		DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		documentManager.unlock(docId, transaction);
	}

	@Override
	public void update(String sid, WSDocument document) throws Exception {
		updateDocument(sid, document);
	}

	private void updateDocument(String sid, WSDocument document) throws Exception {
		User user = validateSession(sid);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = docDao.findById(document.getId());
		if (doc == null)
			throw new Exception("unexisting document " + document.getId());
		checkLocked(user, doc);
		checkWriteEnable(user, doc.getFolder().getId());
		checkPublished(user, doc);

		// Initialize the lazy loaded collections
		docDao.initialize(doc);
		doc.setCustomId(document.getCustomId());

		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);

		// Create the document history event
		History transaction = new History();
		transaction.setSessionId(sid);
		transaction.setEvent(DocumentEvent.CHANGED.toString());
		transaction.setComment(document.getComment());
		transaction.setUser(user);

		manager.update(doc, WSUtil.toDocument(document), transaction);
	}

	@Override
	public WSDocument[] listDocuments(String sid, long folderId, String fileName) throws Exception {
		User user = validateSession(sid);
		checkReadEnable(user, folderId);

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		List<Document> docs = docDao.findByFolder(folderId, null);
		if (docs.size() > 1)
			Collections.sort(docs, new Comparator<Document>() {
				@Override
				public int compare(Document o1, Document o2) {
					return o1.getFileName().compareTo(o2.getFileName());
				}
			});

		List<WSDocument> wsDocs = new ArrayList<WSDocument>();
		for (Document doc : docs) {
			try {
				checkPublished(user, doc);
				checkNotArchived(doc);
			} catch (Throwable t) {
				continue;
			}

			if (fileName != null && !FileUtil.matches(doc.getFileName(), new String[] { fileName }, null))
				continue;

			docDao.initialize(doc);
			wsDocs.add(WSUtil.toWSDocument(doc));
		}

		return wsDocs.toArray(new WSDocument[0]);
	}

	@Override
	public WSDocument[] getDocuments(String sid, Long[] docIds) throws Exception {
		User user = validateSession(sid);
		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Collection<Long> folderIds = fdao.findFolderIdByUserId(user.getId(), null, true);

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		List<Document> docs = docDao.findByIds(docIds, null);
		List<WSDocument> wsDocs = new ArrayList<WSDocument>();
		for (int i = 0; i < docs.size(); i++) {
			try {
				checkPublished(user, docs.get(i));
				checkNotArchived(docs.get(i));
			} catch (Throwable t) {
				continue;
			}
			docDao.initialize(docs.get(i));
			if (user.isMemberOf("admin") || folderIds.contains(docs.get(i).getFolder().getId()))
				wsDocs.add(WSUtil.toWSDocument(docs.get(i)));
		}

		return wsDocs.toArray(new WSDocument[0]);
	}

	@Override
	public WSDocument[] getVersions(String sid, long docId) throws Exception {
		User user = validateSession(sid);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = docDao.findById(docId);
		if (doc == null)
			throw new Exception("unexisting document " + docId);

		checkReadEnable(user, doc.getFolder().getId());
		checkPublished(user, doc);

		VersionDAO versDao = (VersionDAO) Context.get().getBean(VersionDAO.class);
		List<Version> versions = versDao.findByDocId(docId);
		WSDocument[] wsVersions = new WSDocument[versions.size()];
		for (int i = 0; i < versions.size(); i++) {
			versDao.initialize(versions.get(i));
			wsVersions[i] = WSUtil.toWSDocument(versions.get(i));
			wsVersions[i].setComment(versions.get(i).getComment());
		}

		return wsVersions;
	}

	@Override
	public WSDocument[] getRecentDocuments(String sid, Integer max) throws Exception {
		User user = validateSession(sid);

		HistoryDAO dao = (HistoryDAO) Context.get().getBean(HistoryDAO.class);
		StringBuffer query = new StringBuffer(
				"select docId from History where deleted=0 and (docId is not NULL) and userId=" + user.getId());
		query.append(" order by date desc");
		List<Object> records = (List<Object>) dao.findByQuery(query.toString(), null, max);

		Set<Long> docIds = new HashSet<Long>();

		/*
		 * Iterate over records composing the response XML document
		 */
		for (Object record : records) {
			Long id = (Long) record;
			// Discard a record if already visited
			if (docIds.contains(id))
				continue;
			else
				docIds.add(id);
		}

		return getDocuments(sid, docIds.toArray(new Long[0]));
	}

	@Override
	public void sendEmail(String sid, Long[] docIds, String recipients, String subject, String message)
			throws Exception {
		User user = validateSession(sid);

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		ContextProperties config = (ContextProperties) Context.get().getProperties();
		Session session = SessionManager.get().get(sid);

		EMail mail;
		try {
			mail = new EMail();
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
			List<Document> docs = new ArrayList<Document>();
			if (docIds != null && docIds.length > 0) {
				for (long id : docIds) {
					Document doc = docDao.findById(id);
					try {
						checkPublished(user, doc);
					} catch (Throwable t) {
						continue;
					}

					if (doc != null && folderDao.isReadEnabled(doc.getFolder().getId(), user.getId())) {
						createAttachment(mail, doc);
						docs.add(doc);
					}
				}
			}

			// Send the message
			EMailSender sender = new EMailSender(user.getTenantId());
			sender.send(mail);

			for (Document doc : docs) {
				try {
					checkPublished(user, doc);
				} catch (Throwable t) {
					continue;
				}

				// Create the document history event
				HistoryDAO dao = (HistoryDAO) Context.get().getBean(HistoryDAO.class);
				History history = new History();
				history.setSessionId(sid);
				history.setDocId(doc.getId());
				history.setEvent(DocumentEvent.SENT.toString());
				history.setUser(user);
				history.setComment(StringUtils.abbreviate(recipients, 4000));
				history.setFilename(doc.getFileName());
				history.setVersion(doc.getVersion());
				history.setPath(folderDao.computePathExtended(doc.getFolder().getId()));
				dao.store(history);
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			throw e;
		}
	}

	private void createAttachment(EMail email, Document doc) throws IOException {
		EMailAttachment att = new EMailAttachment();
		att.setIcon(doc.getIcon());
		Storer storer = (Storer) Context.get().getBean(Storer.class);
		String resource = storer.getResourceName(doc, null, null);
		att.setData(storer.getBytes(doc.getId(), resource));
		att.setFileName(doc.getFileName());
		String extension = doc.getFileExtension();
		att.setMimeType(MimeType.get(extension));

		if (att != null) {
			email.addAttachment(2 + email.getAttachments().size(), att);
		}
	}

	@Override
	public WSDocument createAlias(String sid, long docId, long folderId, String type) throws Exception {
		User user = validateSession(sid);

		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		long rootId = fdao.findRoot(user.getTenantId()).getId();

		if (folderId == rootId) {
			log.error("Cannot create alias in the root");
			throw new Exception("Cannot create alias in the root");
		}

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document originalDoc = docDao.findById(docId);
		checkDownloadEnable(user, originalDoc.getFolder().getId());
		checkWriteEnable(user, folderId);

		FolderDAO mdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Folder folder = mdao.findById(folderId);
		if (folder == null) {
			log.error("Folder " + folder + " not found");
			throw new Exception("error - folder not found");
		}

		// Create the document history event
		History transaction = new History();
		transaction.setSessionId(sid);
		transaction.setEvent(DocumentEvent.SHORTCUT_STORED.toString());
		transaction.setComment("");
		transaction.setUser(user);

		DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);

		Document doc = documentManager.createAlias(originalDoc, folder, type, transaction);

		checkPublished(user, doc);

		return WSUtil.toWSDocument(doc);
	}

	@Override
	public void reindex(String sid, long docId, String content) throws Exception {
		validateSession(sid);
		DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		documentManager.reindex(docId, content);
	}

	@Override
	public WSDocument[] getAliases(String sid, long docId) throws Exception {
		User user = validateSession(sid);
		Collection<Long> folderIds = null;
		if (!user.isMemberOf("admin")) {
			FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			folderIds = fdao.findFolderIdByUserId(user.getId(), null, true);
		}

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		List<Document> docs = new ArrayList<Document>();
		if (user.isMemberOf("admin"))
			docs = docDao.findByWhere("_entity.docRef=" + docId, null, null);
		else {
			String idsStr = folderIds.toString().replace('[', '(').replace(']', ')');
			docs = docDao.findByWhere("_entity.docRef=" + docId + " and _entity.id in " + idsStr, null, null);
		}

		List<WSDocument> wsDocs = new ArrayList<WSDocument>();
		for (int i = 0; i < docs.size(); i++) {
			docDao.initialize(docs.get(i));
			if (user.isMemberOf("admin") || folderIds.contains(docs.get(i).getFolder().getId()))
				wsDocs.add(WSUtil.toWSDocument(docs.get(i)));
		}

		return wsDocs.toArray(new WSDocument[0]);
	}

	@Override
	public long upload(String sid, Long docId, Long folderId, boolean release, String filename, String language,
			DataHandler content) throws Exception {
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
	public WSLink link(String sid, long doc1, long doc2, String type) throws Exception {
		User user = validateSession(sid);

		DocumentLinkDAO linkDao = (DocumentLinkDAO) Context.get().getBean(DocumentLinkDAO.class);
		DocumentLink link = linkDao.findByDocIdsAndType(doc1, doc2, type);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

		Document document1 = docDao.findById(doc1);
		if (document1 == null)
			throw new Exception("Document with ID " + doc1 + " not found");
		Document document2 = docDao.findById(doc2);
		if (document2 == null)
			throw new Exception("Document with ID " + doc2 + " not found");

		checkWriteEnable(user, document2.getFolder().getId());

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
			throw new Exception("Documents already linked");
		}
	}

	@Override
	public WSLink[] getLinks(String sid, long docId) throws Exception {
		User user = validateSession(sid);

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		DocumentLinkDAO linkDao = (DocumentLinkDAO) Context.get().getBean(DocumentLinkDAO.class);

		Document document = docDao.findById(docId);
		if (document == null)
			throw new Exception("Document with ID " + docId + " not found");

		checkReadEnable(user, document.getFolder().getId());
		List<DocumentLink> links = linkDao.findByDocId(docId);
		List<WSLink> lnks = new ArrayList<WSLink>();
		for (DocumentLink link : links) {
			WSLink lnk = new WSLink();
			lnk.setId(link.getId());
			lnk.setDoc1(link.getDocument1().getId());
			lnk.setDoc2(link.getDocument2().getId());
			lnk.setType(link.getType());
			lnks.add(lnk);
		}

		return lnks.toArray(new WSLink[0]);
	}

	@Override
	public void deleteLink(String sid, long id) throws Exception {
		validateSession(sid);
		DocumentLinkDAO linkDao = (DocumentLinkDAO) Context.get().getBean(DocumentLinkDAO.class);
		linkDao.delete(id);
	}

	@Override
	public String getExtractedText(String sid, long docId) throws Exception {
		validateSession(sid);
		SearchEngine indexer = (SearchEngine) Context.get().getBean(SearchEngine.class);
		return indexer.getHit(docId).getContent();
	}

	@Override
	public String createDownloadTicket(String sid, long docId, String suffix, Integer expireHours, String expireDate)
			throws Exception {
		validateSession(sid);

		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		History transaction = new History();
		transaction.setSession(SessionManager.get().get(sid));

		Ticket ticket = manager.createDownloadTicket(docId, suffix, expireHours, convertStringToDate(expireDate), null,
				transaction);

		return ticket.getUrl();
	}

	@Override
	public void setPassword(String sid, long docId, String password) throws Exception {
		User user = validateSession(sid);
		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = dao.findById(docId);

		checkPermission(Permission.PASSWORD, user, doc.getFolder().getId());

		Session session = SessionManager.get().get(sid);

		// Create the document history event
		History transaction = new History();
		transaction.setSession(session);
		transaction.setComment("");

		doc = dao.findDocument(docId);
		dao.setPassword(docId, password, transaction);
	}

	@Override
	public void unsetPassword(String sid, long docId, String currentPassword) throws Exception {
		User user = validateSession(sid);
		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = dao.findById(docId);

		checkPermission(Permission.PASSWORD, user, doc.getFolder().getId());

		Session session = SessionManager.get().get(sid);

		// Create the document history event
		History transaction = new History();
		transaction.setSession(session);
		transaction.setComment("");

		doc = dao.findDocument(docId);
		if (doc.isGranted(currentPassword))
			dao.unsetPassword(docId, transaction);
		else
			throw new Exception("You cannot access the document");
	}

	@Override
	public boolean unprotect(String sid, long docId, String password) throws Exception {
		validateSession();

		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		return manager.unprotect(sid, docId, password);
	}

	@Override
	public WSNote addNote(String sid, long docId, String note) throws Exception {
		User user = validateSession(sid);
		WSDocument document = getDocument(sid, docId);
		if (document == null)
			throw new Exception("Document with ID " + docId + " not found or not accessible");

		DocumentNote newNote = new DocumentNote();
		newNote.setDocId(document.getId());
		newNote.setMessage(note);
		newNote.setUserId(user.getId());
		newNote.setUsername(user.getFullName());

		History transaction = new History();
		transaction.setSessionId(sid);
		transaction.setUser(user);

		DocumentNoteDAO dao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);
		dao.store(newNote, transaction);

		return WSNote.fromDocumentNote(newNote);
	}

	@Override
	public void deleteNote(String sid, long noteId) throws Exception {
		User user = validateSession(sid);
		DocumentNoteDAO dao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);
		DocumentNote note = dao.findById(noteId);
		if (note == null)
			return;

		if (user.isMemberOf("admin") || user.getId() == note.getUserId())
			dao.delete(note.getId());
	}

	@Override
	public String deleteVersion(String sid, long docId, String version) throws Exception {
		validateSession(sid);
		VersionDAO dao = (VersionDAO) Context.get().getBean(VersionDAO.class);
		Version ver = dao.findByVersion(docId, version);

		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);

		// Create the document history event
		History transaction = new History();
		transaction.setSessionId(sid);

		Version latestVersion = manager.deleteVersion(ver.getId(), transaction);
		return latestVersion.getVersion();
	}

	@Override
	public WSNote[] getNotes(String sid, long docId) throws Exception {
		validateSession(sid);
		WSDocument document = getDocument(sid, docId);
		if (document == null)
			throw new Exception("Document with ID " + docId + " not found or not accessible");

		DocumentNoteDAO dao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);
		List<DocumentNote> notes = dao.findByDocId(docId);
		List<WSNote> wsNotes = new ArrayList<WSNote>();
		if (notes != null)
			for (DocumentNote note : notes)
				wsNotes.add(WSNote.fromDocumentNote(note));
		return wsNotes.toArray(new WSNote[0]);
	}

	@Override
	public WSRating rateDocument(String sid, long docId, int vote) throws Exception {
		User user = validateSession(sid);
		WSDocument document = getDocument(sid, docId);
		if (document == null)
			throw new Exception("Document with ID " + docId + " not found or not accessible");

		History transaction = new History();
		transaction.setSessionId(sid);
		transaction.setUser(user);

		RatingDAO ratingDao = (RatingDAO) Context.get().getBean(RatingDAO.class);

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
	public WSRating[] getRatings(String sid, long docId) throws Exception {
		validateSession(sid);
		WSDocument document = getDocument(sid, docId);
		if (document == null)
			throw new Exception("Document with ID " + docId + " not found or not accessible");

		RatingDAO ratingDao = (RatingDAO) Context.get().getBean(RatingDAO.class);
		List<Rating> ratings = ratingDao.findByDocId(docId);
		List<WSRating> wsRatings = new ArrayList<WSRating>();
		if (ratings != null)
			for (Rating rating : ratings)
				wsRatings.add(WSRating.fromRating(rating));

		return wsRatings.toArray(new WSRating[0]);
	}
}