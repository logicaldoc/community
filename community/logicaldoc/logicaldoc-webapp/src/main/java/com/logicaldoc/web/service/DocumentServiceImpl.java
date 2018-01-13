package com.logicaldoc.web.service;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.AccessControlException;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.RowMapper;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;
import com.logicaldoc.core.automation.Automation;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailAttachment;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.communication.MessageTemplate;
import com.logicaldoc.core.communication.MessageTemplateDAO;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.contact.Contact;
import com.logicaldoc.core.contact.ContactDAO;
import com.logicaldoc.core.conversion.FormatConverterManager;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Bookmark;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentLink;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.document.DocumentNote;
import com.logicaldoc.core.document.History;
import com.logicaldoc.core.document.Rating;
import com.logicaldoc.core.document.Version;
import com.logicaldoc.core.document.dao.BookmarkDAO;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.document.dao.DocumentLinkDAO;
import com.logicaldoc.core.document.dao.DocumentNoteDAO;
import com.logicaldoc.core.document.dao.HistoryDAO;
import com.logicaldoc.core.document.dao.RatingDAO;
import com.logicaldoc.core.document.dao.VersionDAO;
import com.logicaldoc.core.document.thumbnail.ThumbnailManager;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.core.ticket.Ticket;
import com.logicaldoc.core.transfer.InMemoryZipImport;
import com.logicaldoc.core.transfer.ZipExport;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.core.util.MailUtil;
import com.logicaldoc.core.util.UserUtil;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIBookmark;
import com.logicaldoc.gui.common.client.beans.GUIContact;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
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
import com.logicaldoc.web.util.ServiceUtil;

/**
 * The document service for the operations on the documents done through the
 * GUI.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentServiceImpl extends RemoteServiceServlet implements DocumentService {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(DocumentServiceImpl.class);

	@Override
	public void addBookmarks(long[] ids, int type) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		BookmarkDAO bookmarkDao = (BookmarkDAO) Context.get().getBean(BookmarkDAO.class);
		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);

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
						bookmark.setTitle(doc.getFileName());
						bookmark.setFileType(doc.getType());
					} else {
						Folder f = fdao.findById(id);
						bookmark.setTitle(f.getName());
					}

					bookmarkDao.store(bookmark);
				}
			} catch (AccessControlException e) {
				ServiceUtil.throwServerException(session, log, e);
			} catch (Exception e) {
				ServiceUtil.throwServerException(session, log, e);
			}
		}
	}

	@Override
	public void indexDocuments(Long[] docIds) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		for (Long id : docIds) {
			if (id != null)
				try {
					documentManager.reindex(id, null);
				} catch (Throwable e) {
					ServiceUtil.throwServerException(session, log, e);
				}
		}
	}

	@Override
	public GUIDocument[] addDocuments(boolean importZip, String charset, boolean immediateIndexing,
			final GUIDocument metadata) throws ServerException {
		final Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		List<GUIDocument> createdDocs = new ArrayList<GUIDocument>();
		List<Document> docs = new ArrayList<Document>();

		Map<String, File> uploadedFilesMap = UploadServlet.getReceivedFiles(getThreadLocalRequest(), session.getSid());
		log.debug("Uploading " + uploadedFilesMap.size() + " files");

		Map<String, String> uploadedFileNames = UploadServlet.getReceivedFileNames(getThreadLocalRequest(),
				session.getSid());

		DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);

		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		final Folder parent = folderDao.findFolder(metadata.getFolder().getId());
		if (uploadedFilesMap.isEmpty())
			ServiceUtil.throwServerException(session, log, new Exception("No file uploaded"));

		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		if (!fdao.isWriteEnabled(metadata.getFolder().getId(), session.getUserId()))
			ServiceUtil.throwServerException(session, log, new Exception(
					"The user doesn't have the write permission on the current folder"));

		List<Long> docsToIndex = new ArrayList<Long>();

		try {
			for (String fileId : uploadedFilesMap.keySet()) {
				final File file = uploadedFilesMap.get(fileId);
				final String filename = uploadedFileNames.get(fileId);

				if (filename.endsWith(".zip") && importZip) {
					log.debug("file = " + file);

					// copy the file into the user folder
					final File destFile = new File(UserUtil.getUserResource(session.getUserId(), "zip"), filename);
					FileUtils.copyFile(file, destFile);

					final long userId = session.getUserId();
					final String sessionId = session.getSid();
					// Prepare the import thread
					Thread zipImporter = new Thread(new Runnable() {
						public void run() {
							/*
							 * Prepare the Master document used to create the
							 * new one
							 */
							Document doc = toDocument(metadata);
							doc.setTenantId(session.getTenantId());
							doc.setCreation(new Date());

							InMemoryZipImport importer = new InMemoryZipImport(doc, charset);
							importer.process(destFile, parent, userId, sessionId);
							try {
								FileUtils.forceDelete(destFile);
							} catch (IOException e) {
								log.error("Unable to delete " + destFile, e);
							}
						}
					});

					// And launch it
					zipImporter.start();
				} else {
					// Create the document history event
					History transaction = new History();
					transaction.setSession(session);
					transaction.setEvent(DocumentEvent.STORED.toString());
					transaction.setComment(metadata.getComment());

					/*
					 * Prepare the Master document used to create the new one
					 */
					Document doc = toDocument(metadata);
					doc.setTenantId(session.getTenantId());
					doc.setCreation(new Date());
					doc.setFileName(filename);

					// Create the new document
					doc = documentManager.create(file, doc, transaction);

					if (immediateIndexing && doc.getIndexed() == Document.INDEX_TO_INDEX)
						docsToIndex.add(doc.getId());

					createdDocs.add(fromDocument(doc, metadata.getFolder(), null));
					docs.add(doc);
				}
			}
			UploadServlet.cleanReceivedFiles(getThreadLocalRequest().getSession());

			if (!docsToIndex.isEmpty())
				indexDocuments(docsToIndex.toArray(new Long[0]));
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}

		/*
		 * We have to notify the specified users in a separate thread
		 */
		if (metadata.getNotifyUsers() != null && metadata.getNotifyUsers().length > 0) {
			Thread notifier = new Thread(new Runnable() {
				public void run() {
					try {
						UserDAO uDao = (UserDAO) Context.get().getBean(UserDAO.class);
						for (long userId : metadata.getNotifyUsers()) {
							User user = uDao.findById(userId);

							EMail mail = new EMail();
							mail.setHtml(1);
							mail.setTenantId(session.getTenantId());

							mail.setAccountId(-1);
							mail.setAuthor(session.getUser().getUsername());
							mail.setAuthorAddress(session.getUser().getEmail());

							mail.setFolder("outbox");
							mail.setSentDate(new Date());
							mail.setUsername(session.getUsername());

							Recipient recipient = new Recipient();
							recipient.setName(user.getUsername());
							recipient.setAddress(user.getEmail());
							recipient.setType(Recipient.TYPE_EMAIL);
							recipient.setMode(Recipient.MODE_EMAIL_TO);
							recipient.setRead(1);
							HashSet<Recipient> recipients = new HashSet<Recipient>();
							recipients.add(recipient);
							mail.setRecipients(recipients);

							MessageTemplateDAO tDao = (MessageTemplateDAO) Context.get().getBean(
									MessageTemplateDAO.class);
							MessageTemplate template = tDao.findByNameAndLanguage("newdoc", user.getLanguage(),
									user.getTenantId());

							Map<String, Object> dictionary = new HashMap<String, Object>();
							dictionary.put("creator", session.getUser());
							dictionary.put("documents", docs);
							dictionary.put("document", docs.get(0));
							dictionary.put("message", metadata.getNotifyMessage());
							dictionary.put(Automation.LOCALE, user.getLocale());

							mail.setSubject(template.getFormattedSubject(dictionary));
							mail.setMessageText("<html><body>" + template.getFormattedBody(dictionary)
									+ "</html></body>");

							EMailSender sender = new EMailSender(session.getTenantName());
							sender.send(mail);
						}
					} catch (Throwable e) {
						log.warn(e.getMessage(), e);
					}
				}
			});
			notifier.start();
		}

		return createdDocs.toArray(new GUIDocument[0]);
	}

	@Override
	public GUIDocument[] addDocuments(String language, long folderId, boolean importZip, String charset,
			boolean immediateIndexing, final Long templateId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		if (folderId == fdao.findRoot(session.getTenantId()).getId())
			throw new RuntimeException("Cannot add documents in the root");

		GUIDocument metadata = new GUIDocument();
		metadata.setLanguage(language);
		metadata.setFolder(new GUIFolder(folderId));
		metadata.setTemplateId(templateId);
		return addDocuments(importZip, charset, immediateIndexing, metadata);
	}

	@Override
	public GUIDocument checkin(GUIDocument document, boolean major) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		Map<String, File> uploadedFilesMap = UploadServlet.getReceivedFiles(getThreadLocalRequest(), session.getSid());
		File file = uploadedFilesMap.values().iterator().next();
		if (file != null) {
			// check that we have a valid file for storing as new version
			Map<String, String> uploadedFileNames = UploadServlet.getReceivedFileNames(getThreadLocalRequest(),
					session.getSid());
			String fileName = uploadedFileNames.values().iterator().next();

			log.debug("Checking in file " + fileName);

			try {
				// Create the document history event
				History transaction = new History();
				transaction.setSession(session);
				transaction.setEvent(DocumentEvent.CHECKEDIN.toString());
				transaction.setComment(document.getComment());

				DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
				Document doc = dao.findById(document.getId());
				if (doc.getDocRef() != null)
					doc = dao.findById(doc.getDocRef().longValue());

				// checkin the document; throws an exception if
				// something goes wrong
				DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
				documentManager.checkin(doc.getId(), new FileInputStream(file), fileName, major, toDocument(document),
						transaction);
				UploadServlet.cleanReceivedFiles(getThreadLocalRequest().getSession());
				return getById(doc.getId());
			} catch (Throwable t) {
				return (GUIDocument) ServiceUtil.throwServerException(session, log, t);
			}
		} else
			return null;
	}

	@Override
	public void checkout(long docId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		// Create the document history event
		History transaction = new History();
		transaction.setSession(session);
		transaction.setEvent(DocumentEvent.CHECKEDOUT.toString());
		transaction.setComment("");

		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = dao.findById(docId);
		if (doc.getDocRef() != null)
			docId = doc.getDocRef().longValue();

		DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		try {
			documentManager.checkout(docId, transaction);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void delete(long[] ids) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		if (ids.length > 0) {
			DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			for (long id : ids) {
				try {
					Document doc = dao.findById(id);
					if (doc == null)
						continue;

					// Create the document history event
					History transaction = new History();
					transaction.setSession(session);
					transaction.setEvent(DocumentEvent.DELETED.toString());
					transaction.setComment("");

					// If it is a shortcut, we delete only the shortcut
					if (doc.getDocRef() != null) {
						transaction.setEvent(DocumentEvent.SHORTCUT_DELETED.toString());
						dao.delete(doc.getId(), transaction);
						continue;
					}

					// The document of the selected documentRecord must be
					// not immutable
					if (doc.getImmutable() == 1 && !transaction.getUser().isMemberOf("admin")) {
						log.debug("Document " + id + " was not deleted because immutable");
						continue;
					}

					// The document must be not locked
					if (doc.getStatus() == Document.DOC_LOCKED) {
						log.debug("Document " + id + " was not deleted because locked");
						continue;
					}

					// Check if there are some shortcuts associated to the
					// deleting document. All the shortcuts must be deleted.
					if (dao.findAliasIds(doc.getId()).size() > 0)
						for (Long shortcutId : dao.findAliasIds(doc.getId())) {
							dao.delete(shortcutId);
						}
					dao.delete(doc.getId(), transaction);
				} catch (Throwable t) {
					ServiceUtil.throwServerException(session, log, t);
				}
			}
		}
	}

	@Override
	public void deleteBookmarks(long[] bookmarkIds) throws ServerException {
		ServiceUtil.validateSession(getThreadLocalRequest());

		BookmarkDAO dao = (BookmarkDAO) Context.get().getBean(BookmarkDAO.class);
		for (long id : bookmarkIds) {
			dao.delete(id);
		}

	}

	@Override
	public void deleteLinks(long[] ids) throws ServerException {
		ServiceUtil.validateSession(getThreadLocalRequest());

		DocumentLinkDAO dao = (DocumentLinkDAO) Context.get().getBean(DocumentLinkDAO.class);
		for (long id : ids) {
			dao.delete(id);
		}
	}

	@Override
	public GUIAttribute[] getAttributes(long templateId) throws ServerException {
		ServiceUtil.validateSession(getThreadLocalRequest());

		TemplateDAO templateDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		Template template = templateDao.findById(templateId);
		templateDao.initialize(template);

		GUIAttribute[] attributes = prepareGUIAttributes(template, null);

		return attributes;
	}

	private static GUIAttribute[] prepareGUIAttributes(Template template, Document doc) {
		List<GUIAttribute> attributes = new ArrayList<GUIAttribute>();
		if (template == null || template.getAttributes() == null || template.getAttributes().isEmpty())
			return new GUIAttribute[0];
		try {
			if (template != null) {
				for (String attrName : template.getAttributeNames()) {
					Attribute extAttr = template.getAttributes().get(attrName);
					GUIAttribute att = new GUIAttribute();
					att.setName(attrName);
					att.setSetId(extAttr.getSetId());
					att.setPosition(extAttr.getPosition());
					att.setLabel(extAttr.getLabel());
					att.setMandatory(extAttr.getMandatory() == 1);
					att.setEditor(extAttr.getEditor());
					att.setStringValue(extAttr.getStringValue());

					att.setOptions(new String[] { extAttr.getStringValue() });

					if (doc != null)
						if (doc.getValue(attrName) != null) {
							att.setValue(doc.getValue(attrName));
						} else
							att.setValue(extAttr.getValue());

					// Normalize dates
					if (att.getValue() instanceof Date)
						att.setValue(ServiceUtil.convertToDate((Date) att.getValue()));

					att.setType(extAttr.getType());

					attributes.add(att);
				}
			}

			return attributes.toArray(new GUIAttribute[0]);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			return null;
		}
	}

	@Override
	public GUIDocument getById(long docId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		try {
			return getDocument(session, docId);
		} catch (Throwable t) {
			return (GUIDocument) ServiceUtil.throwServerException(session, log, t);
		}
	}

	public static GUIDocument getDocument(Session session, long docId) throws Exception {
		if (session != null)
			ServiceUtil.validateSession(session.getSid());

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = docDao.findById(docId);

		GUIDocument document = null;
		GUIFolder folder = null;

		if (doc != null) {
			folder = FolderServiceImpl.getFolder(session, doc.getFolder().getId());
			if (folder == null)
				throw new ServerException("You cannot access the document");

			Long aliasId = null;
			String aliasFileName = null;
			String aliasType = null;

			// Check if it is an alias
			if (doc.getDocRef() != null) {
				aliasFileName = doc.getFileName();
				aliasType = doc.getType();
				long id = doc.getDocRef();
				doc = docDao.findById(id);
				aliasId = docId;
			}

			if (session != null)
				checkPublished(session != null ? session.getUser() : null, doc);

			docDao.initialize(doc);

			document = fromDocument(doc, folder, session != null ? session.getUser() : null);

			if (aliasId != null)
				document.setDocRef(aliasId);
			if (StringUtils.isNotEmpty(aliasFileName))
				document.setFileName(aliasFileName);
			if (StringUtils.isNotEmpty(aliasType)) {
				document.setType(aliasType);
				document.setIcon(aliasType + "-sc");
			}

			if (session != null) {
				FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
				Set<Permission> permissions = fdao.getEnabledPermissions(doc.getFolder().getId(), session.getUserId());
				List<String> permissionsList = new ArrayList<String>();
				for (Permission permission : permissions)
					permissionsList.add(permission.toString());
				folder.setPermissions(permissionsList.toArray(new String[permissionsList.size()]));
			}
		}
		return document;
	}

	public static GUIDocument fromDocument(Document doc, GUIFolder folder, User sessionUser) {
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		if (doc.getId() != 0L)
			docDao.initialize(doc);

		GUIDocument document = new GUIDocument();
		document.setId(doc.getId());
		if (doc.getDocRef() != null && doc.getDocRef().longValue() != 0) {
			document.setDocRef(doc.getDocRef());
			document.setDocRefType(doc.getDocRefType());
		}
		document.setCustomId(doc.getCustomId());
		if (doc.getTags().size() > 0)
			document.setTags(doc.getTagsAsWords().toArray(new String[doc.getTags().size()]));
		else
			document.setTags(new String[0]);
		document.setType(doc.getType());
		document.setFileName(doc.getFileName());
		document.setVersion(doc.getVersion());
		document.setCreation(doc.getCreation());
		document.setCreator(doc.getCreator());
		document.setDate(doc.getDate());
		document.setPublisher(doc.getPublisher());
		document.setFileVersion(doc.getFileVersion());
		document.setLanguage(doc.getLanguage());
		document.setTemplateId(doc.getTemplateId());
		document.setLastModified(doc.getLastModified());
		document.setLockUserId(doc.getLockUserId());
		document.setLockUser(doc.getLockUser());
		document.setComment(doc.getComment());
		document.setStatus(doc.getStatus());
		document.setWorkflowStatus(doc.getWorkflowStatus());
		document.setImmutable(doc.getImmutable());
		document.setFileSize(new Long(doc.getFileSize()).floatValue());
		document.setStartPublishing(doc.getStartPublishing());
		document.setStopPublishing(doc.getStopPublishing());
		document.setPublished(doc.getPublished());
		document.setSigned(doc.getSigned());
		document.setStamped(doc.getStamped());
		document.setBarcoded(doc.getBarcoded());
		document.setIndexed(doc.getIndexed());
		document.setExtResId(doc.getExtResId());
		document.setPages(doc.getPages());
		document.setNature(doc.getNature());
		document.setFormId(doc.getFormId());
		document.setIcon(FilenameUtils.getBaseName(doc.getIcon()));
		document.setPasswordProtected(doc.isPasswordProtected());

		if (doc.getRating() != null)
			document.setRating(doc.getRating());

		if (doc.getCustomId() != null)
			document.setCustomId(doc.getCustomId());
		else
			document.setCustomId("");

		if (doc.getTemplate() != null) {
			document.setTemplate(doc.getTemplate().getName());
			document.setTemplateId(doc.getTemplate().getId());
		}

		if (sessionUser != null) {
			BookmarkDAO bDao = (BookmarkDAO) Context.get().getBean(BookmarkDAO.class);
			document.setBookmarked(bDao.isDocBookmarkedByUser(document.getId(), sessionUser.getId()));
			if (document.getDocRef() != null)
				document.setBookmarked(bDao.isDocBookmarkedByUser(document.getDocRef(), sessionUser.getId()));
		}

		GUIAttribute[] attributes = prepareGUIAttributes(doc.getTemplate(), doc);
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

	@Override
	public GUIVersion[] getVersionsById(long id1, long id2) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			VersionDAO versDao = (VersionDAO) Context.get().getBean(VersionDAO.class);
			Version docVersion = versDao.findById(id1);
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
				version1.setVersion(docVersion.getVersion());
				version1.setCreation(docVersion.getCreation());
				version1.setCreator(docVersion.getCreator());
				version1.setDate(docVersion.getDate());
				version1.setPublisher(docVersion.getPublisher());
				version1.setFileVersion(docVersion.getFileVersion());
				version1.setLanguage(docVersion.getLanguage());
				version1.setTemplateId(docVersion.getTemplateId());
				version1.setFileSize(new Float(docVersion.getFileSize()));
				version1.setWorkflowStatus(docVersion.getWorkflowStatus());
				if (docVersion.getRating() != null)
					version1.setRating(docVersion.getRating());
				version1.setStartPublishing(docVersion.getStartPublishing());
				version1.setStopPublishing(docVersion.getStopPublishing());
				version1.setPublished(docVersion.getPublished());
				version1.setPages(docVersion.getPages());

				version1.setTemplate(docVersion.getTemplateName());
				versDao.initialize(docVersion);
				for (String attrName : docVersion.getAttributeNames()) {
					Attribute extAttr = docVersion.getAttributes().get(attrName);
					version1.setValue(attrName, extAttr.getValue());
				}
				GUIFolder folder1 = new GUIFolder();
				folder1.setName(docVersion.getFolderName());
				folder1.setId(docVersion.getFolderId());
				version1.setFolder(folder1);
			}

			docVersion = versDao.findById(id2);
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
				version2.setVersion(docVersion.getVersion());
				version2.setCreation(docVersion.getCreation());
				version2.setCreator(docVersion.getCreator());
				version2.setDate(docVersion.getDate());
				version2.setPublisher(docVersion.getPublisher());
				version2.setFileVersion(docVersion.getFileVersion());
				version2.setLanguage(docVersion.getLanguage());
				version2.setFileSize(new Float(docVersion.getFileSize()));
				if (docVersion.getRating() != null)
					version2.setRating(docVersion.getRating());
				version2.setWorkflowStatus(docVersion.getWorkflowStatus());
				version2.setStartPublishing(docVersion.getStartPublishing());
				version2.setStopPublishing(docVersion.getStopPublishing());
				version2.setPublished(docVersion.getPublished());
				version2.setPages(docVersion.getPages());

				version2.setTemplateId(docVersion.getTemplateId());
				version2.setTemplate(docVersion.getTemplateName());
				versDao.initialize(docVersion);
				for (String attrName : docVersion.getAttributeNames()) {
					Attribute extAttr = docVersion.getAttributes().get(attrName);
					version2.setValue(attrName, extAttr.getValue());
				}
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
			} else if (version1 != null && version2 == null) {
				versions = new GUIVersion[1];
				versions[0] = version1;
			} else if (version1 == null && version2 != null) {
				versions = new GUIVersion[1];
				versions[0] = version2;
			} else
				return null;

			return versions;
		} catch (Throwable t) {
			log.error("Exception linking documents: " + t.getMessage(), t);
			return (GUIVersion[]) ServiceUtil.throwServerException(session, null, t);
		}
	}

	@Override
	public void linkDocuments(long[] inDocIds, long[] outDocIds) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		DocumentLinkDAO linkDao = (DocumentLinkDAO) Context.get().getBean(DocumentLinkDAO.class);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		if (outDocIds.length > 0) {
			try {
				for (int i = 0; i < inDocIds.length; i++) {
					for (int j = 0; j < outDocIds.length; j++) {
						DocumentLink link = linkDao.findByDocIdsAndType(inDocIds[i], outDocIds[j], "default");
						if (link == null) {
							// The link doesn't exist and must be created
							link = new DocumentLink();
							link.setTenantId(session.getTenantId());
							link.setDocument1(docDao.findById(inDocIds[i]));
							link.setDocument2(docDao.findById(outDocIds[j]));
							link.setType("default");
							linkDao.store(link);
						}
					}
				}
			} catch (Throwable t) {
				ServiceUtil.throwServerException(session, log, t);
			}
		}
	}

	@Override
	public void lock(long[] docIds, String comment) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		// Unlock the document; throws an exception if something
		// goes wrong
		DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		try {
			// Create the document history event
			History transaction = new History();
			transaction.setSession(session);
			transaction.setEvent(DocumentEvent.LOCKED.toString());
			transaction.setComment(comment);
			for (long id : docIds) {
				documentManager.lock(id, Document.DOC_LOCKED, transaction);
			}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void makeImmutable(long[] docIds, String comment) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			for (long id : docIds) {
				Document doc = docDao.findById(id);

				if (doc.getImmutable() == 0) {
					// The document of the selected documentRecord must be
					// not locked
					if (doc.getStatus() != Document.DOC_UNLOCKED) {
						continue;
					}

					// Create the document history event
					History transaction = new History();
					transaction.setSession(session);
					transaction.setComment(comment);

					manager.makeImmutable(id, transaction);
				}
			}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void markHistoryAsRead(String event) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		HistoryDAO dao = (HistoryDAO) Context.get().getBean(HistoryDAO.class);
		for (History history : dao.findByUserIdAndEvent(session.getUserId(), event, null)) {
			dao.initialize(history);
			history.setIsNew(0);
			dao.store(history);
		}
	}

	@Override
	public void markIndexable(long[] docIds) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			for (long id : docIds)
				manager.changeIndexingStatus(docDao.findById(id), AbstractDocument.INDEX_TO_INDEX);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void markUnindexable(long[] docIds) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			for (long id : docIds)
				manager.changeIndexingStatus(docDao.findById(id), AbstractDocument.INDEX_SKIP);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void restore(Long[] docIds, long folderId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

		for (Long docId : docIds) {
			if (docId == null)
				continue;
			History transaction = new History();
			transaction.setSession(session);
			docDao.restore(docId, folderId, transaction);
		}
	}

	@Override
	public GUIDocument save(GUIDocument document) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			if (document.getId() != 0) {
				if (document.getDocRef() != null) {
					Document alias = docDao.findById(document.getDocRef());
					docDao.initialize(alias);
					alias.setFileName(document.getFileName());
					alias.setType(FilenameUtils.getExtension(alias.getFileName()).toLowerCase());
					docDao.store(alias);
				}

				Document doc = docDao.findById(document.getId());
				docDao.initialize(doc);
				String originalDocFilename = doc.getFileName();
				doc.setCustomId(document.getCustomId());
				doc.setComment(document.getComment());

				try {
					Document docVO = toDocument(document);
					docVO.setBarcoded(doc.getBarcoded());
					docVO.setTenantId(session.getTenantId());
					if (document.getDocRef() != null) {
						docVO.setFileName(originalDocFilename);
						docVO.setType(FilenameUtils.getExtension(originalDocFilename).toLowerCase());
					}

					// Create the document history event
					History transaction = new History();
					transaction.setSession(session);
					transaction.setEvent(DocumentEvent.CHANGED.toString());
					transaction.setComment(document.getComment());

					DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
					documentManager.update(doc, docVO, transaction);

					if (document.getDocRef() != null)
						return getById(document.getDocRef());
					else
						return getById(document.getId());
				} catch (Throwable t) {
					log.error(t.getMessage(), t);
					throw new RuntimeException(t.getMessage(), t);
				}
			} else
				return null;
		} catch (Throwable t) {
			return (GUIDocument) ServiceUtil.throwServerException(session, log, t);
		}
	}

	/**
	 * Produces a plain new Document from a GUIDocument
	 */
	protected Document toDocument(GUIDocument document) {
		Document docVO = new Document();
		if (document.getTags() != null && document.getTags().length > 0)
			docVO.setTagsFromWords(new HashSet<String>(Arrays.asList(document.getTags())));

		docVO.setCustomId(document.getCustomId());
		docVO.setFileName(document.getFileName());
		docVO.setVersion(document.getVersion());
		docVO.setCreation(document.getCreation());
		docVO.setCreator(document.getCreator());
		docVO.setDate(document.getDate());
		docVO.setPublisher(document.getPublisher());
		docVO.setFileVersion(document.getFileVersion());
		docVO.setLanguage(document.getLanguage());
		if (document.getFileSize() != null)
			docVO.setFileSize(document.getFileSize().longValue());

		docVO.setRating(document.getRating());
		docVO.setComment(document.getComment());
		docVO.setWorkflowStatus(document.getWorkflowStatus());
		docVO.setStartPublishing(document.getStartPublishing());
		docVO.setStopPublishing(document.getStopPublishing());
		docVO.setPublished(document.getPublished());
		docVO.setBarcoded(document.getBarcoded());
		docVO.setExtResId(document.getExtResId());
		docVO.setPages(document.getPages());
		docVO.setNature(document.getNature());
		docVO.setFormId(document.getFormId());

		if (document.getTemplateId() != null) {
			docVO.setTemplateId(document.getTemplateId());
			TemplateDAO templateDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
			Template template = templateDao.findById(document.getTemplateId());
			docVO.setTemplate(template);
			if (document.getAttributes().length > 0) {
				for (GUIAttribute attr : document.getAttributes()) {
					Attribute templateAttribute = template.getAttributes().get(attr.getName());
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
						} else if (templateType == GUIAttribute.TYPE_USER) {
							extAttr.setIntValue(attr.getIntValue());
							extAttr.setStringValue(attr.getStringValue());
							extAttr.setType(templateType);
						}
					} else {
						if (templateType == Attribute.TYPE_INT) {
							if (attr.getValue() != null)
								extAttr.setIntValue((Long) attr.getValue());
							else
								extAttr.setIntValue(null);
						} else if (templateType == Attribute.TYPE_BOOLEAN) {
							if (attr.getBooleanValue() != null)
								extAttr.setValue(attr.getBooleanValue());
							else
								extAttr.setBooleanValue(null);
						} else if (templateType == Attribute.TYPE_DOUBLE) {
							if (attr.getValue() != null)
								extAttr.setDoubleValue((Double) attr.getValue());
							else
								extAttr.setDoubleValue(null);
						} else if (templateType == Attribute.TYPE_DATE) {
							if (attr.getValue() != null)
								extAttr.setDateValue((Date) attr.getValue());
							else
								extAttr.setDateValue(null);
						} else if (templateType == Attribute.TYPE_STRING) {
							if (attr.getValue() != null)
								extAttr.setStringValue((String) attr.getValue());
							else
								extAttr.setStringValue(null);
						} else if (templateType == Attribute.TYPE_USER) {
							if (attr.getValue() != null) {
								extAttr.setIntValue(attr.getIntValue());
								extAttr.setStringValue(attr.getStringValue());
							} else {
								extAttr.setIntValue(null);
								extAttr.setStringValue(null);
							}
							extAttr.setType(templateType);
						}
					}

					extAttr.setLabel(attr.getLabel());
					extAttr.setType(templateType);
					extAttr.setPosition(attr.getPosition());
					extAttr.setMandatory(attr.isMandatory() ? 1 : 0);
					extAttr.setSetId(templateAttribute.getSetId());

					docVO.getAttributes().put(attr.getName(), extAttr);
				}
			}
		}

		docVO.setStatus(document.getStatus());
		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		if (document.getFolder() != null)
			docVO.setFolder(fdao.findById(document.getFolder().getId()));
		return docVO;
	}

	@Override
	public String sendAsEmail(GUIEmail email, String locale) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		DocumentDAO documentDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		ContextProperties config = (ContextProperties) Context.get().getProperties();

		// Needed in case the zip compression was requested by the user
		File zipFile = null;

		// Needed in case of thumbnail generation
		File thumbnailFile = null;

		EMail mail;
		try {
			mail = new EMail();
			mail.setHtml(1);
			mail.setTenantId(session.getTenantId());

			mail.setAccountId(-1);
			mail.setAuthor(session.getUser().getUsername());

			if (config.getBoolean(session.getTenantName() + ".smtp.userasfrom", true)) {
				if (email.getFrom() != null)
					mail.setAuthorAddress(email.getFrom().getEmail());
				else
					mail.setAuthorAddress(session.getUser().getEmail());
			}

			if (email.getTos() != null && email.getTos().length > 0) {
				StringBuffer sb = new StringBuffer();
				for (GUIContact contact : email.getTos()) {
					if (sb.length() > 0)
						sb.append(",");
					sb.append(contact.getEmail());
				}
				mail.parseRecipients(sb.toString());
			}

			if (email.getCcs() != null && email.getCcs().length > 0) {
				StringBuffer sb = new StringBuffer();
				for (GUIContact contact : email.getCcs()) {
					if (sb.length() > 0)
						sb.append(",");
					sb.append(contact.getEmail());
				}
				mail.parseRecipientsCC(sb.toString());
			}

			mail.setFolder("outbox");
			mail.setSentDate(new Date());
			mail.setUsername(session.getUsername());

			List<Document> attachedDocs = documentDao.findByIds(ArrayUtils.toObject(email.getDocIds()), null);
			for (Document document : attachedDocs)
				documentDao.initialize(document);

			/*
			 * Subject and email are processed by the scripting engine
			 */
			Automation engine = new Automation("sendmail", LocaleUtil.toLocale(locale), session.getTenantId());
			Map<String, Object> dictionary = new HashMap<String, Object>();
			dictionary.put(Automation.LOCALE, LocaleUtil.toLocale(locale));
			dictionary.put("sender", session.getUser());
			dictionary.put("documents", attachedDocs);
			dictionary.put("document", attachedDocs.get(0));
			String message = engine.evaluate(email.getMessage(), dictionary);

			mail.setSubject(engine.evaluate(email.getSubject(), dictionary));

			if (email.isSendAsTicket()) {
				// Prepare a new download ticket
				History transaction = new History();
				transaction.setSession(session);
				Ticket ticket = manager.createDownloadTicket(email.getDocIds()[0], null, null, null, null, transaction);

				Document doc = documentDao.findById(email.getDocIds()[0]);

				message = message
						+ "<div style='margin-top:10px; border-top:1px solid black; background-color:#CCCCCC;'><b>&nbsp;"
						+ I18N.message("clicktodownload", LocaleUtil.toLocale(locale)) + ": <a href='"
						+ ticket.getUrl() + "'>" + doc.getFileName() + "</a></b></div>";

				if (doc.getDocRef() != null)
					doc = documentDao.findById(doc.getDocRef());

				try {
					thumbnailFile = createThumbnail(doc, session.getSid());
					if (thumbnailFile != null) {
						String thumb = thumbnailFile.toURI().toURL().toString();
						mail.getImages().add(thumb);
						message += "<p><img src='cid:image_1'/></p>";
					}
				} catch (IOException ioe) {
					log.warn(ioe.getMessage());
				}

				mail.setMessageText("<html><head><meta charset='utf-8' /></head><body>" + message + "</body></html>");
			} else {
				if (email.isZipCompression()) {
					/*
					 * Create a temporary archive for sending it as unique
					 * attachment
					 */
					zipFile = File.createTempFile("email", "zip");
					OutputStream out = null;

					try {
						out = new FileOutputStream(zipFile);

						// Create the document history event
						History transaction = new History();
						transaction.setSession(session);
						transaction.setEvent(DocumentEvent.DOWNLOADED.toString());

						ZipExport export = new ZipExport();
						export.process(email.getDocIds(), out, email.isPdfConversion(), transaction);
						createAttachment(mail, zipFile);
					} catch (Throwable t) {
						log.error(t.getMessage(), t);
						try {
							if (out != null)
								out.close();
						} catch (Throwable q) {

						}
					}
				} else {
					for (long id : email.getDocIds())
						createAttachment(mail, id, email.isPdfConversion(), session.getSid());
				}
			}

			try {
				message = "<html><head><meta charset='utf-8' /></head><body>" + message + "</body></html>";
				mail.setMessageText(message);

				// Send the message
				EMailSender sender = new EMailSender(session.getTenantName());
				sender.send(mail);

				FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
				for (Document d : attachedDocs) {
					Document doc = d;
					if (doc.getDocRef() != null)
						doc = documentDao.findById(doc.getDocRef());

					// Create the document history event
					HistoryDAO dao = (HistoryDAO) Context.get().getBean(HistoryDAO.class);
					History history = new History();
					history.setSession(session);
					history.setDocId(doc.getId());
					history.setEvent(DocumentEvent.SENT.toString());
					history.setComment(StringUtils.abbreviate(
							StringUtil.collectionToString(mail.getRecipients(), ", "), 4000));
					history.setFilename(doc.getFileName());
					history.setVersion(doc.getVersion());
					history.setPath(fDao.computePathExtended(doc.getFolder().getId()));
					dao.store(history);
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
			} catch (Throwable ex) {
				log.warn(ex.getMessage(), ex);
				return "error";
			}
		} catch (Throwable e) {
			log.warn(e.getMessage(), e);
			return "error";
		} finally {
			if (zipFile != null)
				try {
					FileUtils.forceDelete(zipFile);
				} catch (IOException e) {
				}
			if (thumbnailFile != null)
				try {
					FileUtils.forceDelete(zipFile);
				} catch (IOException e) {
				}
		}
	}

	private File createThumbnail(Document doc, String sid) throws IOException {
		Storer storer = (Storer) Context.get().getBean(Storer.class);
		String thumbResource = storer.getResourceName(doc, doc.getFileVersion(), "thumb.jpg");

		// In any case try to produce the thumbnail
		if (storer.size(doc.getId(), thumbResource) <= 0L) {
			ThumbnailManager thumbManager = (ThumbnailManager) Context.get().getBean(ThumbnailManager.class);
			try {
				thumbManager.createTumbnail(doc, doc.getFileVersion(), sid);
			} catch (Throwable t) {
				log.error(t.getMessage(), t);
			}
		}

		if (storer.exists(doc.getId(), thumbResource)) {
			File file = File.createTempFile("thumb-", ".jpg");
			storer.writeToFile(doc.getId(), thumbResource, file);
			return file;
		}

		return null;
	}

	private void createAttachment(EMail email, long docId, boolean pdfConversion, String sid) throws IOException {
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Storer storer = (Storer) Context.get().getBean(Storer.class);
		Document doc = docDao.findById(docId);
		String resource = storer.getResourceName(doc, null, null);

		boolean convertToPdf = pdfConversion;
		if (doc.getDocRef() != null) {
			// this is an alias
			if ("pdf".equals(doc.getDocRefType())) {
				doc = docDao.findById(doc.getDocRef());
				convertToPdf = true;
			}
		}

		EMailAttachment att = new EMailAttachment();
		att.setIcon(doc.getIcon());
		att.setFileName(doc.getFileName());
		String extension = doc.getFileExtension();
		att.setMimeType(MimeType.get(extension));

		if (convertToPdf) {
			if (!"pdf".equals(FilenameUtils.getExtension(doc.getFileName().toLowerCase()))) {
				FormatConverterManager manager = (FormatConverterManager) Context.get().getBean(
						FormatConverterManager.class);
				manager.convertToPdf(doc, sid);
				resource = storer.getResourceName(doc, null, "conversion.pdf");
			}
			att.setMimeType(MimeType.get("pdf"));
			att.setFileName(FilenameUtils.getBaseName(doc.getFileName()) + ".pdf");
		}

		att.setData(storer.getBytes(doc.getId(), resource));

		if (att != null) {
			email.addAttachment(2 + email.getAttachments().size(), att);
		}
	}

	private void createAttachment(EMail email, File zipFile) throws IOException {
		EMailAttachment att = new EMailAttachment();
		att.setData(FileUtil.toByteArray(zipFile));
		att.setFileName("doc.zip");
		String extension = "zip";
		att.setMimeType(MimeType.get(extension));

		if (att != null) {
			email.addAttachment(2 + email.getAttachments().size(), att);
		}
	}

	@Override
	public void unlock(long[] docIds) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			// Create the document history event
			History transaction = new History();
			transaction.setSession(session);

			// Unlock the document; throws an exception if something
			// goes wrong
			DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			for (long id : docIds) {
				documentManager.unlock(id, transaction);
			}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void updateBookmark(GUIBookmark bookmark) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		BookmarkDAO bookmarkDao = (BookmarkDAO) Context.get().getBean(BookmarkDAO.class);
		try {
			Bookmark bk;
			if (bookmark.getId() != 0) {
				bk = bookmarkDao.findById(bookmark.getId());
				bookmarkDao.initialize(bk);
			} else
				return;

			bk.setTitle(bookmark.getName());
			bk.setDescription(bookmark.getDescription());

			bookmarkDao.store(bk);
			bookmark.setId(bk.getId());
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void updateLink(long id, String type) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentLinkDAO dao = (DocumentLinkDAO) Context.get().getBean(DocumentLinkDAO.class);
			DocumentLink link = dao.findById(id);
			dao.initialize(link);
			link.setType(type);
			dao.store(link);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void cleanUploadedFileFolder() throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		UploadServlet.cleanReceivedFiles(session.getSid());
	}

	@Override
	public GUIRating getRating(long docId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		RatingDAO ratingDao = (RatingDAO) Context.get().getBean(RatingDAO.class);

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
		} catch (Throwable t) {
			return (GUIRating) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public int saveRating(GUIRating rating) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
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

			History transaction = new History();
			transaction.setSession(session);
			ratingDao.store(rat, transaction);

			return updateDocumentRating(rating.getDocId());
		} catch (Throwable t) {
			return (Integer) ServiceUtil.throwServerException(session, log, t);
		}
	}

	private int updateDocumentRating(long docId) {
		RatingDAO ratingDao = (RatingDAO) Context.get().getBean(RatingDAO.class);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

		Rating votesDoc = ratingDao.findVotesByDocId(docId);
		Document doc = docDao.findById(docId);
		docDao.initialize(doc);
		int average = 0;
		if (votesDoc != null && votesDoc.getAverage() != null)
			average = votesDoc.getAverage().intValue();
		doc.setRating(average);
		docDao.store(doc);
		return average;
	}

	@Override
	public long addNote(long docId, String message) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentNote note = new DocumentNote();
			note.setTenantId(session.getTenantId());
			note.setDocId(docId);
			note.setUserId(session.getUserId());
			note.setUsername(session.getUser().getFullName());
			note.setDate(new Date());
			note.setMessage(message);

			History transaction = new History();
			transaction.setSession(session);

			DocumentNoteDAO dao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);
			dao.store(note, transaction);

			return note.getId();
		} catch (Throwable t) {
			return (Integer) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void deleteNotes(long[] ids) throws ServerException {
		ServiceUtil.validateSession(getThreadLocalRequest());

		DocumentNoteDAO dao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);
		for (long id : ids)
			dao.delete(id);
	}

	@Override
	public void bulkUpdate(long[] ids, GUIDocument vo, boolean ignoreEmptyFields) throws ServerException {
		ServiceUtil.validateSession(getThreadLocalRequest());

		for (long id : ids) {
			try {
				GUIDocument buf = getById(id);
				if (buf.getImmutable() == 1 || buf.getStatus() != Document.DOC_UNLOCKED)
					continue;

				buf.setComment(vo.getComment() != null ? vo.getComment() : "");

				if (vo.getPublished() > -1)
					buf.setPublished(vo.getPublished());
				if (vo.getStartPublishing() != null)
					buf.setStartPublishing(vo.getStartPublishing());
				if (vo.getStopPublishing() != null)
					buf.setStopPublishing(vo.getStopPublishing());
				if (StringUtils.isNotEmpty(vo.getLanguage()))
					buf.setLanguage(vo.getLanguage());
				if (vo.getTags() != null && vo.getTags().length > 0)
					buf.setTags(vo.getTags());
				else if (!ignoreEmptyFields)
					buf.setTags(null);
				if (vo.getTemplateId() != null)
					buf.setTemplateId(vo.getTemplateId());

				if (vo.getAttributes() != null && vo.getAttributes().length > 0) {
					if (ignoreEmptyFields) {
						List<GUIAttribute> attributes = new ArrayList<GUIAttribute>();
						for (GUIAttribute att : vo.getAttributes()) {
							if (att.getValue() != null && StringUtils.isNotEmpty(att.getValue().toString()))
								attributes.add(att);
							else
								attributes.add(buf.getAttribute(att.getName()));
						}
						buf.setAttributes(attributes.toArray(new GUIAttribute[0]));
					} else {
						buf.setAttributes(vo.getAttributes());
					}
				}

				save(buf);
			} catch (Throwable e) {
				log.error(e.getMessage(), e);
			}
		}
	}

	protected static void checkPublished(User user, Document doc) throws Exception {
		if (!user.isMemberOf("admin") && !user.isMemberOf("publisher") && !doc.isPublishing())
			throw new FileNotFoundException("Document not published");
	}

	@Override
	public void updateNote(long docId, long noteId, String message) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentNoteDAO dao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);
			DocumentNote note = dao.findById(noteId);
			note.setUserId(session.getUser().getId());
			note.setUsername(session.getUser().getFullName());
			note.setMessage(message);
			dao.store(note);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public GUIDocument deleteVersions(long[] ids) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			for (long id : ids) {
				History transaction = new History();
				transaction.setSession(session);
				Version version = manager.deleteVersion(id, transaction);
				return getById(version.getDocId());
			}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
		return null;
	}

	@Override
	public GUIDocument createEmpty(GUIDocument vo) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);

			if (!fdao.isWriteEnabled(vo.getFolder().getId(), session.getUserId())) {
				throw new RuntimeException("The user doesn't have the write permission on the current folder");
			}

			Document doc = toDocument(vo);
			doc.setId(0L);

			History transaction = new History();
			transaction.setSession(session);
			transaction.setEvent(DocumentEvent.STORED.toString());
			Document document = documentManager.create(IOUtils.toInputStream("", "UTF-8"), doc, transaction);

			// If that VO is in checkout, perform a checkout also
			if (vo.getStatus() == Document.DOC_CHECKED_OUT) {
				transaction = new History();
				transaction.setSession(session);
				documentManager.checkout(document.getId(), transaction);
			}

			return fromDocument(document, vo.getFolder(), null);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
		return null;
	}

	@Override
	public void deleteFromTrash(Long[] ids) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		if (ids == null || ids.length < 1)
			return;

		try {
			String idsStr = Arrays.asList(ids).toString().replace('[', '(').replace(']', ')');
			DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			dao.bulkUpdate("set ld_deleted=2 where ld_id in " + idsStr, null);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void emptyTrash() throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			dao.bulkUpdate("set ld_deleted=2 where ld_deleted=1 and  ld_deleteuserid=" + session.getUserId(), null);

			FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			fdao.bulkUpdate("set ld_deleted=2 where ld_deleted=1 and  ld_deleteuserid=" + session.getUserId(), null);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void archiveDocuments(long[] docIds, String comment) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			History transaction = new History();
			transaction.setSession(session);
			manager.archiveDocuments(docIds, transaction);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public long archiveFolder(long folderId, String comment) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			History transaction = new History();
			transaction.setSession(session);
			transaction.setComment(comment);
			return manager.archiveFolder(folderId, transaction);
		} catch (Throwable t) {
			return (Long) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void unarchiveDocuments(long[] docIds) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

			for (long id : docIds) {
				// Create the document history event
				History transaction = new History();
				transaction.setSession(session);

				dao.unarchive(id, transaction);
			}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public long countDocuments(long[] folderIds, int status) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		long count = 0;
		try {
			for (int i = 0; i < folderIds.length; i++) {
				count += countDocuments(session.getUser(), folderIds[i], status);
			}
		} catch (Throwable t) {
			return (Long) ServiceUtil.throwServerException(session, log, t);
		}
		return count;
	}

	private long countDocuments(User user, long folderId, int status) throws ServerException {
		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);

		List<Long> childrenFolderIds = fdao.findIdsByParentId(folderId);
		childrenFolderIds.add(folderId);

		StringBuffer query = new StringBuffer("select count(ld_id) from ld_document where ld_deleted=0 and ld_status="
				+ status);
		query.append(" and ld_folderid in " + childrenFolderIds.toString().replace("[", "(").replace("]", ")"));

		return dao.queryForLong(query.toString());
	}

	@Override
	public String createDownloadTicket(long docId, String suffix, Integer expireHours, Date expireDate)
			throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			HttpServletRequest request = this.getThreadLocalRequest();
			String urlPrefix = request.getScheme() + "://" + request.getServerName() + ":" + request.getServerPort()
					+ request.getContextPath();

			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			History transaction = new History();
			transaction.setSession(session);
			Ticket ticket = manager
					.createDownloadTicket(docId, suffix, expireHours, expireDate, urlPrefix, transaction);

			return ticket.getUrl();
		} catch (Throwable t) {
			return (String) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void setPassword(long docId, String password) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

		// Create the document history event
		History transaction = new History();
		transaction.setSession(session);
		transaction.setComment("");

		try {
			Document doc = dao.findById(docId);
			ServiceUtil.checkPermission(Permission.PASSWORD, session.getUser(), doc.getFolder().getId());

			dao.setPassword(docId, password, transaction);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void unsetPassword(long docId, String currentPassword) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		// Create the document history event
		History transaction = new History();
		transaction.setSession(session);
		transaction.setComment("");

		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		try {
			Document doc = dao.findDocument(docId);
			if (session.getUser().isMemberOf("admin") || doc.isGranted(currentPassword))
				dao.unsetPassword(docId, transaction);
			else
				throw new Exception("You cannot access the document");
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public boolean unprotect(long docId, String password) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		try {
			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			return manager.unprotect(session.getSid(), docId, password);
		} catch (Throwable t) {
			return (Boolean) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public String getContentAsString(long docId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document doc = docDao.findById(docId);
			if (doc == null)
				throw new Exception("Unexisting document");

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
		} catch (Throwable t) {
			return (String) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public GUIDocument checkinContent(long docId, String content) throws Exception {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document doc = docDao.findById(docId);
			if (doc == null)
				throw new Exception("Unexisting document");

			FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			if (!fDao.isWriteEnabled(doc.getFolder().getId(), session.getUserId()))
				throw new IOException("You don't have the WRITE permission");

			if (doc.getStatus() != AbstractDocument.DOC_CHECKED_OUT || doc.getLockUserId() != session.getUserId())
				throw new IOException("You have not checked our the file");

			History transaction = new History();
			transaction.setComment("Text content editing");
			transaction.setSession(session);

			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			manager.checkin(docId, IOUtils.toInputStream(content, "UTF-8"), doc.getFileName(), false, null, transaction);

			return getById(docId);
		} catch (Throwable t) {
			return (GUIDocument) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public GUIDocument createDocument(GUIDocument document, String content) throws Exception {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			if (!fDao.isWriteEnabled(document.getFolder().getId(), session.getUserId()))
				throw new IOException("You don't have the WRITE permission");

			History transaction = new History();
			transaction.setComment("Text content creation");
			transaction.setSession(session);

			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			Document doc = manager.create(IOUtils.toInputStream(content, "UTF-8"), toDocument(document), transaction);

			return getById(doc.getId());
		} catch (Throwable t) {
			return (GUIDocument) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public GUIRating getUserRating(long docId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
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
		} catch (Throwable t) {
			return (GUIRating) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public Integer deleteRating(long id) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			RatingDAO rDao = (RatingDAO) Context.get().getBean(RatingDAO.class);
			Rating rat = rDao.findById(id);
			if (rat != null && rat.getUserId() != session.getUserId())
				throw new Exception("Cannot delete the vote of another user");
			rDao.delete(id);
			return updateDocumentRating(rat.getDocId());
		} catch (Throwable t) {
			return (Integer) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public GUIDocument convert(long docId, String fileVersion, String format) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document doc = docDao.findById(docId);
			if (doc == null)
				throw new Exception("Unexisting document");

			FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			if (!fDao.isWriteEnabled(doc.getFolder().getId(), session.getUserId()))
				throw new IOException("You don't have the WRITE permission");
			History transaction = new History();
			transaction.setSession(session);

			FormatConverterManager manager = (FormatConverterManager) Context.get().getBean(
					FormatConverterManager.class);
			Document conversion = manager.convert(doc, fileVersion, format, transaction);
			if (conversion == null)
				throw new Exception("Unable to convert");
			return getById(conversion.getId());
		} catch (Throwable t) {
			return (GUIDocument) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public GUIEmail extractEmail(long docId, String fileVersion) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		InputStream is = null;
		try {
			GUIDocument doc = getById(docId);
			if (!doc.getFileName().toLowerCase().endsWith(".eml") && !doc.getFileName().toLowerCase().endsWith(".msg"))
				throw new Exception("Not an email file");

			Storer storer = (Storer) Context.get().getBean(Storer.class);
			String resource = storer.getResourceName(docId, fileVersion, null);
			is = storer.getStream(docId, resource);

			EMail email = null;
			if (doc.getFileName().toLowerCase().endsWith(".eml"))
				email = MailUtil.messageToMail(is, false);
			else
				email = MailUtil.msgToMail(is, false);

			GUIEmail guiMail = new GUIEmail();
			if (email.getFrom() != null)
				guiMail.setFrom(new GUIContact(email.getFrom().getName(), null, email.getFrom().getAddress()));
			if (email.getReplyTo() != null)
				guiMail.setReplyTo(new GUIContact(email.getReplyTo().getName(), null, email.getReplyTo().getAddress()));
			guiMail.setSent(email.getSentDate());
			guiMail.setReceived(email.getReceivedDate());
			guiMail.setSubject(email.getSubject());
			guiMail.setMessage(email.isHtml() ? HTMLSanitizer.sanitize(email.getMessageText()) : email.getMessageText());

			Set<Recipient> recipients = email.getRecipients();
			List<GUIContact> contacts = new ArrayList<GUIContact>();
			for (Recipient rec : recipients)
				contacts.add(new GUIContact(rec.getName(), null, rec.getAddress()));
			guiMail.setTos(contacts.toArray(new GUIContact[0]));

			recipients = email.getRecipientsCC();
			contacts = new ArrayList<GUIContact>();
			for (Recipient rec : recipients)
				contacts.add(new GUIContact(rec.getName(), null, rec.getAddress()));
			guiMail.setCcs(contacts.toArray(new GUIContact[0]));

			recipients = email.getRecipientsBCC();
			contacts = new ArrayList<GUIContact>();
			for (Recipient rec : recipients)
				contacts.add(new GUIContact(rec.getName(), null, rec.getAddress()));
			guiMail.setBccs(contacts.toArray(new GUIContact[0]));

			List<GUIDocument> attachments = new ArrayList<GUIDocument>();
			for (int i = 1; i <= email.getAttachmentsCount(); i++) {
				EMailAttachment att = email.getAttachment(i);
				GUIDocument d = new GUIDocument();
				d.setFileName(att.getFileName());
				d.setFileSize(new Float(att.getSize()));
				d.setIcon(IconSelector.selectIcon(FilenameUtils.getExtension(att.getFileName())));
				d.setFolder(doc.getFolder());
				attachments.add(d);
			}
			guiMail.setAttachments(attachments.toArray(new GUIDocument[0]));

			return guiMail;
		} catch (Throwable t) {
			return (GUIEmail) ServiceUtil.throwServerException(session, log, t);
		} finally {
			IOUtils.closeQuietly(is);
		}
	}

	@Override
	public GUIDocument saveEmailAttachment(long docId, String fileVersion, String attachmentFileName)
			throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		InputStream is = null;
		File tmp = null;
		try {
			tmp = File.createTempFile("attcopy", null);
			GUIDocument doc = getById(docId);
			if (!doc.getFileName().toLowerCase().endsWith(".eml") && !doc.getFileName().toLowerCase().endsWith(".msg"))
				throw new Exception("Not an email file");
			ServiceUtil.checkPermission(Permission.WRITE, session.getUser(), doc.getFolder().getId());

			Storer storer = (Storer) Context.get().getBean(Storer.class);
			String resource = storer.getResourceName(docId, fileVersion, null);
			is = storer.getStream(docId, resource);

			EMail email = MailUtil.messageToMail(is, false);
			EMailAttachment attachment = null;
			if (email.getAttachments().size() > 0)
				for (EMailAttachment att : email.getAttachments().values()) {
					if (attachmentFileName.equals(att.getFileName())) {
						attachment = att;
						break;
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

			History transaction = new History();
			transaction.setSession(session);
			Document d = manager.create(tmp, docVO, transaction);
			return getById(d.getId());
		} catch (Throwable t) {
			return (GUIDocument) ServiceUtil.throwServerException(session, log, t);
		} finally {
			IOUtils.closeQuietly(is);
			FileUtil.strongDelete(tmp);
		}
	}

	@Override
	public GUIDocument replaceAlias(long aliasId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			History transaction = new History();
			transaction.setSession(session);
			Document doc = manager.replaceAlias(aliasId, transaction);
			return getDocument(session, doc.getId());
		} catch (Throwable t) {
			return (GUIDocument) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void deDuplicate(Long folderId, boolean retainNewest) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		ServiceUtil.checkMenu(getThreadLocalRequest(), Menu.REPORTS);

		try {
			// First of all, find all duplicates digests
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			List<String> digests = docDao.findDuplicatedDigests(session.getTenantId(), folderId);
			log.info("Found {} duplicated digests", digests.size());

			StringBuffer duplicationsQuery = new StringBuffer(
					"select ld_id, ld_digest, ld_date, ld_folderid from ld_document where ld_deleted=0 ");
			duplicationsQuery.append(" and ld_tenantid = ");
			duplicationsQuery.append(Long.toString(session.getTenantId()));
			duplicationsQuery.append(" and ld_docref is null and ld_digest in ('");
			duplicationsQuery.append(String.join("','", digests));
			duplicationsQuery.append("') order by ld_digest asc, ld_date ");
			if (retainNewest)
				duplicationsQuery.append(" desc ");
			else
				duplicationsQuery.append(" asc ");

			@SuppressWarnings("unchecked")
			List<Document> duplications = (List<Document>) docDao.query(duplicationsQuery.toString(), null,
					new RowMapper<Document>() {
						public Document mapRow(ResultSet rs, int rowNum) throws SQLException {
							Document doc = new Document();
							doc.setTenantId(session.getTenantId());
							doc.setId(rs.getLong(1));
							doc.setDigest(rs.getString(2));
							doc.setDate(new Date(rs.getTimestamp(3).getTime()));

							Folder folder = new Folder();
							folder.setId(rs.getLong(4));
							folder.setTenantId(session.getTenantId());
							doc.setFolder(folder);

							return doc;
						}
					}, null);
			log.info("Found {} duplicated files to deduplicate", duplications.size());

			List<Document> currentDuplications = new ArrayList<Document>();
			String currentDigest = null;
			for (Document doc : duplications) {
				if (currentDigest != null && !currentDigest.equals(doc.getDigest()))
					deduplicateDocuments(session, currentDuplications);
				currentDuplications.add(doc);
				currentDigest = doc.getDigest();
			}

			if (!currentDuplications.isEmpty())
				deduplicateDocuments(session, currentDuplications);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	private void deduplicateDocuments(Session session, List<Document> duplications) throws Exception {
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document maintainedDoc = docDao.findById(duplications.get(0).getId());
		log.info("Process digest {}, retain document {} dated {}", maintainedDoc.getDigest(), maintainedDoc,
				maintainedDoc.getDate());

		duplications.remove(0);

		// Delete the other files
		History transaction = new History();
		transaction.setSession(session);
		docDao.deleteAll(duplications, transaction);

		// Create the aliases
		FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		for (Document duplicate : duplications) {
			transaction = new History();
			transaction.setSession(session);
			Document alias = manager.createAlias(maintainedDoc, fDao.findById(duplicate.getFolder().getId()), null,
					transaction);
			log.info("Created new alias {}", alias);
		}

		duplications.clear();
	}
}