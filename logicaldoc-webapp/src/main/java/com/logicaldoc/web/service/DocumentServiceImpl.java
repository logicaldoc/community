package com.logicaldoc.web.service;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
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
import java.util.Set;
import java.util.stream.Collectors;

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
import com.logicaldoc.core.PersistenceException;
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
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.core.ticket.Ticket;
import com.logicaldoc.core.ticket.TicketDAO;
import com.logicaldoc.core.transfer.InMemoryZipImport;
import com.logicaldoc.core.transfer.ZipExport;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.gui.common.client.AccessDeniedException;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.ServerException;
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
			} catch (Throwable e) {
				ServiceUtil.throwServerException(session, log, e);
			}
		}
	}

	private void index(Long[] docIds, Session session) throws Exception {
		if (docIds == null)
			return;
		log.info("Indexing documents {}", Arrays.toString(docIds));
		DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		for (Long id : docIds) {
			if (id != null) {
				DocumentHistory transaction = new DocumentHistory();
				transaction.setSession(session);
				documentManager.reindex(id, null, transaction);
			}
		}
	}

	@Override
	public void indexDocuments(Long[] docIds) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			Runnable runnable = new Runnable() {

				@Override
				public void run() {
					try {
						index(docIds, session);
					} catch (Exception e) {
						throw new RuntimeException("Error indexing the document", e);
					}
				}
			};

			ServiceUtil.executeLongRunningOperation("Index Documents", runnable, session);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public GUIDocument[] addDocuments(boolean importZip, String charset, boolean immediateIndexing,
			final GUIDocument metadata) throws ServerException {
		final Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			List<GUIDocument> createdDocs = new ArrayList<GUIDocument>();
//			HttpServletRequest servletRequest = getThreadLocalRequest();

			Runnable runnable = new Runnable() {

				@Override
				public void run() {
					List<Document> docs = new ArrayList<Document>();

//					Map<String, File> uploadedFilesMap = GWTUploadServlet.getReceivedFiles(servletRequest,
//							session.getSid());
					Map<String, File> uploadedFilesMap = UploadServlet.getReceivedFiles(session.getSid());
					log.debug("Uploading {} files", uploadedFilesMap.size());

//					Map<String, String> uploadedFileNames = GWTUploadServlet.getReceivedFileNames(servletRequest,
//							session.getSid());

					DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);

					FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
					Folder parent;
					try {
						parent = folderDao.findFolder(metadata.getFolder().getId());
					} catch (PersistenceException e1) {
						throw new RuntimeException(e1.getMessage());
					}

					if (uploadedFilesMap.isEmpty())
						throw new RuntimeException(new Exception("No file uploaded"));

					FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
					if (!fdao.isWriteEnabled(metadata.getFolder().getId(), session.getUserId()))
						throw new RuntimeException("The user doesn't have the write permission on the current folder");

					List<Long> docsToIndex = new ArrayList<Long>();
					for (String filename : uploadedFilesMap.keySet()) {
						final File file = uploadedFilesMap.get(filename);
						try {
							if (filename.toLowerCase().endsWith(".zip") && importZip) {
								log.debug("zip file = {}", file);

								// copy the zip into a temporary file
								final File tempZip = File.createTempFile("upload-", ".zip");
								FileUtils.copyFile(file, tempZip);

								final long userId = session.getUserId();
								final String sessionId = session.getSid();
								// Prepare the import thread
								Thread zipImporter = new Thread(new Runnable() {
									public void run() {
										/*
										 * Prepare the Master document used to
										 * create the new one
										 */
										try {
											Document doc = toDocument(metadata);
											doc.setTenantId(session.getTenantId());
											doc.setCreation(new Date());

											InMemoryZipImport importer = new InMemoryZipImport(doc, charset);
											importer.process(tempZip, parent, userId, sessionId);
										} catch (Throwable e) {
											log.error("Unable to delete temporary file {}", tempZip.getAbsolutePath(),
													e);
										} finally {
											FileUtil.strongDelete(tempZip);
										}
									}
								});

								// And launch it
								zipImporter.start();
							} else {
								// Create the document history event
								DocumentHistory transaction = new DocumentHistory();
								transaction.setSession(session);
								transaction.setEvent(DocumentEvent.STORED.toString());
								transaction.setComment(metadata.getComment());

								/*
								 * Prepare the Master document used to create
								 * the new one
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
						} catch (Throwable t) {
							throw new RuntimeException(t.getMessage(), t);
						} finally {
							FileUtil.strongDelete(file);
						}
					}

					try {
						for (String uploadedEntry : uploadedFilesMap.keySet())
							FileUtil.strongDelete(uploadedFilesMap.get(uploadedEntry));
					} catch (Throwable t) {

					}

					if (!docsToIndex.isEmpty())
						try {
							index(docsToIndex.toArray(new Long[0]), session);
						} catch (Exception e1) {
							throw new RuntimeException(e1.getMessage(), e1);
						}

					/*
					 * We have to notify the specified users in a separate
					 * thread
					 */
					if (metadata.getNotifyUsers() != null && metadata.getNotifyUsers().length > 0) {
						Thread notifier = new Thread(new Runnable() {
							public void run() {
								try {
									Map<Locale, Set<Recipient>> emailRecipientsMap = new HashMap<Locale, Set<Recipient>>();
									Map<Locale, Set<Recipient>> systemRecipientsMap = new HashMap<Locale, Set<Recipient>>();
									prepareRecipients(metadata.getNotifyUsers(), emailRecipientsMap,
											systemRecipientsMap);

									SystemMessageDAO systemMessageDao = (SystemMessageDAO) Context.get()
											.getBean(SystemMessageDAO.class);
									for (Locale locale : emailRecipientsMap.keySet()) {
										EMail mail = new EMail();
										mail.setHtml(1);
										mail.setTenantId(session.getTenantId());
										mail.setAccountId(-1);
										mail.setAuthor(session.getUser().getUsername());

										ContextProperties config = (ContextProperties) Context.get().getProperties();
										if (config.getBoolean(session.getTenantName() + ".smtp.userasfrom", true))
											mail.setAuthorAddress(session.getUser().getEmail());

										mail.setFolder("outbox");
										mail.setSentDate(new Date());
										mail.setUsername(session.getUsername());
										mail.setRecipients(emailRecipientsMap.get(locale));

										MessageTemplateDAO tDao = (MessageTemplateDAO) Context.get()
												.getBean(MessageTemplateDAO.class);
										MessageTemplate template = tDao.findByNameAndLanguage("newdoc",
												locale.toString(), mail.getTenantId());

										Map<String, Object> dictionary = new HashMap<String, Object>();
										dictionary.put("creator", session.getUser());
										dictionary.put("documents", docs);
										dictionary.put("document", docs.get(0));
										dictionary.put("message", metadata.getNotifyMessage());
										dictionary.put(Automation.LOCALE, locale);

										mail.setSubject(template.getFormattedSubject(dictionary));
										mail.setMessageText("<html><body>" + template.getFormattedBody(dictionary)
												+ "</html></body>");

										log.info("Notify the creation of new documents {} to {}", docs.toString(),
												mail.getRecipients().toString());
										EMailSender sender = new EMailSender(session.getTenantName());
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
								} catch (Throwable e) {
									log.warn(e.getMessage(), e);
								}
							}
						});
						notifier.start();
					}
				}
			};

			if (ServiceUtil.executeLongRunningOperation("Add Documents", runnable, session))
				return createdDocs.toArray(new GUIDocument[0]);
			else
				return null;
		} catch (Throwable t) {
			return (GUIDocument[]) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public GUIDocument checkin(GUIDocument document, boolean major) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		Map<String, File> uploadedFilesMap = UploadServlet.getReceivedFiles(session.getSid());
		File file = uploadedFilesMap.values().iterator().next();
		String fileName = uploadedFilesMap.keySet().iterator().next();

		if (file != null) {
			log.debug("Checking in file {}", fileName);

			try {
				// Create the document history event
				DocumentHistory transaction = new DocumentHistory();
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
				try (FileInputStream fis = new FileInputStream(file)) {
					documentManager.checkin(doc.getId(), fis, fileName, major, toDocument(document), transaction);
				}
				UploadServlet.cleanReceivedFiles(session.getSid());
				GUIDocument checkedInDocument = getById(doc.getId());

				/*
				 * We have to notify the specified users in a separate thread
				 */
				if (document.getNotifyUsers() != null && document.getNotifyUsers().length > 0) {
					Thread notifier = new Thread(new Runnable() {
						public void run() {
							try {
								SystemMessageDAO systemMessageDao = (SystemMessageDAO) Context.get()
										.getBean(SystemMessageDAO.class);
								DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
								Document doc = dao.findById(document.getId());
								dao.initialize(doc);

								String tile = prepareTileAsString(session, doc);

								Map<Locale, Set<Recipient>> emailRecipientsMap = new HashMap<Locale, Set<Recipient>>();
								Map<Locale, Set<Recipient>> systemRecipientsMap = new HashMap<Locale, Set<Recipient>>();
								prepareRecipients(document.getNotifyUsers(), emailRecipientsMap, systemRecipientsMap);

								for (Locale locale : emailRecipientsMap.keySet()) {
									EMail mail = new EMail();
									mail.setHtml(1);
									mail.setTenantId(session.getTenantId());
									mail.setAccountId(-1);
									mail.setAuthor(session.getUser().getUsername());

									ContextProperties config = (ContextProperties) Context.get().getProperties();
									if (config.getBoolean(session.getTenantName() + ".smtp.userasfrom", true))
										mail.setAuthorAddress(session.getUser().getEmail());

									mail.setFolder("outbox");
									mail.setSentDate(new Date());
									mail.setUsername(session.getUsername());
									mail.setRecipients(emailRecipientsMap.get(locale));

									MessageTemplateDAO tDao = (MessageTemplateDAO) Context.get()
											.getBean(MessageTemplateDAO.class);
									MessageTemplate template = tDao.findByNameAndLanguage("checkin", locale.toString(),
											mail.getTenantId());

									Map<String, Object> dictionary = new HashMap<String, Object>();
									dictionary.put("user", session.getUser());
									dictionary.put("document", doc);
									dictionary.put("message", document.getNotifyMessage());
									dictionary.put(Automation.LOCALE, locale);
									dictionary.put("tile", tile);

									mail.setSubject(template.getFormattedSubject(dictionary));
									mail.setMessageText(
											"<html><body>" + template.getFormattedBody(dictionary) + "</html></body>");

									log.info("Notify the checkin of document {} to {}", doc,
											mail.getRecipients().toString());
									EMailSender sender = new EMailSender(session.getTenantName());
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
							} catch (Throwable e) {
								log.warn(e.getMessage(), e);
							}
						}

					});
					notifier.start();
				}

				return checkedInDocument;
			} catch (Throwable t) {
				return (GUIDocument) ServiceUtil.throwServerException(session, log, t);
			}
		} else
			return null;
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
					Set<Recipient> recipients = new HashSet<Recipient>();
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
				Set<Recipient> recipients = new HashSet<Recipient>();
				recipients.add(rec);
				systemRecipientsMap.put(user.getLocale(), recipients);
			}
		}
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
	public GUIDocument promoteVersion(long docId, String version) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		log.debug("Promoting version {} of document {}", version, docId);

		try {
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document doc = docDao.findById(docId);
			if (doc == null)
				throw new Exception("Unexisting document");

			FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			if (!fDao.isWriteEnabled(doc.getFolder().getId(), session.getUserId()))
				throw new IOException("You don't have the WRITE permission");

			doc = docDao.findDocument(docId);

			if (doc.getStatus() != AbstractDocument.DOC_UNLOCKED)
				throw new IOException("The document is locked");

			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			transaction.setDocument(doc);

			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			manager.promoteVersion(doc.getId(), version, transaction);

			return getById(doc.getId());
		} catch (Throwable t) {
			return (GUIDocument) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void checkout(long[] docIds) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

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
					documentManager.checkout(doc.getId(), transaction.clone());
			}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void lock(long[] docIds, String comment) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		// Unlock the document; throws an exception if something
		// goes wrong
		DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		try {
			// Create the document history event
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			transaction.setEvent(DocumentEvent.LOCKED.toString());
			transaction.setComment(comment);
			for (long id : docIds) {
				Document doc = dao.findDocument(id);
				if (doc != null)
					documentManager.lock(doc.getId(), Document.DOC_LOCKED, transaction.clone());
			}
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
					DocumentHistory transaction = new DocumentHistory();
					transaction.setSession(session);
					transaction.setEvent(DocumentEvent.DELETED.toString());
					transaction.setComment("");

					// If it is a shortcut, we delete only the shortcut
					if (doc.getDocRef() != null) {
						transaction.setEvent(DocumentEvent.SHORTCUT_DELETED.toString());
						boolean deleted = dao.delete(doc.getId(), transaction);
						if (!deleted)
							throw new Exception("Document has not been deleted");
						continue;
					}

					// The document of the selected documentRecord must be
					// not immutable
					if (doc.getImmutable() == 1 && !transaction.getUser().isMemberOf("admin")) {
						log.debug("Document {} was not deleted because immutable", id);
						continue;
					}

					// The document must be not locked
					if (doc.getStatus() == Document.DOC_LOCKED) {
						log.debug("Document {} was not deleted because locked", id);
						continue;
					}

					// Check if there are some shortcuts associated to the
					// deleting document. All the shortcuts must be deleted.
					if (dao.findAliasIds(doc.getId()).size() > 0)
						for (Long shortcutId : dao.findAliasIds(doc.getId())) {
							dao.delete(shortcutId);
						}
					boolean deleted = dao.delete(doc.getId(), transaction);
					if (!deleted)
						throw new Exception("Document has not been deleted");
				} catch (Throwable t) {
					ServiceUtil.throwServerException(session, log, t);
				}
			}
		}
	}

	@Override
	public void deleteBookmarks(long[] bookmarkIds) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			BookmarkDAO dao = (BookmarkDAO) Context.get().getBean(BookmarkDAO.class);
			for (long id : bookmarkIds) {
				boolean deleted = dao.delete(id);
				if (!deleted)
					throw new Exception("Bookmarks have not been deleted");
			}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void deleteLinks(long[] ids) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentLinkDAO dao = (DocumentLinkDAO) Context.get().getBean(DocumentLinkDAO.class);
			for (long id : ids) {
				boolean deleted = dao.delete(id);
				if (!deleted)
					throw new Exception("Bookmarks have not been deleted");
			}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public GUIDocument getById(long docId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		try {
			GUIDocument doc = getDocument(session, docId);
			return doc;
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
			FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			fDao.initialize(doc.getFolder());
			folder = FolderServiceImpl.fromFolder(doc.getFolder(), false);

//			Long aliasId = null;
//			String aliasFileName = null;
//			String aliasType = null;
//			String aliasColor = null;
//
//			// Check if it is an alias
//			if (doc.getDocRef() != null) {
//				aliasFileName = doc.getFileName();
//				aliasType = doc.getType();
//				aliasColor = doc.getColor();
//				long id = doc.getDocRef();
//				doc = docDao.findById(id);
//				aliasId = docId;
//			}

			if (session != null)
				checkPublished(session.getUser(), doc);

			docDao.initialize(doc);

			document = fromDocument(doc, folder, session != null ? session.getUser() : null);

//			if (aliasId != null)
//				document.setDocRef(aliasId);
//			if (StringUtils.isNotEmpty(aliasFileName))
//				document.setFileName(aliasFileName);
//			if (StringUtils.isNotEmpty(aliasColor))
//				document.setColor(aliasColor);
//			if (StringUtils.isNotEmpty(aliasType)) {
//				document.setType(aliasType);
//			}

			if (session != null && folder != null) {
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

	public static GUIDocument fromDocument(Document doc, GUIFolder folder, User sessionUser)
			throws PersistenceException {
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
		if (realDoc.getTags().size() > 0)
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
		document.setIcon(FilenameUtils.getBaseName(doc.getIcon()));
		document.setPasswordProtected(realDoc.isPasswordProtected());
		document.setLinks(realDoc.getLinks());
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

		if (sessionUser != null && !isFolder) {
			BookmarkDAO bDao = (BookmarkDAO) Context.get().getBean(BookmarkDAO.class);
			document.setBookmarked(bDao.isDocBookmarkedByUser(document.getId(), sessionUser.getId()));
			if (document.getDocRef() != null)
				document.setBookmarked(bDao.isDocBookmarkedByUser(document.getDocRef(), sessionUser.getId()));
		}

		GUIAttribute[] attributes = TemplateServiceImpl.prepareGUIAttributes(realDoc.getTemplate(), realDoc,
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

				version1.setTemplate(docVersion.getTemplateName());
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
					att.setMultiple(extAttr.getMultiple() == 1);
					att.setParent(extAttr.getParent());
					att.setStringValues(extAttr.getStringValues());
					att.setEditor(extAttr.getEditor());
					att.setStringValue(extAttr.getStringValue());
					att.setIntValue(extAttr.getIntValue());
					att.setBooleanValue(extAttr.getBooleanValue());
					att.setDoubleValue(extAttr.getDoubleValue());
					att.setType(extAttr.getType());
					version1.addAttribute(att);
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

				version2.setTemplateId(docVersion.getTemplateId());
				version2.setTemplate(docVersion.getTemplateName());
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
					att.setMultiple(extAttr.getMultiple() == 1);
					att.setParent(extAttr.getParent());
					att.setEditor(extAttr.getEditor());
					att.setStringValue(extAttr.getStringValue());
					att.setIntValue(extAttr.getIntValue());
					att.setBooleanValue(extAttr.getBooleanValue());
					att.setDoubleValue(extAttr.getDoubleValue());
					att.setType(extAttr.getType());
					version2.addAttribute(att);
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
			log.error("Exception linking documents: {}", t.getMessage(), t);
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
					DocumentHistory transaction = new DocumentHistory();
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
		DocumentHistoryDAO dao = (DocumentHistoryDAO) Context.get().getBean(DocumentHistoryDAO.class);
		dao.markHistoriesAsRead(event, session.getUserId());
	}

	@Override
	public void markIndexable(long[] docIds, int policy) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			for (long id : docIds)
				manager.changeIndexingStatus(docDao.findById(id), policy);
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
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			Document object = toDocument(document);

			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			transaction.setEvent(
					document.getId() == 0L ? DocumentEvent.CHANGED.toString() : DocumentEvent.STORED.toString());
			transaction.setComment(document.getComment());

			Validator validator = new Validator();
			validator.validate(object, object.getTemplate(), transaction);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public GUIDocument save(GUIDocument guiDocument) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document document = docDao.findById(guiDocument.getId());
			docDao.initialize(document);

			if (guiDocument.getId() != 0) {
				if (guiDocument.getDocRef() != null) {
					/*
					 * We are saving an alias
					 */

					document.setFileName(guiDocument.getFileName());
					document.setColor(guiDocument.getColor());
					document.setType(FileUtil.getExtension(document.getFileName()).toLowerCase());
					boolean stored = docDao.store(document);
					if (!stored)
						throw new Exception("Alias not stored");

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
				transaction.setComment(guiDocument.getComment());

				DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
				documentManager.update(document, docVO, transaction);
				return getById(guiDocument.getId());
			} else
				return null;
		} catch (Throwable t) {
			return (GUIDocument) ServiceUtil.throwServerException(session, log, t);
		}
	}

	/**
	 * Produces a plain new Document from a GUIDocument
	 * 
	 * @param guiDocument the GUI document
	 * 
	 * @return the core Document
	 * 
	 * @throws PersistenceException error in the database
	 */
	public static Document toDocument(GUIDocument guiDocument) throws PersistenceException {
		Document docVO = new Document();
		if (guiDocument.getTags() != null && guiDocument.getTags().length > 0)
			docVO.setTagsFromWords(new HashSet<String>(Arrays.asList(guiDocument.getTags())));

		docVO.setCustomId(guiDocument.getCustomId());
		docVO.setFileName(guiDocument.getFileName());
		docVO.setVersion(guiDocument.getVersion());
		docVO.setCreation(guiDocument.getCreation());
		docVO.setCreator(guiDocument.getCreator());
		docVO.setDate(guiDocument.getDate());
		docVO.setPublisher(guiDocument.getPublisher());
		docVO.setFileVersion(guiDocument.getFileVersion());
		docVO.setLanguage(guiDocument.getLanguage());
		docVO.setFileSize(guiDocument.getFileSize());

		docVO.setRating(guiDocument.getRating());
		docVO.setComment(guiDocument.getComment());
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
						// This check is useful to avoid errors
						// related to the old template
						// attributes keys that remains on the form
						// value manager
						if (attr.getValue() != null && attr.getValue().toString().trim().isEmpty()
								&& templateType != 0) {
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
						} else if (templateType == GUIAttribute.TYPE_USER || templateType == GUIAttribute.TYPE_FOLDER) {
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
						} else if (templateType == Attribute.TYPE_USER || templateType == Attribute.TYPE_FOLDER) {
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
		}

		docVO.setStatus(guiDocument.getStatus());
		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		if (guiDocument.getFolder() != null)
			docVO.setFolder(fdao.findById(guiDocument.getFolder().getId()));
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

			if (email.getBccs() != null && email.getBccs().length > 0) {
				StringBuffer sb = new StringBuffer();
				for (GUIContact contact : email.getBccs()) {
					if (sb.length() > 0)
						sb.append(",");
					sb.append(contact.getEmail());
				}
				mail.parseRecipientsBCC(sb.toString());
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

			if (email.isSendAsTicket()) {
				// Prepare a new download ticket
				DocumentHistory transaction = new DocumentHistory();
				transaction.setSession(session);

				Document doc = documentDao.findDocument(email.getDocIds()[0]);
				Ticket ticket = manager.createDownloadTicket(email.getDocIds()[0], null, null, null, null, null,
						transaction);
				String ticketDiv = "<div style='margin-top:10px; border-top:1px solid black; background-color:#CCCCCC;'><b>&nbsp;"
						+ I18N.message("clicktodownload", LocaleUtil.toLocale(locale)) + ": <a href='" + ticket.getUrl()
						+ "'>" + doc.getFileName() + "</a></b></div>";
				dictionary.put("downloadTicket", ticketDiv);
			}

			String message = engine.evaluate(email.getMessage(), dictionary);
			mail.setSubject(engine.evaluate(email.getSubject(), dictionary));

			if (email.isSendAsTicket()) {
				if (!email.getMessage().contains("downloadTicket"))
					message += "<br/><br/>" + dictionary.get("downloadTicket");

				Document doc = documentDao.findDocument(email.getDocIds()[0]);

				if (doc.getDocRef() != null)
					doc = documentDao.findById(doc.getDocRef());

				try {
					thumbnailFile = createTile(doc, session.getSid());
					if (thumbnailFile != null) {
						String thumb = thumbnailFile.toURI().toURL().toString();
						mail.getImages().add(thumb);
						message += "<p><img src='cid:image_1'/></p>";
					}
				} catch (IOException ioe) {
					log.warn(ioe.getMessage());
				}

				mail.setMessageText(
						"<html><head><meta charset='utf-8' /></head><body>" + message + "<rl /></body></html>");
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
						DocumentHistory transaction = new DocumentHistory();
						transaction.setSession(session);
						transaction.setEvent(DocumentEvent.DOWNLOADED.toString());

						ZipExport export = new ZipExport();
						export.process(ArrayUtils.toObject(email.getDocIds()), out, email.isPdfConversion(),
								transaction);
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
				} catch (Throwable e) {
				}
			if (thumbnailFile != null)
				try {
					FileUtils.forceDelete(thumbnailFile);
				} catch (Throwable e) {
				}
		}
	}

	private File createTile(Document doc, String sid) throws IOException {
		Storer storer = (Storer) Context.get().getBean(Storer.class);
		String tileResource = storer.getResourceName(doc, doc.getFileVersion(), ThumbnailManager.SUFFIX_TILE);

		// In any case try to produce the thumbnail
		if (storer.size(doc.getId(), tileResource) <= 0L) {
			ThumbnailManager thumbManager = (ThumbnailManager) Context.get().getBean(ThumbnailManager.class);
			try {
				thumbManager.createTile(doc, doc.getFileVersion(), sid);
			} catch (Throwable t) {
				log.error(t.getMessage(), t);
			}
		}

		if (storer.exists(doc.getId(), tileResource)) {
			File file = File.createTempFile("tile-", ".png");
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
			if (!"pdf".equals(FileUtil.getExtension(doc.getFileName().toLowerCase()))) {
				FormatConverterManager manager = (FormatConverterManager) Context.get()
						.getBean(FormatConverterManager.class);
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
			DocumentHistory transaction = new DocumentHistory();
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

		File dir = new File(System.getProperty("java.io.tmpdir") + "/upload/" + session.getSid());
		if (dir.exists())
			FileUtil.strongDelete(dir);
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

			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			ratingDao.store(rat, transaction);

			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document doc = docDao.findById(rating.getDocId());
			return doc.getRating();
		} catch (Throwable t) {
			return (Integer) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public long addNote(long docId, String message) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document document = docDao.findDocument(docId);
			if (document == null)
				throw new ServerException("Unexisting document " + docId);

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
		} catch (Throwable t) {
			return (Integer) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public GUIDocumentNote[] getNotes(long docId, String fileVersion, Collection<String> types) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document document = docDao.findDocument(docId);
			if (document == null)
				throw new ServerException("Unexisting document " + docId);

			List<GUIDocumentNote> guiNotes = new ArrayList<GUIDocumentNote>();
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
		} catch (Throwable t) {
			return (GUIDocumentNote[]) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void saveNotes(long docId, GUIDocumentNote[] notes, Collection<String> types) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentNoteDAO dao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document document = docDao.findDocument(docId);
			if (document == null)
				throw new ServerException("Unexisting document " + docId);

			List<GUIDocumentNote> notesList = new ArrayList<GUIDocumentNote>();
			if (notes != null && notes.length > 0)
				notesList = Arrays.asList(notes);

			/*
			 * Check for deletions
			 */
			List<DocumentNote> documentNotes = dao.findByDocIdAndTypes(document.getId(), document.getFileVersion(),
					types);
			List<Long> actualNoteIds = documentNotes.stream().map(n -> n.getId()).collect(Collectors.toList());
			List<Long> noteIds = notesList.stream().map(n -> n.getId()).collect(Collectors.toList());
			for (Long actualNoteId : actualNoteIds)
				if (!noteIds.contains(actualNoteId))
					dao.delete(actualNoteId);

			/*
			 * Do the updates / inserts
			 */
			for (GUIDocumentNote guiNote : notesList) {
				DocumentNote note = dao.findById(guiNote.getId());
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

				if (note.getId() == 0L) {
					DocumentHistory transaction = new DocumentHistory();
					transaction.setSession(session);
					dao.store(note, transaction);
				} else {
					dao.store(note);
				}

				/*
				 * If the note specifies a recipient, update the user's address
				 * book
				 */
				if (StringUtils.isNotEmpty(note.getRecipientEmail())) {
					ContactDAO cDao = (ContactDAO) Context.get().getBean(ContactDAO.class);
					List<Contact> contacts = cDao.findByUser(session.getUserId(), note.getRecipientEmail());
					if (contacts.isEmpty()) {
						try {
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
							cDao.store(contact);
						} catch (Throwable t) {
							log.warn("Error storing new contact {}", note.getRecipientEmail(), t);
						}
					}
				}
			}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void deleteNotes(long[] ids) throws ServerException {
		ServiceUtil.validateSession(getThreadLocalRequest());

		DocumentNoteDAO dao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);
		for (long id : ids)
			try {
				dao.delete(id);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
	}

	@Override
	public GUIDocument[] bulkUpdate(long[] ids, GUIDocument vo, boolean ignoreEmptyFields) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		try {
			List<GUIDocument> updatedDocs = new ArrayList<GUIDocument>();
			for (long id : ids) {
				try {
					GUIDocument buf = getById(id);

					if (buf.getImmutable() == 1 || buf.getStatus() != Document.DOC_UNLOCKED) {
						log.warn("Skip bulk update of document {} because it is locked or immutable", buf.getId());
						continue;
					}

					try {
						ServiceUtil.checkPermission(Permission.WRITE, session.getUser(), buf.getFolder().getId());
					} catch (AccessDeniedException e) {
						log.warn(
								"Skip bulk update of document {} because the user {} does not have the write permission",
								buf.getId(), session.getUsername());
						continue;
					}

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

					if (vo.getOcrTemplateId() != null)
						buf.setOcrTemplateId(vo.getOcrTemplateId());
					else if (!ignoreEmptyFields)
						buf.setOcrTemplateId(null);

					if (vo.getBarcodeTemplateId() != null)
						buf.setBarcodeTemplateId(vo.getBarcodeTemplateId());
					else if (!ignoreEmptyFields)
						buf.setBarcodeTemplateId(null);

					if (vo.getAttributes() != null && vo.getAttributes().length > 0) {
						if (ignoreEmptyFields) {
							Map<String, GUIAttribute> attributes = new HashMap<String, GUIAttribute>();
							for (GUIAttribute att : buf.getAttributes())
								attributes.put(att.getName(), att);

							for (GUIAttribute att : vo.getAttributes()) {
								if (att.getValue() != null && StringUtils.isNotEmpty(att.getValue().toString()))
									attributes.put(att.getName(), att);
							}
							buf.setAttributes(attributes.values().toArray(new GUIAttribute[0]));
						} else {
							buf.setAttributes(vo.getAttributes());
						}
					}

					updatedDocs.add(save(buf));
				} catch (Throwable e) {
					log.error(e.getMessage(), e);
				}
			}
			return updatedDocs.toArray(new GUIDocument[0]);
		} catch (Throwable t) {
			return (GUIDocument[]) ServiceUtil.throwServerException(session, log, t);
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
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document document = docDao.findDocument(docId);
			if (document == null)
				throw new ServerException("Unexisting document " + docId);

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

			if (note.getId() == 0L) {
				DocumentHistory transaction = new DocumentHistory();
				transaction.setSession(session);
				dao.store(note, transaction);
			} else {
				dao.store(note);
			}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public GUIDocument deleteVersions(long[] ids) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		long docId = 0;
		try {
			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			for (long id : ids) {
				DocumentHistory transaction = new DocumentHistory();
				transaction.setSession(session);
				Version version = manager.deleteVersion(id, transaction);
				docId = version.getDocId();
			}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}

		return getById(docId);
	}

	@Override
	public GUIDocument createWithContent(GUIDocument vo, String content) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentManager documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);

			if (!fdao.isWriteEnabled(vo.getFolder().getId(), session.getUserId())) {
				throw new RuntimeException("The user doesn't have the write permission on the current folder");
			}

			Document doc = toDocument(vo);
			doc.setId(0L);

			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			transaction.setEvent(DocumentEvent.STORED.toString());
			Document document;
			if (StringUtils.isEmpty(content))
				document = documentManager.create(IOUtils.toInputStream("", "UTF-8"), doc, transaction);
			else
				document = documentManager.create(IOUtils.toInputStream(content, "UTF-8"), doc, transaction);

			// If that VO is in checkout, perform a checkout also
			if (vo.getStatus() == Document.DOC_CHECKED_OUT) {
				transaction = new DocumentHistory();
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
			dao.bulkUpdate("set ld_deleted=2 where ld_id in " + idsStr, (Map<String, Object>) null);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void emptyTrash() throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			dao.bulkUpdate("set ld_deleted=2 where ld_deleted=1 and  ld_deleteuserid=" + session.getUserId(),
					(Map<String, Object>) null);

			FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			fdao.bulkUpdate("set ld_deleted=2 where ld_deleted=1 and  ld_deleteuserid=" + session.getUserId(),
					(Map<String, Object>) null);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void archiveDocuments(long[] docIds, String comment) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			DocumentHistory transaction = new DocumentHistory();
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
			DocumentHistory transaction = new DocumentHistory();
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
				DocumentHistory transaction = new DocumentHistory();
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

		StringBuffer query = new StringBuffer(
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
	public String[] createDownloadTicket(long docId, String suffix, Integer expireHours, Date expireDate,
			Integer maxDownloads) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			HttpServletRequest request = this.getThreadLocalRequest();
			String urlPrefix = request.getScheme() + "://" + request.getServerName() + ":" + request.getServerPort()
					+ request.getContextPath();

			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			Ticket ticket = manager.createDownloadTicket(docId, suffix, expireHours, expireDate, maxDownloads,
					urlPrefix, transaction);

			String[] result = new String[3];
			result[0] = ticket.getTicketId();
			result[1] = ticket.getUrl();
			result[2] = new URI(
					ticket.getUrl().replace(urlPrefix, Context.get().getProperties().getProperty("server.url")))
							.normalize().toString();
			return result;
		} catch (Throwable t) {
			return (String[]) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void setPassword(long docId, String password) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
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
		DocumentHistory transaction = new DocumentHistory();
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
	public GUIDocument checkinContent(long docId, String content) throws ServerException {
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

			DocumentHistory transaction = new DocumentHistory();
			transaction.setComment("Text content editing");
			transaction.setSession(session);

			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			manager.checkin(docId, IOUtils.toInputStream(content, "UTF-8"), doc.getFileName(), false, null,
					transaction);

			return getById(docId);
		} catch (Throwable t) {
			return (GUIDocument) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void replaceFile(long docId, String fileVersion, String comment) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		Map<String, File> uploadedFilesMap = UploadServlet.getReceivedFiles(session.getSid());
		File file = uploadedFilesMap.values().iterator().next();
		if (file != null) {
			try {
				DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
				Document doc = docDao.findById(docId);
				if (doc == null)
					throw new Exception("Unexisting document");

				FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
				if (!fDao.isWriteEnabled(doc.getFolder().getId(), session.getUserId()))
					throw new IOException("You don't have the WRITE permission");

				doc = docDao.findDocument(docId);

				if (doc.getStatus() != AbstractDocument.DOC_UNLOCKED)
					throw new IOException("The document is locked");

				DocumentHistory transaction = new DocumentHistory();
				transaction.setComment(comment);
				transaction.setSession(session);

				DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
				manager.replaceFile(doc.getId(), fileVersion, file, transaction);

				UploadServlet.cleanReceivedFiles(session.getSid());
			} catch (Throwable t) {
				ServiceUtil.throwServerException(session, log, t);
			}
		}
	}

	@Override
	public GUIDocument createDocument(GUIDocument document, String content) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			if (!fDao.isWriteEnabled(document.getFolder().getId(), session.getUserId()))
				throw new IOException("You don't have the WRITE permission");

			DocumentHistory transaction = new DocumentHistory();
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
			if (rat == null)
				return 0;

			if (rat != null && rat.getUserId() != session.getUserId())
				throw new Exception("Cannot delete the rating left by another user");
			rDao.delete(id);

			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document doc = docDao.findById(rat.getDocId());
			return doc.getRating();
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
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);

			FormatConverterManager manager = (FormatConverterManager) Context.get()
					.getBean(FormatConverterManager.class);
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
			is = storer.getStream(doc.getId(), resource);

			EMail email = null;
			try {
				if (doc.getFileName().toLowerCase().endsWith(".eml"))
					email = MailUtil.messageToMail(is, true);
				else
					email = MailUtil.msgToMail(is, true);
			} catch (Throwable t) {
				log.warn("Cannot render the email document {}", docId);
			}

			GUIEmail guiMail = new GUIEmail();

			if (email != null) {
				if (email.getFrom() != null)
					guiMail.setFrom(new GUIContact(email.getFrom().getName(), null, email.getFrom().getAddress()));

				guiMail.setSent(email.getSentDate());
				guiMail.setReceived(email.getReceivedDate() != null ? email.getReceivedDate() : doc.getCreation());
				guiMail.setSubject(email.getSubject());
				guiMail.setMessage(
						email.isHtml() ? HTMLSanitizer.sanitize(email.getMessageText()) : email.getMessageText());
				guiMail.setSigned(email.isSigned());

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

				recipients = email.getReplyTo();
				contacts = new ArrayList<GUIContact>();
				for (Recipient rec : recipients)
					contacts.add(new GUIContact(rec.getName(), null, rec.getAddress()));
				guiMail.setReplyTo(contacts.toArray(new GUIContact[0]));

				List<GUIDocument> attachments = new ArrayList<GUIDocument>();
				for (int i = 1; i <= email.getAttachmentsCount(); i++) {
					EMailAttachment att = email.getAttachment(i);
					GUIDocument d = new GUIDocument();
					d.setFileName(att.getFileName());
					d.setFileSize(att.getSize());
					d.setIcon(IconSelector.selectIcon(att.getFileName()));
					d.setFolder(doc.getFolder());
					attachments.add(d);
				}
				guiMail.setAttachments(attachments.toArray(new GUIDocument[0]));
			}
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

			DocumentHistory transaction = new DocumentHistory();
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
			DocumentHistory transaction = new DocumentHistory();
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
			List<Document> duplications = (List<Document>) docDao.query(duplicationsQuery.toString(), null,
					new RowMapper<Document>() {
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
		List<Long> duplicatedIds = duplications.stream().map(d -> d.getId()).collect(Collectors.toList());

		log.warn("Deleting the duplicated documents {}", duplicatedIds);
		StringBuffer updateStatement = new StringBuffer("update ld_document set ld_deleted=1 where ");
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
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			TicketDAO dao = (TicketDAO) Context.get().getBean(TicketDAO.class);
			dao.delete(ticketId);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void enableTicket(long ticketId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			TicketDAO dao = (TicketDAO) Context.get().getBean(TicketDAO.class);
			Ticket ticket = dao.findById(ticketId);
			ticket.setEnabled(1);
			dao.store(ticket);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void disableTicket(long ticketId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			TicketDAO dao = (TicketDAO) Context.get().getBean(TicketDAO.class);
			Ticket ticket = dao.findById(ticketId);
			ticket.setEnabled(0);
			dao.store(ticket);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void enforceFilesIntoFolderStorage(long folderId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
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
				try {
					notifyEnforcement(session, I18N.message("enforcementofstoragereport", user.getLocale(),
							new Object[] { movedFiles, treePath }));
				} catch (Throwable t) {
					log.warn(t.getMessage(), t);
				}
			} catch (Throwable t) {
				log.error("Error enforcing files storage into tree {}", treePath, t);
				try {
					notifyEnforcement(session, I18N.message("enforcementofstorageerror", user.getLocale(),
							new Object[] { movedFiles, treePath, t.getMessage() }));
				} catch (Throwable e) {
					log.warn(e.getMessage(), e);
				}
			}
		}).start();
	}

	private void notifyEnforcement(Session session, String message) throws Exception {
		User user = session.getUser();

		// Prepare the system message
		Recipient sysRecipient = new Recipient();
		sysRecipient.setName(user.getUsername());
		sysRecipient.setAddress(user.getEmail());
		sysRecipient.setType(Recipient.TYPE_SYSTEM);
		sysRecipient.setMode("message");

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
		mail.setFolder("outbox");
		mail.setSentDate(new Date());
		mail.setUsername(user.getUsername());
		mail.getRecipients().add(emailRecipient);
		mail.setSubject(sys.getSubject());
		mail.setMessageText(message);

		EMailSender sender = new EMailSender(session.getTenantName());
		sender.send(mail);
	}

	@Override
	public GUIDocument merge(long[] docIds, long targetFolderId, String fileName) throws ServerException {
		final Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);

			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			List<Document> docs = new ArrayList<Document>();
			for (long docId : docIds)
				docs.add(docDao.findDocument(docId));

			Document doc = manager.merge(docs, targetFolderId,
					fileName.toLowerCase().endsWith(".pdf") ? fileName : fileName + ".pdf", transaction);
			return getDocument(session, doc.getId());
		} catch (Throwable t) {
			return (GUIDocument) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public int updatePages(long docId) throws ServerException {
		final Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document doc = docDao.findDocument(docId);
			if (doc != null) {
				docDao.initialize(doc);
				int pages = manager.countPages(doc);
				doc.setPages(pages);
				return pages;
			}
			return 1;
		} catch (Throwable t) {
			return (Integer) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public GUIDocument rename(long documentId, String name) throws ServerException {
		final Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			User user = session.getUser();
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document doc = docDao.findById(documentId);
			ServiceUtil.checkPermission(Permission.RENAME, user, doc.getFolder().getId());
			checkPublished(user, doc);

			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);

			DocumentHistory transaction = new DocumentHistory();
			transaction.setSession(session);
			transaction.setUser(user);
			manager.rename(documentId, name, transaction);

			return getDocument(session, documentId);
		} catch (Throwable t) {
			return (GUIDocument) ServiceUtil.throwServerException(session, log, t);
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
		} catch (Throwable t) {
			log.warn("Cannot generate tile of document {}", doc, t);
		} finally {
			FileUtil.strongDelete(tileFile);
		}
		return tile;
	}
}