package com.logicaldoc.core.automation;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import jakarta.mail.MessagingException;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.bouncycastle.cms.CMSException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailAttachment;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.communication.MailUtil;
import com.logicaldoc.core.communication.Message;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.communication.SystemMessage;
import com.logicaldoc.core.communication.SystemMessageDAO;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.store.Store;
import com.logicaldoc.util.MimeType;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.spring.Context;

/**
 * Utility functions to handle emails and send messages from within the
 * Automation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.1
 */
@AutomationDictionary
public class MailTool {

	private static final Logger log = LoggerFactory.getLogger(MailTool.class);

	/**
	 * Sends some documents to a recipient
	 * 
	 * @param documents collection of documents to send
	 * @param from the email address to be used as From
	 * @param to the email address of the recipient
	 * @param subject subject of the email
	 * @param message message printed in the body of the email
	 * 
	 * @throws MessagingException Cannot send the email
	 * @throws IOException I/O error
	 */
	public void sendDocuments(Collection<Document> documents, String from, String to, String subject, String message)
			throws IOException, MessagingException {
		sendDocuments(documents, from, Arrays.asList(to), subject, message);
	}

	/**
	 * Sends some documents to a selection of recipients
	 * 
	 * @param documents collection of documents to send
	 * @param from the email address to be used as From
	 * @param to collection of email addresses to send the email to
	 * @param subject subject of the email
	 * @param message message printed in the body of the email
	 * 
	 * @throws IOException I/O error
	 * 
	 */
	public void sendDocuments(Collection<Document> documents, String from, Collection<String> to, String subject,
			String message) throws IOException, MessagingException {
		if (documents == null || documents.isEmpty())
			return;

		EMail email = new EMail();
		email.setHtml(1);
		email.setTenantId(documents.iterator().next().getTenantId());
		email.setAccountId(-1);
		email.setAuthor("_workflow");

		if (to != null && !to.isEmpty())
			email.parseRecipients(String.join(",", to));

		email.setSubject(subject);
		email.setMessageText(message);

		for (Document document : documents) {
			EMailAttachment att = new EMailAttachment();
			att.setIcon(document.getIcon());
			att.setFileName(document.getFileName());
			String extension = document.getFileExtension();
			att.setMimeType(MimeType.get(extension));
			Store store = Context.get(Store.class);
			att.setData(store.getBytes(document.getId(), store.getResourceName(document, null, null)));
			email.addAttachment(2 + email.getAttachments().size(), att);
		}

		EMailSender sender = new EMailSender(documents.iterator().next().getTenantId());
		if (StringUtils.isNotEmpty(from))
			sender.setSender(from);
		sender.send(email);
	}

	/**
	 * Sends a document by email to a recipient
	 * 
	 * @param document the document to send
	 * @param from the email address to be used as From
	 * @param to email addresses of the recipient
	 * @param subject subject of the email
	 * @param message message printed in the body of the email
	 * 
	 * @throws IOException I/O error
	 * @throws MessagingException Cannot send the email
	 */
	public void sendDocument(Document document, String from, String to, String subject, String message)
			throws IOException, MessagingException {
		sendDocuments(Arrays.asList(document), from, Arrays.asList(to), subject, message);
	}

	/**
	 * Sends a document by email to a selection of recipients
	 * 
	 * @param document the document to send
	 * @param from the email address to be used as From
	 * @param to collection of email addresses to send the email to
	 * @param subject subject of the email
	 * @param message message printed in the body of the email
	 * 
	 * @throws IOException I/O error
	 * @throws MessagingException Cannot send the email
	 */
	public void sendDocument(Document document, String from, Collection<String> to, String subject, String message)
			throws IOException, MessagingException {
		sendDocuments(Arrays.asList(document), from, to, subject, message);
	}

	/**
	 * Sends a simple email to a selection of recipients
	 * 
	 * @param tenantId identifier of the tenant
	 * @param from the email address to be used as From
	 * @param to collection of email addresses to send the email to
	 * @param subject subject of the email
	 * @param message message printed in the body of the email
	 * 
	 * @throws MessagingException Cannot send the message
	 */
	public void sendMessage(long tenantId, String from, Collection<String> to, String subject, String message)
			throws MessagingException {
		EMail email = new EMail();
		email.setHtml(1);
		email.setTenantId(tenantId);

		email.setAccountId(-1);
		email.setAuthor("_workflow");
		if (StringUtils.isNotEmpty(from)) {
			email.setAuthorAddress(from);
			Recipient rec = new Recipient();
			rec.setAddress(from);
			rec.setType(Recipient.TYPE_EMAIL);
			email.setFrom(rec);
		}

		if (to != null && !to.isEmpty()) {
			email.parseRecipients(String.join(",", to));
		}

		email.setSubject(subject);
		email.setMessageText(message);

		EMailSender sender = new EMailSender(tenantId);
		if (StringUtils.isNotEmpty(from))
			sender.setSender(from);
		sender.send(email);
	}

	/**
	 * Sends a simple email to a recipient
	 * 
	 * @param tenantId identifier of the tenant
	 * @param from the email address to be used as From
	 * @param to email address of the recipient
	 * @param subject subject of the email
	 * @param message message printed in the body of the email
	 * 
	 * @throws MessagingException Cannot send the email
	 */
	public void sendMessage(long tenantId, String from, String to, String subject, String message)
			throws MessagingException {
		this.sendMessage(tenantId, from, to != null ? Arrays.asList(to) : null, subject, message);
	}

	/**
	 * Creates an {@link EMail} object given the document that stores an email
	 * message.
	 * 
	 * @param document the document that contains the email(must be a .eml or a
	 *        .msg)
	 * @param extractAttachments if the attachments binaries have to be
	 *        extracted
	 * 
	 * @return The object representation of the email
	 * 
	 * @throws IOException I/O error getting the document's contents
	 * @throws MessagingException Cannot read the source message
	 */
	public EMail documentToEMail(Document document, boolean extractAttachments) throws MessagingException, IOException {
		if (!(document.getFileName().toLowerCase().endsWith(".eml")
				|| document.getFileName().toLowerCase().endsWith(".msg")))
			throw new IllegalArgumentException("Filename must end with .msg or .eml");

		EMail email = null;
		Store store = Context.get(Store.class);
		try (InputStream stream = store.getStream(document.getId(), store.getResourceName(document, null, null))) {
			if (document.getFileName().toLowerCase().endsWith(".eml"))
				email = MailUtil.messageToMail(stream, extractAttachments);
			else

				email = MailUtil.msgToMail(stream, extractAttachments);
		} catch (CMSException e) {
			throw new MessagingException(e.getMessage(), e);
		}
		return email;
	}

	/**
	 * Sends a system message to a user
	 * 
	 * @param recipient username of the recipient
	 * @param message body of the message
	 * @param subject subject of the message
	 * @param scope number of days the message will stay active before expiring
	 * @param priority a priority: <b>0</b> = low, <b>1</b> = medium, <b>2</b> =
	 *        high
	 * 
	 * @throws PersistenceException Error in the persistence layer
	 */
	public void sendSystemMessage(String recipient, String message, String subject, int scope, int priority)
			throws PersistenceException {
		UserDAO uDao = Context.get(UserDAO.class);
		User user = uDao.findByUsername(recipient);

		SystemMessage m = new SystemMessage();
		m.setTenantId(user.getTenantId());
		m.setAuthor(user.getUsername());
		m.setSentDate(new Date());
		m.setStatus(SystemMessage.STATUS_NEW);
		m.setType(Message.TYPE_SYSTEM);
		m.setLastNotified(new Date());
		m.setMessageText(message);
		m.setSubject(subject);
		Recipient rec = new Recipient();
		rec.setName(user.getUsername());
		rec.setAddress(user.getUsername());
		rec.setType(Recipient.TYPE_SYSTEM);
		rec.setMode("message");
		Set<Recipient> recipients = new HashSet<>();
		recipients.add(rec);
		m.setRecipients(recipients);
		m.setDateScope(scope);
		m.setPrio(priority);

		SystemMessageDAO dao = Context.get(SystemMessageDAO.class);
		dao.store(m);
	}

	/**
	 * Extracts attachments of email files (.eml, .msg) in the current folder
	 * 
	 * @param doc the document
	 * @param filterFileName a filter on the extensions of the attachment to be
	 *        extracted (comma separated)
	 * @param username the user that will be impersonated to write the
	 *        attachments
	 * @return a list with the new documents created
	 */
	public List<Document> extractAttachments(Document doc, String filterFileName, String username) {

		List<Document> createdDocs = new ArrayList<>();

		User user = new SecurityTool().getUser(username);
		InputStream is = null;
		try {
			long docId = doc.getId();
			Store store = Context.get(Store.class);
			String resource = store.getResourceName(docId, doc.getFileVersion(), null);
			is = store.getStream(docId, resource);

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
							tmpFile = FileUtil.createTempFile("att-", null);
							FileUtils.writeByteArrayToFile(tmpFile, att.getData());

							Document docVO = new Document();
							docVO.setFileName(att.getFileName());
							docVO.setTenantId(doc.getTenantId());
							docVO.setFolder(doc.getFolder());
							docVO.setLanguage(doc.getLanguage());

							DocumentHistory transaction = new DocumentHistory();
							transaction.setUser(user);

							DocumentManager manager = Context.get(DocumentManager.class);
							Document attDoc = manager.create(tmpFile, docVO, transaction).getDocument();
							createdDocs.add(attDoc);
						} finally {
							if (tmpFile != null)
								FileUtil.delete(tmpFile);
						}
					}
				}
			}

		} catch (Exception t) {
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