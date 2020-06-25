package com.logicaldoc.core.automation;

import java.util.Arrays;
import java.util.Collection;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailAttachment;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.communication.MailUtil;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.MimeType;

/**
 * Utility functions to handle emails and send messages from within the
 * Automation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.1
 */
@AutomationDictionary
public class MailTool {

	/**
	 * Sends some documents to a recipient
	 * 
	 * @param documents collection of documents to send
	 * @param from the email address to be used as From
	 * @param to the email address of the recipient
	 * @param subject subject of the email
	 * @param message message printed in the body of the email
	 * 
	 * @throws Exception error sending the email
	 */
	public void sendDocuments(Collection<Document> documents, String from, String to, String subject, String message)
			throws Exception {
		sendDocuments(documents, from, Arrays.asList(new String[] { to }), subject, message);
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
	 * @throws Exception error sending the email
	 */
	public void sendDocuments(Collection<Document> documents, String from, Collection<String> to, String subject,
			String message) throws Exception {
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
			Storer storer = (Storer) Context.get().getBean(Storer.class);
			att.setData(storer.getBytes(document.getId(), storer.getResourceName(document, null, null)));
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
	 * @throws Exception error sending the email
	 */
	public void sendDocument(Document document, String from, String to, String subject, String message)
			throws Exception {
		sendDocuments(Arrays.asList(new Document[] { document }), from, Arrays.asList(new String[] { to }), subject,
				message);
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
	 * @throws Exception error sending the email
	 */
	public void sendDocument(Document document, String from, Collection<String> to, String subject, String message)
			throws Exception {
		sendDocuments(Arrays.asList(new Document[] { document }), from, to, subject, message);
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
	 * @throws Exception error sending the email
	 */
	public void sendMessage(long tenantId, String from, Collection<String> to, String subject, String message)
			throws Exception {
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
	 * @throws Exception error sending the email
	 */
	public void sendMessage(long tenantId, String from, String to, String subject, String message) throws Exception {
		this.sendMessage(tenantId, from, to != null ? Arrays.asList(new String[] { to }) : null, subject, message);
	}

	/**
	 * Creates an {@link EMail} object given the document that stores an email message. 
	 * 
	 * @param document the document that contains the email(must be a .eml or a .msg)
	 * @param extractAttachments if the attachments binaries have to be extracted
	 * 
	 * @return The object representation of the email
	 * 
	 * @throws Exception the email cannot be correctly analyzed
	 */
	public EMail documentToEMail(Document document, boolean extractAttachments) throws Exception {
		assert (document.getFileName().toLowerCase().endsWith(".eml")
				|| document.getFileName().toLowerCase().endsWith(".msg"));
		EMail email = null;
		Storer storer = (Storer) Context.get().getBean(Storer.class);
		if (document.getFileName().toLowerCase().endsWith(".eml"))
			email = MailUtil.messageToMail(
					storer.getStream(document.getId(), storer.getResourceName(document, null, null)),
					extractAttachments);
		else
			email = MailUtil.msgToMail(storer.getStream(document.getId(), storer.getResourceName(document, null, null)),
					extractAttachments);
		return email;
	}
}