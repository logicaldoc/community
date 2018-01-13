package com.logicaldoc.core.automation;

import java.util.Arrays;
import java.util.Collection;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailAttachment;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.MimeType;

/**
 * Utility functions sending emails
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.1
 */
public class MailTool {

	public void sendDocuments(Collection<Document> documents, String from, String to, String subject, String message)
			throws Exception {
		sendDocuments(documents, from, Arrays.asList(new String[] { to }), subject, message);
	}

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

	public void sendDocument(Document document, String from, String to, String subject, String message)
			throws Exception {
		sendDocuments(Arrays.asList(new Document[] { document }), from, Arrays.asList(new String[] { to }), subject,
				message);
	}

	public void sendDocument(Document document, String from, Collection<String> to, String subject, String message)
			throws Exception {
		sendDocuments(Arrays.asList(new Document[] { document }), from, to, subject, message);
	}

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

	public void sendMessage(long tenantId, String from, String to, String subject, String message) throws Exception {
		this.sendMessage(tenantId, from, to != null ? Arrays.asList(new String[] { to }) : null, subject, message);
	}
}