package com.logicaldoc.core.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.util.Calendar;
import java.util.List;
import java.util.Properties;

import javax.mail.Address;
import javax.mail.BodyPart;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Multipart;
import javax.mail.Part;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.URLName;
import javax.mail.internet.ContentDisposition;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeUtility;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.auxilii.msgparser.MsgParser;
import com.auxilii.msgparser.RecipientEntry;
import com.auxilii.msgparser.attachment.Attachment;
import com.auxilii.msgparser.attachment.FileAttachment;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailAttachment;
import com.logicaldoc.core.communication.Recipient;

/**
 * Utility methods for handling emails
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class MailUtil {
	protected static Logger log = LoggerFactory.getLogger(MailUtil.class);

	public static final String NO_SUBJECT = "(Message without subject)";

	public static final String NO_BODY = "(Message without body)";

	/**
	 * Builds an EMail from a .msg file.
	 **/
	public static EMail msgToMail(InputStream is, boolean extractAttachmentContent) throws MessagingException,
			IOException {

		EMail email = new EMail();
		MsgParser msgp = new MsgParser();
		com.auxilii.msgparser.Message msg = msgp.parseMsg(is);

		email.setSubject(msg.getSubject() == null || msg.getSubject().isEmpty() ? NO_SUBJECT : msg.getSubject());

		Recipient rec = new Recipient();
		rec.setAddress(msg.getFromEmail());
		rec.setName(msg.getFromName());
		email.setFrom(rec);
		email.setAuthor(msg.getFromName());
		email.setAuthorAddress(msg.getFromEmail());

		rec = new Recipient();
		rec.setAddress(msg.getToEmail());
		rec.setName(msg.getToName());
		rec.setMode(Recipient.MODE_EMAIL_CC);
		rec.setType(Recipient.TYPE_EMAIL);
		email.getRecipients().add(rec);

		if (StringUtils.isNotEmpty(msg.getBodyHTML()) && msg.getBodyHTML().toLowerCase().startsWith("<html")) {
			email.setMessageText(msg.getBodyHTML());
			email.setHtml(1);
		}

		if (StringUtils.isEmpty(email.getMessageText()))
			if (StringUtils.isNotEmpty(msg.getBodyRTF())) {
				email.setMessageText(msg.getBodyRTF());
				email.setHtml(0);
			}

		if (StringUtils.isEmpty(email.getMessageText()))
			if (StringUtils.isNotEmpty(msg.getBodyText())) {
				email.setMessageText(msg.getBodyText());
				email.setHtml(0);
			}

		List<RecipientEntry> recs = msg.getCcRecipients();
		for (RecipientEntry entry : recs) {
			Recipient r = new Recipient();
			r.setAddress(entry.getToEmail());
			r.setName(entry.getToName());
			r.setMode(Recipient.MODE_EMAIL_CC);
			r.setType(Recipient.TYPE_EMAIL);
			email.getRecipientsCC().add(r);
		}

		recs = msg.getBccRecipients();
		for (RecipientEntry entry : recs) {
			Recipient r = new Recipient();
			r.setAddress(entry.getToEmail());
			r.setName(entry.getToName());
			r.setMode(Recipient.MODE_EMAIL_BCC);
			r.setType(Recipient.TYPE_EMAIL);
			email.getRecipientsBCC().add(r);
		}
		List<Attachment> atts = msg.getAttachments();
		for (Attachment att : atts) {
			if (att instanceof FileAttachment) {
				FileAttachment fatt = (FileAttachment) att;
				if (StringUtils.isNotEmpty(fatt.getFilename()) && fatt.getSize() > 0) {
					EMailAttachment emailAtt = new EMailAttachment();
					emailAtt.setFileName(fatt.getFilename());
					emailAtt.setSize(fatt.getSize());
					if (extractAttachmentContent)
						emailAtt.setData(fatt.getData());
					email.addAttachment(emailAtt);
				}
			}
		}

		return email;
	}

	/**
	 * Builds an EMail from a .msg file.
	 **/
	public static EMail msgToMail(File msgFile, boolean extractAttachmentContent) throws MessagingException,
			IOException {

		InputStream is = null;
		try {
			is = new FileInputStream(msgFile);
			return msgToMail(is, extractAttachmentContent);
		} finally {
			IOUtils.closeQuietly(is);
		}
	}

	/**
	 * Builds an EMail from a .eml file.
	 **/
	public static EMail messageToMail(InputStream is, boolean extractAttachmentContent) throws MessagingException,
			IOException {
		Properties props = System.getProperties();
		props.put("mail.transport.protocol", "smtp");
    	props.put("mail.smtp.provider.class", CustomTransport.class.getName());
		props.put("mail.smtp.provider.vendor", "foo");
		props.put("mail.smtp.provider.version", "0.0.0");
//		props.put("mail.host", "smtp.unexisting.com");
		props.put("mail.transport.protocol", "smtp");

		Session mailSession = Session.getDefaultInstance(props, null);
		MimeMessage msg = new MimeMessage(mailSession, is);
		return messageToMail(msg, extractAttachmentContent);
	}

	/**
	 * Builds an EMail from a .eml file.
	 **/
	public static EMail messageToMail(File emlFile, boolean extractAttachmentContent) throws MessagingException,
			IOException {

		InputStream is = null;
		try {
			is = new FileInputStream(emlFile);
			return messageToMail(is, extractAttachmentContent);
		} finally {
			IOUtils.closeQuietly(is);
		}
	}

	/**
	 * Builds an EMail from a Message.
	 * 
	 * @param msg the source message
	 * @param extractAttachmentContent if the binary of the attachments has to
	 *        be extracted
	 * @return The EMail instance
	 * @throws MessagingException
	 * @throws IOException
	 */
	public static EMail messageToMail(javax.mail.Message msg, boolean extractAttachmentContent)
			throws MessagingException, IOException {
		EMail email = new EMail();

		Calendar receivedDate = Calendar.getInstance();

		// Can be void
		if (msg.getReceivedDate() != null) {
			receivedDate.setTime(msg.getReceivedDate());
		}
		email.setReceivedDate(receivedDate.getTime());

		Calendar sentDate = Calendar.getInstance();

		// Can be void
		if (msg.getSentDate() != null) {
			sentDate.setTime(msg.getSentDate());
		}
		email.setSentDate(sentDate.getTime());

		email.setSubject(msg.getSubject() == null || msg.getSubject().isEmpty() ? NO_SUBJECT : msg.getSubject());

		String body = getText(msg);
		if (body.charAt(0) == 'H')
			email.setHtml(1);
		else
			email.setHtml(0);
		email.setMessageText(body.substring(1));

		if (msg.getReplyTo() != null && msg.getReplyTo().length > 0) {
			Recipient rec = new Recipient();
			rec.setName(getAddressName(msg.getReplyTo()[0]));
			rec.setAddress(getAddressEmail(msg.getReplyTo()[0]));
			rec.setType(Recipient.TYPE_EMAIL);
			rec.setMode(Recipient.MODE_EMAIL_TO);
			email.setReplyTo(rec);
		}

		if (msg.getFrom() != null && msg.getFrom().length > 0) {
			Recipient rec = new Recipient();
			rec.setName(getAddressName(msg.getFrom()[0]));
			rec.setAddress(getAddressEmail(msg.getFrom()[0]));
			rec.setType(Recipient.TYPE_EMAIL);
			rec.setMode(Recipient.MODE_EMAIL_TO);
			email.setFrom(rec);
			email.setAuthor(rec.getName());
			email.setAuthorAddress(rec.getAddress());
		}

		try {
			Address[] addresses = msg.getRecipients(Message.RecipientType.TO);
			if (addresses != null)
				for (Address address : addresses) {
					Recipient rec = new Recipient();
					rec.setName(getAddressName(address));
					rec.setAddress(getAddressEmail(address));
					rec.setType(Recipient.TYPE_EMAIL);
					rec.setMode(Recipient.MODE_EMAIL_TO);
					email.getRecipients().add(rec);
				}
		} catch (Throwable t) {
			log.warn("Unable to extract TO addresses - %s", t.getMessage());
		}

		try {
			Address[] addresses = msg.getRecipients(Message.RecipientType.CC);
			if (addresses != null)
				for (Address address : addresses) {
					Recipient rec = new Recipient();
					rec.setName(getAddressName(address));
					rec.setAddress(getAddressEmail(address));
					rec.setType(Recipient.TYPE_EMAIL);
					rec.setMode(Recipient.MODE_EMAIL_CC);
					email.getRecipientsCC().add(rec);
				}
		} catch (Throwable t) {
			log.warn("Unable to extract CC addresses - %s", t.getMessage());
		}

		try {
			Address[] addresses = msg.getRecipients(Message.RecipientType.BCC);
			if (addresses != null)
				for (Address address : addresses) {
					Recipient rec = new Recipient();
					rec.setName(getAddressName(address));
					rec.setAddress(getAddressEmail(address));
					rec.setType(Recipient.TYPE_EMAIL);
					rec.setMode(Recipient.MODE_EMAIL_BCC);
					email.getRecipientsBCC().add(rec);
				}
		} catch (Throwable t) {
			log.warn("Unable to extract BCC addresses - {}", t.getMessage());
		}

		if (msg.isMimeType("multipart/*")) {
			Multipart mp = (Multipart) msg.getContent();
			int count = mp.getCount();
			for (int i = 1; i < count; i++) {
				BodyPart bp = mp.getBodyPart(i);
				addAttachments(bp, email, extractAttachmentContent);
			}
		}

		return email;
	}

	private static void addAttachments(BodyPart p, EMail email, boolean extractAttachmentContent)
			throws UnsupportedEncodingException, MessagingException, IOException {
		if (p.isMimeType("multipart/*")) {
			Multipart mp = (Multipart) p.getContent();
			int count = mp.getCount();
			for (int i = 1; i < count; i++) {
				BodyPart bp = mp.getBodyPart(i);
				if (bp.getFileName() != null) {
					addAttachment(bp, email);
				} else if (bp.isMimeType("multipart/*")) {
					addAttachments(bp, email, extractAttachmentContent);
				}
			}
		} else if (StringUtils.isNotEmpty(p.getFileName())) {
			addAttachment(p, email);
		}
	}

	private static void addAttachment(BodyPart bp, EMail email) throws UnsupportedEncodingException, MessagingException {
		// Skip part without a filename
		if (StringUtils.isEmpty(bp.getFileName()))
			return;

		// Skip part that is not an attachment
		String[] values = bp.getHeader("Content-Disposition");
		String disposition = "";
		if (values != null && values.length > 0)
			disposition = new ContentDisposition(values[0]).getDisposition();
		if (!disposition.contains("attachment"))
			return;

		String name = MimeUtility.decodeText(bp.getFileName());
		String fileName = FilenameUtils.getName(name);

		EMailAttachment attachment = new EMailAttachment();
		attachment.setFileName(fileName);
		attachment.setMimeType(bp.getContentType());
		attachment.setSize(bp.getSize());

		InputStream is = null;
		try {
			is = bp.getInputStream();
			byte[] bytes = IOUtils.toByteArray(is);
			attachment.setData(bytes);
			attachment.setSize(bytes.length);
		} catch (Throwable t) {
			IOUtils.closeQuietly(is);
		}
		email.addAttachment(attachment);
	}

	/**
	 * Get text from message
	 */
	private static String getText(Part p) throws MessagingException, IOException {
		if (p.isMimeType("text/*")) {
			Object obj = p.getContent();
			String str = NO_BODY;

			if (obj instanceof InputStream) {
				InputStream is = (InputStream) obj;
				StringWriter writer = new StringWriter();
				IOUtils.copy(is, writer, "UTF-8");
				str = writer.toString();
			} else {
				str = (String) obj;
			}

			if (p.isMimeType("text/html")) {
				return "H" + str;
			} else if (p.isMimeType("text/plain")) {
				return "T" + str;
			} else {
				// Otherwise let's set as text/plain
				return "T" + str;
			}
		} else if (p.isMimeType("multipart/alternative")) {
			// prefer html over plain text
			Multipart mp = (Multipart) p.getContent();
			String text = "T" + NO_BODY;
			// log.info("Mime Parts: {}", mp.getCount());

			for (int i = 0; i < mp.getCount(); i++) {
				Part bp = mp.getBodyPart(i);

				if (bp.isMimeType("text/plain")) {
					text = getText(bp);
				} else if (bp.isMimeType("text/html")) {
					text = getText(bp);
					break;
				} else {
					text = getText(bp);
				}
			}

			return text;
		} else if (p.isMimeType("multipart/*")) {
			Multipart mp = (Multipart) p.getContent();

			for (int i = 0; i < mp.getCount(); i++) {
				String s = getText(mp.getBodyPart(i));

				if (s != null)
					return s;
			}
		}

		return "T" + NO_BODY;
	}

	/**
	 * Extracts the address name
	 */
	public static String getAddressName(Address a) {
		if (a != null) {
			InternetAddress ia = (InternetAddress) a;

			if (ia.getPersonal() != null) {
				return ia.getPersonal();
			} else {
				return ia.getAddress();
			}
		} else {
			return "";
		}
	}

	/**
	 * Extracts the address email
	 */
	public static String getAddressEmail(Address a) {
		if (a != null) {
			InternetAddress ia = (InternetAddress) a;
			return ia.getAddress();
		} else {
			return "";
		}
	}

	class CustomTransport extends Transport {

		public CustomTransport(Session smtpSession, URLName urlName) {
			super(smtpSession, urlName);
		}

		@Override
		public void sendMessage(Message message, Address[] addresses) throws MessagingException {
			// Take the message and write it somewhere
			// e.g.: a logger or an OutputStream message.writeTo(...);
		}

		@Override
		public void connect() throws MessagingException {
		}

		@Override
		public void connect(String host, int port, String username, String password) throws MessagingException {
		}

		@Override
		public void connect(String host, String username, String password) throws MessagingException {
		}

		@Override
		public void close() {
		}
	}
}