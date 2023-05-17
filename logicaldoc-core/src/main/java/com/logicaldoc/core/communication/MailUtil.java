package com.logicaldoc.core.communication;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.text.ParseException;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.mail.Address;
import javax.mail.BodyPart;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Multipart;
import javax.mail.Part;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.URLName;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MailDateFormat;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;
import javax.mail.internet.MimeUtility;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.bouncycastle.cms.CMSException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.auxilii.msgparser.MsgParser;
import com.auxilii.msgparser.RecipientEntry;
import com.auxilii.msgparser.attachment.Attachment;
import com.auxilii.msgparser.attachment.FileAttachment;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.P7M;

import net.markenwerk.utils.mail.smime.SmimeUtil;

/**
 * Utility methods for handling emails
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class MailUtil {
	private static final String MULTIPART_STAR = "multipart/*";

	private static final String IPM_NOTE_SMIME = "IPM.Note.SMIME";

	protected static Logger log = LoggerFactory.getLogger(MailUtil.class);

	public static final String NO_SUBJECT = "(Message without subject)";

	public static final String NO_BODY = "(Message without body)";

	public static boolean emlContainsAttachments(InputStream is) {
		try {
			MimeMessage msg = readMime(is);
			if (msg != null && msg.isMimeType(MULTIPART_STAR)) {
				Multipart mp = (Multipart) msg.getContent();
				int count = mp.getCount();
				return count > 1;
			}
		} catch (Throwable t) {
			log.warn(t.getMessage(), t);
		}
		return false;
	}

	public static int countEmlAttachments(InputStream is) {
		try {
			EMail email = messageToMail(is, true);
			return email.getAttachmentsCount();
		} catch (Throwable t) {
			log.warn(t.getMessage(), t);
		}
		return 0;
	}

	/**
	 * Builds an EMail from a .msg file
	 * 
	 * @param is the input stream
	 * @param extractAttachmentContent if the attachments have to be extracted
	 * 
	 * @return the EMail object
	 * 
	 * @throws IOException I/O error
	 * @throws MessagingException error reading the contents
	 * @throws CMSException error reading the contents
	 */
	public static EMail msgToMail(InputStream is, boolean extractAttachmentContent)
			throws IOException, MessagingException, CMSException {

		EMail email = new EMail();
		MsgParser msgp = new MsgParser();
		com.auxilii.msgparser.Message msg = msgp.parseMsg(is);

		String messageClass = msg.getMessageClass();
		if (messageClass.toLowerCase().contains(IPM_NOTE_SMIME.toLowerCase())) {
			FileAttachment p7mAttachment = (FileAttachment) msg.getAttachments().get(0);
			byte[] p7mBytes = p7mAttachment.getData();
			File tmp = FileUtil.createTempFile("msg", null);
			try {
				P7M p7m = new P7M(p7mBytes);
				p7m.extractOriginalFile(tmp);
				email = messageToMail(tmp, extractAttachmentContent);
				email.setSigned(1);
				copyMetadata(email, msg);

				return email;
			} finally {
				FileUtils.deleteQuietly(tmp);
			}
		}

		copyMetadata(email, msg);

		if (StringUtils.isNotEmpty(msg.getBodyHTML()) && msg.getBodyHTML().toLowerCase().startsWith("<html")) {
			email.setMessageText(msg.getBodyHTML());
			email.setHtml(1);
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

	private static void copyMetadata(EMail email, com.auxilii.msgparser.Message msg) {

		email.setSubject(msg.getSubject() == null || msg.getSubject().isEmpty() ? NO_SUBJECT : msg.getSubject());

		// Note: with com.auxilii.msgparser.Message you can't get sentDate and
		// receivedDate, they are the same
		email.setSentDate(msg.getDate());
		email.setReceivedDate(email.getSentDate());

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
	}

	/**
	 * Builds an EMail from a .msg file.
	 * 
	 * @param msgFile the input file
	 * @param extractAttachmentContent if you want to extract the attachment
	 *        files
	 * 
	 * @return the EMail object
	 * 
	 * @throws IOException I/O error
	 * @throws FileNotFoundException cannot open the file
	 * @throws CMSException cannot read the content
	 * @throws MessagingException cannot read the content
	 */
	public static EMail msgToMail(File msgFile, boolean extractAttachmentContent)
			throws FileNotFoundException, IOException, MessagingException, CMSException {
		try (InputStream is = new FileInputStream(msgFile)) {
			return msgToMail(is, extractAttachmentContent);
		}
	}

	/**
	 * Builds an EMail from a stream
	 * 
	 * @param is the stream to read
	 * @param extractAttachmentContent if you want to extract the attachment
	 *        files
	 * 
	 * @return the EMail object
	 * 
	 * @throws MessagingException raised if the message cannot be read
	 * @throws IOException raised if case of I/O errors
	 */
	public static EMail messageToMail(InputStream is, boolean extractAttachmentContent)
			throws MessagingException, IOException {
		MimeMessage msg = readMime(is);
		return messageToMail(msg, extractAttachmentContent);
	}

	/**
	 * Constructs a MimeMessage from a stream
	 * 
	 * @param is the input stream
	 * 
	 * @return the creaed message
	 * 
	 * @throws MessagingException the contents are invalid
	 */
	public static MimeMessage readMime(InputStream is) throws MessagingException {
		Properties props = System.getProperties();
		props.put("mail.transport.protocol", "smtp");
		props.put("mail.smtp.provider.class", CustomTransport.class.getName());
		props.put("mail.smtp.provider.vendor", "foo");
		props.put("mail.smtp.provider.version", "0.0.0");
		// props.put("mail.host", "smtp.unexisting.com");

		try {
			Session mailSession = Session.getInstance(props, null);
			MimeMessage msg = new MimeMessage(mailSession, is);
			return msg;
		} catch (Throwable t) {
			log.warn(t.getMessage());
		}
		return null;
	}

	/**
	 * Builds an EMail from a .eml file.
	 * 
	 * @param emlFile the input file
	 * @param extractAttachmentContent if you want to extract the attachment
	 *        files
	 * 
	 * @return the EMail object
	 * 
	 * @throws MessagingException if the source file cannot be read
	 * @throws IOException raised if case of I/O errors
	 */
	public static EMail messageToMail(File emlFile, boolean extractAttachmentContent)
			throws MessagingException, IOException {
		try (InputStream is = new FileInputStream(emlFile);) {
			return messageToMail(is, extractAttachmentContent);
		}
	}

	/**
	 * Builds an EMail from a Message.
	 * 
	 * @param msg the source message
	 * @param extractAttachmentContent if the binary of the attachments has to
	 *        be extracted
	 * @return The EMail instance
	 * 
	 * @throws MessagingException error reading the message
	 * @throws IOException error in I/O operations
	 */
	public static EMail messageToMail(javax.mail.Message msg, boolean extractAttachmentContent)
			throws MessagingException, IOException {
		EMail email = new EMail();

		setReceivedDate(msg, email);

		email.setSubject(msg.getSubject() == null || msg.getSubject().isEmpty() ? NO_SUBJECT : msg.getSubject());

		setMessageText(msg, email);

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

		setTO(msg, email);

		setCC(msg, email);

		setBCC(msg, email);

		setReplyTo(msg, email);

		if (msg.getContent() instanceof MimeMultipart) {
			MimeMultipart multipart = (MimeMultipart) msg.getContent();
			int count = multipart.getCount();
			for (int i = 1; i < count; i++) {
				BodyPart bp = multipart.getBodyPart(i);
				
				if(bp.getFileName()!=null && bp.getFileName().toLowerCase().contains(".p7s")) {
					MimeBodyPart smimeBody =   SmimeUtil.getSignedContent(multipart);
					addAttachments(smimeBody, email, extractAttachmentContent);
				}else
				addAttachments(bp, email, extractAttachmentContent);
			}
		} else if (NO_BODY.equals(email.getMessageText())) {
			// This is not a multipart message and if there is no text, perhaps
			// the unique part is a file and we treat it as attachment
			addAttachment(msg, email);
		}

		return email;
	}
	
	private static void setReplyTo(javax.mail.Message msg, EMail email) throws MessagingException {
		Address[] addresses = msg.getReplyTo();
		if (addresses != null) {
			try {
				email.setReplyTo(Stream.of(addresses).map(address -> {
					Recipient rec = new Recipient();
					rec.setName(getAddressName(address));
					rec.setAddress(getAddressEmail(address));
					rec.setType(Recipient.TYPE_EMAIL);
					rec.setMode(Recipient.MODE_EMAIL_REPLYTO);
					return rec;
				}).collect(Collectors.toSet()));
			} catch (Throwable t) {
				log.warn("Unable to extract BCC addresses - %s", t.getMessage());
			}
		}
	}

	private static void setBCC(javax.mail.Message msg, EMail email) throws MessagingException {
		Address[] addresses = msg.getRecipients(Message.RecipientType.BCC);
		if (addresses != null) {
			try {
				email.setRecipientsBCC(Stream.of(addresses).map(address -> {
					Recipient rec = new Recipient();
					rec.setName(getAddressName(address));
					rec.setAddress(getAddressEmail(address));
					rec.setType(Recipient.TYPE_EMAIL);
					rec.setMode(Recipient.MODE_EMAIL_BCC);
					return rec;
				}).collect(Collectors.toSet()));
			} catch (Throwable t) {
				log.warn("Unable to extract BCC addresses - %s", t.getMessage());
			}
		}
	}

	private static void setCC(javax.mail.Message msg, EMail email) throws MessagingException {
		Address[] addresses = msg.getRecipients(Message.RecipientType.CC);
		if (addresses != null) {
			try {
				email.setRecipientsCC(Stream.of(addresses).map(address -> {
					Recipient rec = new Recipient();
					rec.setName(getAddressName(address));
					rec.setAddress(getAddressEmail(address));
					rec.setType(Recipient.TYPE_EMAIL);
					rec.setMode(Recipient.MODE_EMAIL_CC);
					return rec;
				}).collect(Collectors.toSet()));
			} catch (Throwable t) {
				log.warn("Unable to extract CC addresses - %s", t.getMessage());
			}
		}
	}

	private static void setTO(javax.mail.Message msg, EMail email) throws MessagingException {
		Address[] addresses = msg.getRecipients(Message.RecipientType.TO);
		if (addresses != null) {
			try {
				email.setRecipients(Stream.of(addresses).map(address -> {
					Recipient rec = new Recipient();
					rec.setName(getAddressName(address));
					rec.setAddress(getAddressEmail(address));
					rec.setType(Recipient.TYPE_EMAIL);
					rec.setMode(Recipient.MODE_EMAIL_TO);
					return rec;
				}).collect(Collectors.toSet()));
			} catch (Throwable t) {
				log.warn("Unable to extract TO addresses - %s", t.getMessage());
			}
		}
	}

	private static void setMessageText(javax.mail.Message msg, EMail email) throws MessagingException, IOException {
		String body = getText(msg);
		email.setHtml(body.charAt(0) == 'H' ? 1 : 0);
		email.setMessageText(body.substring(1));
	}

	private static void setReceivedDate(javax.mail.Message msg, EMail email) throws MessagingException {
		Date receivedDate = msg.getReceivedDate();
		if (receivedDate == null) {
			String[] date = msg.getHeader("Delivery-Date");
			if (date != null && StringUtils.isNotEmpty(date[0]))
				try {
					receivedDate = new MailDateFormat().parse(date[0]);
				} catch (ParseException e) {
					// Nothing to do
				}
		}
		if (receivedDate == null) {
			String[] date = msg.getHeader("Deferred-Delivery");
			if (date != null && StringUtils.isNotEmpty(date[0]))
				try {
					receivedDate = new MailDateFormat().parse(date[0]);
				} catch (ParseException e) {
					// Nothing to do
				}
		}

		email.setReceivedDate(receivedDate);

		Calendar sentDate = Calendar.getInstance();

		// Can be void
		if (msg.getSentDate() != null) {
			sentDate.setTime(msg.getSentDate());
		}
		email.setSentDate(sentDate.getTime());
	}

	private static void addAttachments(BodyPart p, EMail email, boolean extractAttachmentContent)
			throws UnsupportedEncodingException, MessagingException, IOException {
		if (p.getContent() instanceof Multipart) {
			Multipart mp = (Multipart) p.getContent();
			int count = mp.getCount();

			for (int i = 0; i < count; i++) {
				BodyPart bp = mp.getBodyPart(i);
				if (bp.getFileName() != null && extractAttachmentContent) {
					addAttachment(bp, email);
				} else if (p.getContent() instanceof Multipart) {
					addAttachments(bp, email, extractAttachmentContent);
				}
			}
		} else if (extractAttachmentContent) {
			addAttachment(p, email);
		}
	}

	private static void addAttachment(Part part, EMail email) throws UnsupportedEncodingException, MessagingException {
		String fileName = part.getFileName();
		if (part.getContentType().equalsIgnoreCase("message/rfc822")) {
			// The part is another email (may happen in case of forwards).
			try (InputStream is = part.getInputStream()) {
				EMail embeddedEmail = messageToMail(part.getInputStream(), false);
				fileName = embeddedEmail.getSubject() + ".eml";
			} catch (Throwable t) {
				log.warn(t.getMessage(), t);
			}
		}

		// Skip part without a filename
		if (StringUtils.isEmpty(fileName))
			return;

		fileName = MimeUtility.decodeText(fileName);
		fileName = FileUtil.getName(fileName);

		EMailAttachment attachment = new EMailAttachment();
		attachment.setFileName(fileName);
		attachment.setMimeType(part.getContentType());
		attachment.setSize(part.getSize());

		try (InputStream is = part.getInputStream()) {
			byte[] bytes = IOUtils.toByteArray(is);
			attachment.setData(bytes);
			attachment.setSize(bytes.length);
		} catch (IOException e) {
			log.error(e.getMessage(), e);
		}
		email.addAttachment(attachment);
	}

	/**
	 * Get text from message
	 * 
	 * @param p the MIME part
	 * 
	 * @return the part's text
	 * 
	 * @throws MessagingException error reading the message
	 * @throws IOException error in I/O operations
	 */
	private static String getText(Part p) throws MessagingException, IOException {
		if (p.isMimeType("text/*")) {
			return extractTextFromTextStar(p);
		} else if (p.isMimeType("multipart/alternative")) {
			return extractTextFromMultipartAlernative(p);
		} else if (p.isMimeType(MULTIPART_STAR)) {
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
	 * Extracts the text from a part of type text/*
	 * 
	 * @param p The part
	 * @return The extracted text
	 * 
	 * @throws IOException error
	 * @throws MessagingException error
	 */
	private static String extractTextFromTextStar(Part p) throws IOException, MessagingException {
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
		} else {
			// Let's set as text/plain
			return "T" + str;
		}
	}

	/**
	 * Extracts the text from a part of type multipart/alternative
	 * 
	 * @param p The part
	 * @return The extracted text
	 * 
	 * @throws IOException error
	 * @throws MessagingException error
	 */
	private static String extractTextFromMultipartAlernative(Part p) throws IOException, MessagingException {
		// prefer html over plain text
		Multipart mp = (Multipart) p.getContent();
		String text = "T" + NO_BODY;
		// log.info("Mime Parts: {}", mp.getCount());

		for (int i = 0; i < mp.getCount(); i++) {
			Part bp = mp.getBodyPart(i);
			text = getText(bp);
			if (bp.isMimeType("text/html"))
				break;
		}
		return text;
	}

	/**
	 * Extracts the address name
	 * 
	 * @param a the address
	 * 
	 * @return the name
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
	 * 
	 * @param a the address
	 * @return the email
	 */
	public static String getAddressEmail(Address a) {
		if (a != null) {
			InternetAddress ia = (InternetAddress) a;
			return ia.getAddress();
		} else {
			return "";
		}
	}

	public static int countMsgAttachments(File msgFile) {
		try (InputStream is = new FileInputStream(msgFile);) {
			return countMsgAttachments(is);
		} catch (Throwable t) {
			log.warn(t.getMessage(), t);
			return 0;
		}
	}

	public static boolean msgContainsAttachments(File msgFile) {
		try (InputStream is = new FileInputStream(msgFile);) {
			return msgContainsAttachments(is);
		} catch (Throwable t) {
			log.warn(t.getMessage(), t);
			return false;
		}
	}

	public static int countMsgAttachments(InputStream is) {
		MsgParser msgp = new MsgParser();

		try {
			com.auxilii.msgparser.Message msg = msgp.parseMsg(is);
			String messageClass = msg.getMessageClass();
			if (messageClass.toLowerCase().contains(IPM_NOTE_SMIME.toLowerCase())) {
				FileAttachment p7mAttachment = (FileAttachment) msg.getAttachments().get(0);
				byte[] p7mBytes = p7mAttachment.getData();
				File tmp = FileUtil.createTempFile("msg", null);
				try (FileInputStream fis = new FileInputStream(tmp);) {
					P7M p7m = new P7M(p7mBytes);
					p7m.extractOriginalFile(tmp);
					return countEmlAttachments(fis);
				} finally {
					FileUtils.deleteQuietly(tmp);
				}
			}

			return msg.getAttachments() != null ? msg.getAttachments().size() : 0;
		} catch (Throwable t) {
			log.warn(t.getMessage(), t);
		}

		return 0;
	}

	public static boolean msgContainsAttachments(InputStream is) {
		MsgParser msgp = new MsgParser();

		try {
			com.auxilii.msgparser.Message msg = msgp.parseMsg(is);
			String messageClass = msg.getMessageClass();
			if (messageClass.toLowerCase().contains(IPM_NOTE_SMIME.toLowerCase())) {
				FileAttachment p7mAttachment = (FileAttachment) msg.getAttachments().get(0);
				byte[] p7mBytes = p7mAttachment.getData();
				File tmp = FileUtil.createTempFile("msg", null);
				try (FileInputStream fis = new FileInputStream(tmp);) {
					P7M p7m = new P7M(p7mBytes);
					p7m.extractOriginalFile(tmp);
					return emlContainsAttachments(fis);
				} finally {
					FileUtils.deleteQuietly(tmp);
				}
			}

			return msg.getAttachments() != null && !msg.getAttachments().isEmpty();
		} catch (Throwable t) {
			log.warn(t.getMessage(), t);
		}
		return false;
	}

	public static int countEmlAttachments(File emlFile) {
		try (InputStream is = new FileInputStream(emlFile)) {
			return countEmlAttachments(is);
		} catch (Throwable t) {
			log.warn(t.getMessage(), t);
			return 0;
		}
	}

	public static boolean emlContainsAttachments(File emlFile) {
		try (InputStream is = new FileInputStream(emlFile)) {
			return emlContainsAttachments(is);
		} catch (Throwable t) {
			log.warn(t.getMessage(), t);
			return false;
		}
	}

	/**
	 * Extracts just the body of texts parts in the given email message
	 * 
	 * @param message the email message
	 * 
	 * @throws MessagingException Error in traversing the part
	 * @throws IOException generic I/O error
	 * 
	 * @return the message text
	 */
	public static String extractMessageText(Message message) throws MessagingException, IOException {
		StringBuilder messageText = new StringBuilder();
		extractPartText(message.getContent(), messageText);
		return messageText.toString();
	}

	/**
	 * Extracts just the plain text body of the given part and all it's
	 * sub-parts
	 * 
	 * @param content The part to elaborate
	 * @param textBody The string builder that will receive the text
	 * 
	 * @throws MessagingException Error in traversing the part
	 * @throws IOException generic I/O error
	 */
	private static void extractPartText(Object content, StringBuilder textBody) throws MessagingException, IOException {
		if (content instanceof String) {
			textBody.append("\n" + content.toString());
			return;
		}

		if (content instanceof Part) {
			Part part = (Part) content;
			String disposition = part.getDisposition();
			String contentType = part.getContentType();

			if ((disposition == null || "inline".equals(disposition)) && contentType != null) {
				if (contentType.toLowerCase().startsWith("text/plain")) {
					textBody.append(part.getContent());
				}
			}
			return;
		}

		if (content instanceof Multipart) {
			extractPartTextFromMultipart((Multipart) content, textBody);
		}
	}

	private static void extractPartTextFromMultipart(Multipart multipart, StringBuilder textBody)
			throws MessagingException {
		for (int i = 0, n = multipart.getCount(); i < n; i++) {
			try {
				BodyPart part = multipart.getBodyPart(i);

				Object cont = part.getContent();
				if (cont instanceof Multipart) {
					extractPartText(cont, textBody);
				} else {
					extractPartText(part, textBody);
				}
			} catch (Throwable e) {
				log.warn(e.getMessage(), e);
			}
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
			// Nothing to do
		}

		@Override
		public synchronized void connect(String host, int port, String username, String password)
				throws MessagingException {
			// Nothing to do
		}

		@Override
		public void connect(String host, String username, String password) throws MessagingException {
			// Nothing to do
		}

		@Override
		public synchronized void close() {
			// Nothing to do
		}
	}
}