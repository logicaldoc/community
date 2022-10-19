package com.logicaldoc.core.communication;

import java.io.File;
import java.io.FileOutputStream;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.TimeZone;
import java.util.stream.Collectors;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.activation.URLDataSource;
import javax.mail.Address;
import javax.mail.Authenticator;
import javax.mail.Multipart;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MailDateFormat;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;
import javax.mail.internet.MimeUtility;
import javax.mail.util.ByteArrayDataSource;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.automation.Automation;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.threading.ThreadPools;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;

import net.sf.jmimemagic.Magic;
import net.sf.jmimemagic.MagicMatch;

/**
 * SMTP E-Mail sender service
 * 
 * @author Michael Scholz
 * @author Matteo Caruso - LogicalDOC
 */
public class EMailSender {

	private static final String THREAD_POOL = "Email";

	private static Logger log = LoggerFactory.getLogger(EMailSender.class);

	public static final int SECURITY_NONE = 0;

	public static final int SECURITY_STARTTLS = 1;

	public static final int SECURITY_TLS = 2;

	public static final int SECURITY_SSL = 3;

	private String host = "localhost";

	private String sender = "logicaldoc@acme.com";

	private String username = "";

	private String password = "";

	private int port = 25;

	private boolean authEncrypted = false;

	private int connectionSecurity = SECURITY_NONE;

	public static int FOLDERING_NONE = 0;

	public static int FOLDERING_YEAR = 1;

	public static int FOLDERING_MONTH = 2;

	public static int FOLDERING_DAY = 3;

	public int foldering = FOLDERING_DAY;

	public Long folderId;

	public EMailSender(long tenant) {
		TenantDAO tenantDao = (TenantDAO) Context.get().getBean(TenantDAO.class);
		try {
			loadSettings(tenantDao.findById(tenant).getName());
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}

	public EMailSender(String tenant) {
		loadSettings(tenant);
	}

	private void loadSettings(String tenant) {
		try {
			ContextProperties config = Context.get().getProperties();

			host = config.getProperty(tenant + ".smtp.host");
			port = config.getInt(tenant + ".smtp.port");
			username = config.getProperty(tenant + ".smtp.username");
			password = config.getProperty(tenant + ".smtp.password");
			sender = config.getProperty(tenant + ".smtp.sender");
			authEncrypted = "true".equals(config.getProperty(tenant + ".smtp.authEncrypted"));
			connectionSecurity = config.getInt(tenant + ".smtp.connectionSecurity");
			folderId = config.getLong(tenant + ".smtp.save.folderId", 0);
			foldering = config.getInt(tenant + ".smtp.save.foldering", FOLDERING_DAY);
		} catch (Throwable t) {
			log.warn(t.getMessage(), t);
		}
	}

	public EMailSender() {
	}

	public String getSender() {
		return sender;
	}

	public void setSender(String sender) {
		this.sender = sender;
	}

	public String getHost() {
		return host;
	}

	public void setHost(String host) {
		this.host = host;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public int getPort() {
		return port;
	}

	public void setPort(int port) {
		this.port = port;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	/**
	 * Same as send(EMail, String, Map) but executes in another thread
	 * 
	 * @param email the email to send
	 * @param templateName the template to use to render the body of the message
	 *        using the automation engine
	 * @param dictionary map of variable to pass to the automation
	 */
	public void sendAsync(EMail email, String templateName, Map<String, Object> dictionary) {
		ThreadPools tPools = (ThreadPools) Context.get().getBean(ThreadPools.class);
		tPools.execute(() -> {
			try {
				send(email, templateName, dictionary);
			} catch (Throwable e) {
				log.error(e.getMessage(), e);
			}
		}, THREAD_POOL);
	}

	/**
	 * Sends an email by using a given template
	 * 
	 * @param email The email to send
	 * @param templateName Name of the template to be applied
	 * @param dictionary The dictionary to be used in the template
	 * 
	 * @throws Exception raised if the message cannot be sent
	 */
	public void send(EMail email, String templateName, Map<String, Object> dictionary) throws Exception {
		MessageTemplateDAO templateDao = (MessageTemplateDAO) Context.get().getBean(MessageTemplateDAO.class);
		MessageTemplate template = templateDao.findByNameAndLanguage(templateName, email.getLocale().toString(),
				email.getTenantId());
		if (template == null) {
			log.warn("Template {} was not found", templateName);
			return;
		}

		dictionary.put(Automation.LOCALE, email.getLocale());
		email.setSubject(template.getFormattedSubject(dictionary));
		email.setMessageText(template.getFormattedBody(dictionary));

		send(email);
	}

	/**
	 * Same as send(EMail) but executes in another thread
	 * 
	 * @param email the email to send
	 */
	public void sendAsync(EMail email) {
		ThreadPools tPools = (ThreadPools) Context.get().getBean(ThreadPools.class);
		tPools.execute(() -> {
			try {
				send(email);
			} catch (Throwable e) {
				log.error(e.getMessage(), e);
			}
		}, THREAD_POOL);
	}

	/**
	 * This method sends an email using the smtp-protocol. The email can be a
	 * simple mail or a multipart mail containing up to 5 attachments.
	 * 
	 * @param email E-Mail which should be sent.
	 * 
	 * @throws Exception raised if the email cannot be sent
	 */
	public void send(EMail email) throws Exception {
		try {
			TenantDAO tDao = (TenantDAO) Context.get().getBean(TenantDAO.class);
			String tenantName = tDao.getTenantName(email.getTenantId());
			if (!Context.get().getProperties().getBoolean(tenantName + ".smtp.userasfrom", false))
				email.setAuthorAddress(null);
		} catch (Throwable t) {
			log.warn(t.getMessage(), t);
		}

		Properties props = new Properties();
		if (!StringUtils.isEmpty(username))
			props.put("mail.smtp.auth", "true");

		if (authEncrypted) {
			// The 'smtps' protocol must be used
			props.put("mail.transport.protocol", "smtps");
			props.put("mail.smtps.host", host);
			props.put("mail.smtps.port", port);
			props.put("mail.smtps.ssl.protocols", "TLSv1.1 TLSv1.2");
			if (connectionSecurity == SECURITY_STARTTLS)
				props.put("mail.smtps.starttls.enable", "true");
			if (connectionSecurity == SECURITY_TLS)
				props.put("mail.smtps.starttls.required", "true");
			if (connectionSecurity == SECURITY_SSL) {
				// Necessary property to send e-mails with SSL
				props.put("mail.smtps.ssl.enable", "true");
			}
		} else {
			props.put("mail.transport.protocol", "smtp");
			props.put("mail.smtp.host", host);
			props.put("mail.smtp.port", port);
			if (connectionSecurity == SECURITY_STARTTLS)
				props.put("mail.smtp.starttls.enable", "true");
			if (connectionSecurity == SECURITY_TLS)
				props.put("mail.smtp.starttls.required", "true");
			if (connectionSecurity == SECURITY_SSL) {
				// Necessary property to send e-mails with SSL
				props.put("mail.smtp.ssl.enable", "true");
			}
		}

		props.put("mail.smtp.ssl.protocols", "TLSv1.1 TLSv1.2");
		props.put("mail.smtp.ssl.checkserveridentity", "false");
		props.put("mail.smtp.ssl.trust", "*");
		// props.put("mail.debug", "true");

		Session sess = null;

		try {
			if (!StringUtils.isEmpty(username))
				sess = Session.getInstance(props, new Authenticator() {
					protected PasswordAuthentication getPasswordAuthentication() {
						return new PasswordAuthentication(username, password);
					}
				});
			else
				sess = Session.getInstance(props);
		} catch (SecurityException e) {
			if (!StringUtils.isEmpty(username))
				sess = Session.getInstance(props, new Authenticator() {
					protected PasswordAuthentication getPasswordAuthentication() {
						return new PasswordAuthentication(username, password);
					}
				});
			else
				sess = Session.getInstance(props);
		}

		MimeMessage message = new MimeMessage(sess);

		// The FROM field must to be the one configured for the SMTP connection.
		// because of errors will be returned in the case the sender is not in
		// the SMTP domain.
		InternetAddress from = new InternetAddress(sender);
		if (StringUtils.isNotEmpty(email.getAuthorAddress()))
			try {
				from = new InternetAddress(email.getAuthorAddress());
			} catch (Throwable t) {
				// Nothing to do
			}
		InternetAddress[] to = email.getAddresses();
		InternetAddress[] cc = email.getAddressesCC();
		InternetAddress[] bcc = email.getAddressesBCC();
		message.setFrom(from);
		message.setRecipients(javax.mail.Message.RecipientType.TO, to);
		if (cc.length > 0)
			message.setRecipients(javax.mail.Message.RecipientType.CC, cc);
		if (bcc.length > 0)
			message.setRecipients(javax.mail.Message.RecipientType.BCC, bcc);
		message.setSubject(email.getSubject(), "UTF-8");

		MimeBodyPart body = new MimeBodyPart();
		if (email.isHtml()) {
			body.setContent(new String(email.getMessageText().getBytes("UTF-8"), "UTF-8"),
					"text/html; charset=UTF-8; fileNameCharset=UTF-8");
		} else {
			body.setText(email.getMessageText(), "UTF-8");
		}

		/*
		 * If we have to images, the parts must be 'related' otherwise 'mixed'
		 */
		Multipart mpMessage = new MimeMultipart(email.getImages().isEmpty() ? "mixed" : "related");
		mpMessage.addBodyPart(body);

		int i = 1;
		for (String image : email.getImages()) {
			MimeBodyPart imageBodyPart = new MimeBodyPart();
			DataSource ds = new URLDataSource(new URL(image));
			imageBodyPart.setDataHandler(new DataHandler(ds));
			imageBodyPart.setHeader("Content-ID", "<image_" + (i++) + ">");
			imageBodyPart.setDisposition("inline");
			mpMessage.addBodyPart(imageBodyPart);
		}

		for (Integer partId : email.getAttachments().keySet()) {
			EMailAttachment att = email.getAttachment(partId);
			if (att != null) {
				String mime = "text/plain";
				try {
					MagicMatch match = Magic.getMagicMatch(att.getData(), true);
					mime = match.getMimeType();
				} catch (Throwable t) {
					// Nothing to do
				}
				DataSource fdSource = new ByteArrayDataSource(att.getData(), mime);
				DataHandler fdHandler = new DataHandler(fdSource);
				MimeBodyPart part = new MimeBodyPart();
				part.setDataHandler(fdHandler);
				String fileName = MimeUtility.encodeText(att.getFileName(), "UTF-8", null);
				part.setFileName(fileName);
				mpMessage.addBodyPart(part);
			}
		}

		message.setContent(mpMessage);

		Transport trans = null;
		if (authEncrypted)
			trans = sess.getTransport("smtps");
		else
			trans = sess.getTransport("smtp");

		if (StringUtils.isEmpty(username)) {
			trans.connect(host, port, null, null);
		} else {
			trans.connect(host, port, username, password);
		}

		Address[] adr = message.getAllRecipients();
		// message.setSentDate(new Date());

		MailDateFormat formatter = new MailDateFormat();
		formatter.setTimeZone(TimeZone.getTimeZone("GMT")); // always use UTC
															// for outgoing mail
		Date now = new Date();
		message.setHeader("Date", formatter.format(now));

		trans.sendMessage(message, adr);
		trans.close();

		log.info("Sent email with subject '{}' to recipients {}", email.getSubject(), email.getAllRecipientsEmails());

		/*
		 * If the case, we save the email as document in LogicalDOC's repository
		 */
		email.setSentDate(now);
		historycizeOutgoingEmail(email, message, from);
	}

	/**
	 * Saved the email as a document in the documents repository
	 * 
	 * @param email The email representation
	 * @param message The mime message sent
	 * @param from email address the email was sent from
	 */
	private void historycizeOutgoingEmail(EMail email, MimeMessage message, InternetAddress from) {
		if (folderId == null || !email.isHistoricyze())
			return;

		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		TemplateDAO templateDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Folder saveFolder=null;
		try {
			saveFolder = folderId != null && folderId != 0 ? folderDao.findFolder(folderId) : null;
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		if (saveFolder == null)
			return;

		File emlFile = null;
		try {
			emlFile = File.createTempFile("emailsender", ".eml");
			
			try(FileOutputStream fos = new FileOutputStream(emlFile);){
				message.writeTo(new FileOutputStream(emlFile));
			}

			Folder folder = saveFolder;
			if (foldering == FOLDERING_YEAR) {
				DateFormat df = new SimpleDateFormat("yyyy");
				folder = folderDao.createPath(saveFolder, df.format(email.getSentDate()), true, null);
			} else if (foldering == FOLDERING_MONTH) {
				DateFormat df = new SimpleDateFormat("yyyy/MM");
				folder = folderDao.createPath(saveFolder, df.format(email.getSentDate()), true, null);
			} else if (foldering == FOLDERING_DAY) {
				DateFormat df = new SimpleDateFormat("yyyy/MM/dd");
				folder = folderDao.createPath(saveFolder, df.format(email.getSentDate()), true, null);
			}

			Document emailDocument = new Document();
			emailDocument.setFileName(email.getSubject() + ".eml");
			emailDocument.setType("eml");
			emailDocument.setLocale(email.getLocale() != null ? email.getLocale() : Locale.ENGLISH);
			emailDocument.setFolder(folder);
			emailDocument.setTenantId(folder.getTenantId());
			emailDocument.setTemplate(templateDao.findByName("email", folder.getTenantId()));
			if (emailDocument.getTemplate() != null) {
				Map<String, Attribute> attributes = new HashMap<String, Attribute>();
				Attribute ext = new Attribute();
				ext.setStringValue(StringUtils.substring(from.getAddress(), 0, 3999));
				attributes.put("from", ext);

				if (email.getAddresses() != null) {
					ext = new Attribute();
					ext.setStringValue(StringUtils.substring(Arrays.asList(email.getAddresses()).stream()
							.map(a -> a.getAddress()).collect(Collectors.joining(", ")), 0, 3999));
					attributes.put("to", ext);
				}

				if (email.getAddressesCC() != null) {
					ext = new Attribute();
					ext.setStringValue(StringUtils.substring(Arrays.asList(email.getAddressesCC()).stream()
							.map(a -> a.getAddress()).collect(Collectors.joining(", ")), 0, 3999));
					attributes.put("cc", ext);
				}

				ext = new Attribute();
				ext.setStringValue(StringUtils.substring(email.getSubject(), 0, 3999));
				attributes.put("subject", ext);

				ext = new Attribute();
				ext.setStringValue(StringUtils.substring(email.getAuthor(), 0, 3999));
				attributes.put("sendername", ext);

				ext = new Attribute();
				DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
				ext.setStringValue(df.format(email.getSentDate()));
				attributes.put("sentdate", ext);

				ext = new Attribute();
				ext.setBooleanValue(email.getAttachmentsCount() > 0);
				attributes.put("attachments", ext);

				emailDocument.setAttributes(attributes);
			}

			DocumentHistory transaction = new DocumentHistory();
			transaction.setComment("saved for history");
			transaction.setUser(userDao.findByUsername("_system"));

			manager.create(emlFile, emailDocument, transaction);
			log.debug("Historycizes the email with subject '{}' sent to {}", email.getSubject(),
					email.getAllRecipientsEmails());
		} catch (Throwable t) {
			log.warn("Cannot historycize the email with subject '{}' sent to {}", email.getSubject(),
					email.getAllRecipientsEmails(), t);
		} finally {
			if (emlFile != null && emlFile.exists())
				FileUtil.strongDelete(emlFile);
		}
	}

	public boolean isAuthEncrypted() {
		return authEncrypted;
	}

	public void setAuthEncrypted(boolean authEncrypted) {
		this.authEncrypted = authEncrypted;
	}

	public int getConnectionSecurity() {
		return connectionSecurity;
	}

	public void setConnectionSecurity(int connectionSecurity) {
		this.connectionSecurity = connectionSecurity;
	}

	public int getFoldering() {
		return foldering;
	}

	public void setFoldering(int foldering) {
		this.foldering = foldering;
	}

	public Long getFolderId() {
		return folderId;
	}

	public void setFolderId(Long folderId) {
		this.folderId = folderId;
	}
}