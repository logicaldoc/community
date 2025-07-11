package com.logicaldoc.core.communication;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Base64;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TimeZone;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.RunLevel;
import com.logicaldoc.core.automation.Automation;
import com.logicaldoc.core.automation.AutomationException;
import com.logicaldoc.core.communication.oauth.Microsoft365TokenProvider;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.threading.ThreadPools;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.http.UrlUtil;
import com.logicaldoc.util.io.FileUtil;
// import com.sun.mail.smtp.SMTPTransport;
import com.logicaldoc.util.spring.Context;

import jakarta.activation.DataHandler;
import jakarta.activation.DataSource;
import jakarta.activation.URLDataSource;
import jakarta.mail.Authenticator;
import jakarta.mail.MessagingException;
import jakarta.mail.Multipart;
import jakarta.mail.PasswordAuthentication;
import jakarta.mail.Session;
import jakarta.mail.Transport;
import jakarta.mail.internet.AddressException;
import jakarta.mail.internet.InternetAddress;
import jakarta.mail.internet.MailDateFormat;
import jakarta.mail.internet.MimeBodyPart;
import jakarta.mail.internet.MimeMessage;
import jakarta.mail.internet.MimeMultipart;
import jakarta.mail.internet.MimeUtility;
import jakarta.mail.util.ByteArrayDataSource;
import net.sf.jmimemagic.Magic;
import net.sf.jmimemagic.MagicMatch;

/**
 * SMTP E-Mail sender service
 * 
 * @author Michael Scholz
 * @author Matteo Caruso - LogicalDOC
 */
public class EMailSender {

	private static final String MAIL_TRANSPORT_PROTOCOL = "mail.transport.protocol";

	private static final String UTF_8 = "UTF-8";

	private static final String THREAD_POOL = "Email";

	private static final Logger log = LoggerFactory.getLogger(EMailSender.class);

	public static final int SECURITY_NONE = 0;

	public static final int SECURITY_STARTTLS = 1;

	public static final int SECURITY_TLS = 2;

	public static final int SECURITY_SSL = 3;

	public static final String PROTOCOL_SMTP = "smtp";

	public static final String PROTOCOL_SMTP_MICROSOFT365 = "smtpmicrosoft365";

	public static final int FOLDERING_NONE = 0;

	public static final int FOLDERING_YEAR = 1;

	public static final int FOLDERING_MONTH = 2;

	public static final int FOLDERING_DAY = 3;

	private String host = "localhost";

	private String sender = "logicaldoc@acme.com";

	private String username = "";

	private String password = "";

	private int port = 25;

	private String protocol = PROTOCOL_SMTP;

	/**
	 * In case of OAuth authentication, this field stores the client secret
	 */
	private String clientSecret;

	/**
	 * In case of OAuth authentication, this field stores the client id
	 */
	private String clientId;

	/**
	 * In case of OAuth authentication, this field stores the tenant information
	 */
	private String clientTenant;

	private boolean authEncrypted = false;

	private int connectionSecurity = SECURITY_NONE;

	private int foldering = FOLDERING_DAY;

	private Long folderId;

	public EMailSender(long tenant) {
		TenantDAO tenantDao = Context.get(TenantDAO.class);
		try {
			loadSettings(tenantDao.findById(tenant).getName());
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}

	public EMailSender(String tenant) {
		loadSettings(tenant);
	}

	public void setTenant(long tenant) {
		TenantDAO tenantDao = Context.get(TenantDAO.class);
		try {
			loadSettings(tenantDao.findById(tenant).getName());
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}

	private void loadSettings(String tenant) {
		try {
			ContextProperties config = Context.get().getProperties();

			protocol = config.getProperty(tenant + ".smtp.protocol");
			host = config.getProperty(tenant + ".smtp.host");
			port = config.getInt(tenant + ".smtp.port");
			username = config.getProperty(tenant + ".smtp.username");
			password = config.getProperty(tenant + ".smtp.password");
			sender = config.getProperty(tenant + ".smtp.sender");
			authEncrypted = "true".equals(config.getProperty(tenant + ".smtp.authEncrypted"));
			connectionSecurity = config.getInt(tenant + ".smtp.connectionSecurity");
			folderId = config.getLong(tenant + ".smtp.save.folderId", 0);
			foldering = config.getInt(tenant + ".smtp.save.foldering", FOLDERING_DAY);
			clientSecret = config.getProperty(tenant + ".smtp.clientSecret");
			clientId = config.getProperty(tenant + ".smtp.clientId");
			clientTenant = config.getProperty(tenant + ".smtp.clientTenant");
		} catch (Exception t) {
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
		ThreadPools tPools = Context.get(ThreadPools.class);
		tPools.execute(() -> {
			try {
				send(email, templateName, dictionary);
			} catch (Exception e) {
				log.error(e.getMessage(), e);
			}
			return null;
		}, THREAD_POOL);
	}

	/**
	 * Sends an email by using a given template
	 * 
	 * @param email The email to send
	 * @param templateName Name of the template to be applied
	 * @param dictionary The dictionary to be used in the template
	 * 
	 * @throws MessagingException raised if the email cannot be sent
	 * @throws AutomationException the automation has been evaluated but
	 *         produced an error
	 */
	public void send(EMail email, String templateName, Map<String, Object> dictionary)
			throws MessagingException, AutomationException {
		if (!RunLevel.current().aspectEnabled("sendingMessages")) {
			log.warn("Aspect sendingMessages not enabled");
			return;
		}

		MessageTemplateDAO templateDao = Context.get(MessageTemplateDAO.class);
		MessageTemplate template = null;
		try {
			template = templateDao.findByNameAndLanguage(templateName, email.getLocale().toString(),
					email.getTenantId());
			if (template == null)
				templateDao.findByNameAndLanguage(templateName, email.getLocale().toString(), Tenant.DEFAULT_ID);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
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
		ThreadPools tPools = Context.get(ThreadPools.class);
		tPools.execute(() -> {
			try {
				send(email);
			} catch (Exception e) {
				log.error(e.getMessage(), e);
			}
			return null;
		}, THREAD_POOL);
	}

	/**
	 * This method sends an email using the smtp-protocol. The email can be a
	 * simple mail or a multipart mail containing up to 5 attachments.
	 * 
	 * @param email E-Mail which should be sent.
	 * 
	 * @throws MessagingException raised if the email cannot be sent
	 */
	public void send(EMail email) throws MessagingException {
		if (!RunLevel.current().aspectEnabled("sendingMessages")) {
			log.error("Aspect sendingMessages not enabled");
			return;
		}

		cleanAuthorAddress(email);

		Session session = newMailSession();

		MimeMessage message = new MimeMessage(session);
		for (Map.Entry<String, String> line : email.getHeaders().entrySet())
			message.addHeaderLine(line.getKey() + "=" + line.getValue());

		// The FROM field must to be the one configured for the SMTP connection.
		// because of errors will be returned in the case the sender is not in
		// the SMTP domain.
		InternetAddress from = prepareFrom(email);
		Set<InternetAddress> to = email.getAddresses();
		Set<InternetAddress> cc = email.getAddressesCC();
		Set<InternetAddress> bcc = email.getAddressesBCC();
		message.setFrom(from);
		if (CollectionUtils.isNotEmpty(to))
			message.setRecipients(jakarta.mail.Message.RecipientType.TO, to.toArray(new InternetAddress[0]));
		if (CollectionUtils.isNotEmpty(cc))
			message.setRecipients(jakarta.mail.Message.RecipientType.CC, cc.toArray(new InternetAddress[0]));
		if (CollectionUtils.isNotEmpty(bcc))
			message.setRecipients(jakarta.mail.Message.RecipientType.BCC, bcc.toArray(new InternetAddress[0]));
		message.setSubject(email.getSubject(), UTF_8);

		/*
		 * If we have to images, the parts must be 'related' otherwise 'mixed'
		 */
		Multipart multipartMessage = new MimeMultipart(email.getImages().isEmpty() ? "mixed" : "related");

		if (StringUtils.isNotEmpty(email.getMessageText())) {
			MimeBodyPart body = buildBodyPart(email);
			multipartMessage.addBodyPart(body);
		}

		includeImages(email, multipartMessage);

		includeAttachments(email, multipartMessage);

		message.setContent(multipartMessage);

		MailDateFormat formatter = new MailDateFormat();
		formatter.setTimeZone(TimeZone.getTimeZone("GMT")); // always use UTC
															// for outgoing mail
		Date now = new Date();
		message.setHeader("Date", formatter.format(now));

		boolean noSend = false;
		try {
			noSend = Context.get().getProperties().getBoolean("smtp.nosend", false);
		} catch (Exception e) {
			// Context has not been initialized
			try {
				noSend = new ContextProperties().getBoolean("smtp.nosend", false);
			} catch (IOException e1) {
				log.warn(e1.getMessage());
			}
		}

		if (!noSend) {
			try (Transport transport = buildTransport(session);) {
				transport.sendMessage(message, message.getAllRecipients());
			} catch (IOException e) {
				throw new MessagingException(e.getMessage(), e);
			}

			log.info("Sent email with subject '{}' to recipients {}", email.getSubject(),
					email.getAllRecipientsEmails());
		} else {
			log.info("Email with subject '{}' not sent because of the config parameter smtp.nosend",
					email.getSubject());
		}

		/*
		 * If the case, we save the email as document in LogicalDOC's repository
		 */
		email.setSentDate(now);
		historycizeOutgoingEmail(email, message, from);
	}

	private void includeAttachments(EMail email, Multipart multipartMessage) throws MessagingException {
		for (Integer partId : email.getAttachments().keySet()) {
			EMailAttachment att = email.getAttachment(partId);
			String mime = detectMimeType(att);
			DataSource fdSource = new ByteArrayDataSource(att.getData(), mime);
			DataHandler fdHandler = new DataHandler(fdSource);
			MimeBodyPart part = new MimeBodyPart();
			part.setDataHandler(fdHandler);
			try {
				String fileName = MimeUtility.encodeText(att.getFileName(), UTF_8, null);
				part.setFileName(fileName);
			} catch (UnsupportedEncodingException e) {
				throw new MessagingException(e.getMessage(), e);
			}

			if (StringUtils.isNotEmpty(att.getDisposition())) {
				if ("remove".equals(att.getDisposition())) {
					part.removeHeader("Content-Disposition");
				} else {
					part.setDisposition(att.getDisposition());
				}
			}

			if (StringUtils.isNotEmpty(att.getContentType()))
				part.setHeader("Content-Type", att.getContentType());

			if (StringUtils.isNotEmpty(att.getContentEncoding()))
				part.setHeader("Content-Transfer-Encoding", att.getContentEncoding());

			multipartMessage.addBodyPart(part);
		}
	}

	private void includeImages(EMail email, Multipart multipartMessage) throws MessagingException {
		int i = 1;
		for (String image : email.getImages()) {
			MimeBodyPart imageBodyPart = new MimeBodyPart();

			try {
				DataSource ds = new URLDataSource(UrlUtil.toURL(image));
				imageBodyPart.setDataHandler(new DataHandler(ds));
			} catch (MalformedURLException | URISyntaxException e) {
				throw new MessagingException(e.getMessage(), e);
			}

			imageBodyPart.setHeader("Content-ID", "<image_" + (i++) + ">");
			imageBodyPart.setDisposition("inline");
			multipartMessage.addBodyPart(imageBodyPart);
		}
	}

	protected InternetAddress prepareFrom(EMail email) throws AddressException {
		InternetAddress from = new InternetAddress(sender);
		if (StringUtils.isNotEmpty(email.getAuthorAddress()))
			try {
				from = new InternetAddress(email.getAuthorAddress());
			} catch (AddressException t) {
				// Nothing to do
			}
		return from;
	}

	private static String tokenForSMTP(String userName, String accessToken) {
		final String ctrlA = Character.toString((char) 1);
		final String coded = "user=" + userName + ctrlA + "auth=Bearer " + accessToken + ctrlA + ctrlA;
		return Base64.getEncoder().encodeToString(coded.getBytes());
	}

	private void cleanAuthorAddress(EMail email) {
		try {
			TenantDAO tDao = Context.get(TenantDAO.class);
			String tenantName = tDao.getTenantName(email.getTenantId());
			if (!Context.get().getProperties().getBoolean(tenantName + ".smtp.userasfrom", false))
				email.setAuthorAddress(null);
		} catch (Exception e) {
			// Nothing to do, when using outside a Spring context this code
			// fails but this is not a problem
		}
	}

	private Properties prepareMailSessionProperties() {
		Properties props = new Properties();
		if (!StringUtils.isEmpty(username))
			props.put("mail.smtp.auth", "true");

		if (authEncrypted) {
			// The 'smtps' protocol must be used
			props.put(MAIL_TRANSPORT_PROTOCOL, "smtps");
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
			props.put(MAIL_TRANSPORT_PROTOCOL, "smtp");
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

		putOffice365settings(props);

		return props;
	}

	protected void putOffice365settings(Properties props) {
		if (protocol.contains("365")) {
			props.clear();
			props.put("mail.smtp.auth.xoauth2.disable", "false");
			props.put("mail.smtp.sasl.enable", "true");
			props.put("mail.smtp.auth.mechanisms", "XOAUTH2");
			props.put("mail.smtp.starttls.enable", "true");
			props.put(MAIL_TRANSPORT_PROTOCOL, "smtp");
			props.put("mail.smtp.host", host);
			props.put("mail.smtp.port", port);
			props.put("mail.debug", "true");
		}
	}

	private Session newMailSession() {
		Properties props = prepareMailSessionProperties();

		Session sess = null;
		if (StringUtils.isNotEmpty(username) && PROTOCOL_SMTP.equals(protocol))
			sess = Session.getInstance(props, new Authenticator() {
				@Override
				protected PasswordAuthentication getPasswordAuthentication() {
					return new PasswordAuthentication(username, password);
				}
			});
		else
			sess = Session.getInstance(props);

		return sess;
	}

	private Transport buildTransport(Session session) throws MessagingException, IOException {
		Transport transport = null;
		if (authEncrypted)
			transport = session.getTransport("smtps");
		else
			transport = session.getTransport("smtp");

		if (protocol.equals(PROTOCOL_SMTP_MICROSOFT365)) {
			transport = session.getTransport();

			String token = new Microsoft365TokenProvider(clientSecret, clientId, clientTenant).getAccessToken();
			transport.connect(username, token);
		} else {
			if (StringUtils.isEmpty(username)) {
				transport.connect(host, port, null, null);
			} else {
				transport.connect(host, port, username, password);
			}
		}
		return transport;
	}

	private MimeBodyPart buildBodyPart(EMail email) throws MessagingException {
		MimeBodyPart body = new MimeBodyPart();
		if (email.isHtml()) {
			body.setContent(new String(email.getMessageText().getBytes(StandardCharsets.UTF_8), StandardCharsets.UTF_8),
					"text/html; charset=UTF-8; fileNameCharset=UTF-8");
		} else {
			body.setText(email.getMessageText(), UTF_8);
		}
		return body;
	}

	private String detectMimeType(EMailAttachment att) {
		String mime = StringUtils.isEmpty(att.getMimeType()) ? att.getMimeType() : "text/plain";
		try {
			MagicMatch match = Magic.getMagicMatch(att.getData(), true);
			mime = match.getMimeType();
		} catch (Exception e) {
			// Nothing to do
		}
		return mime;
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

		DocumentManager manager = Context.get(DocumentManager.class);
		TemplateDAO templateDao = Context.get(TemplateDAO.class);
		UserDAO userDao = Context.get(UserDAO.class);

		FolderDAO folderDao = Context.get(FolderDAO.class);
		Folder saveFolder = null;
		try {
			saveFolder = folderId != null && folderId != 0 ? folderDao.findFolder(folderId) : null;
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		if (saveFolder == null)
			return;

		File emlFile = null;
		try {
			emlFile = FileUtil.createTempFile("emailsender", ".eml");

			try (FileOutputStream fos = new FileOutputStream(emlFile);) {
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
				Map<String, Attribute> attributes = new HashMap<>();
				Attribute ext = new Attribute();
				ext.setStringValue(StringUtils.substring(from.getAddress(), 0, 3999));
				attributes.put("from", ext);

				ext = new Attribute();
				ext.setStringValue(StringUtils.substring(
						email.getAddresses().stream().map(a -> a.getAddress()).collect(Collectors.joining(", ")), 0,
						3999));
				attributes.put("to", ext);

				ext = new Attribute();
				ext.setStringValue(StringUtils.substring(
						email.getAddressesCC().stream().map(a -> a.getAddress()).collect(Collectors.joining(", ")), 0,
						3999));
				attributes.put("cc", ext);

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
		} catch (Exception t) {
			log.warn("Cannot historycize the email with subject '{}' sent to {}", email.getSubject(),
					email.getAllRecipientsEmails(), t);
		} finally {
			FileUtil.delete(emlFile);
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

	public String getProtocol() {
		return protocol;
	}

	public String getClientSecret() {
		return clientSecret;
	}

	public String getClientId() {
		return clientId;
	}

	public String getClientTenant() {
		return clientTenant;
	}

	public void setProtocol(String protocol) {
		this.protocol = protocol;
	}

	public void setClientSecret(String clientSecret) {
		this.clientSecret = clientSecret;
	}

	public void setClientId(String clientId) {
		this.clientId = clientId;
	}

	public void setClientTenant(String clientTenant) {
		this.clientTenant = clientTenant;
	}
}