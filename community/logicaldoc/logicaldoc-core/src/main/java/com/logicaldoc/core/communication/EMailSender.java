package com.logicaldoc.core.communication;

import java.net.URL;
import java.util.Map;
import java.util.Properties;

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
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;
import javax.mail.internet.MimeUtility;
import javax.mail.util.ByteArrayDataSource;

import net.sf.jmimemagic.Magic;
import net.sf.jmimemagic.MagicMatch;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.automation.Automation;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * SMTP E-Mail sender service
 * 
 * @author Michael Scholz
 * @author Matteo Caruso - Logical Objects
 */
public class EMailSender {
	private static Logger log = LoggerFactory.getLogger(EMailSender.class);

	public static final int SECURITY_NONE = 0;

	public static final int SECURITY_TLS_IF_AVAILABLE = 1;

	public static final int SECURITY_TLS = 2;

	public static final int SECURITY_SSL = 3;

	private String host = "localhost";

	private String sender = "logicaldoc@acme.com";

	private String username = "";

	private String password = "";

	private int port = 25;

	private boolean authEncripted = false;

	private int connectionSecurity = SECURITY_NONE;

	public EMailSender(long tenant) {
		TenantDAO tenantDao = (TenantDAO) Context.get().getBean(TenantDAO.class);
		loadSettings(tenantDao.findById(tenant).getName());
	}

	public EMailSender(String tenant) {
		loadSettings(tenant);
	}

	private void loadSettings(String tenant) {
		ContextProperties config = Context.get().getProperties();

		host = config.getProperty(tenant + ".smtp.host");
		port = config.getInt(tenant + ".smtp.port");
		username = config.getProperty(tenant + ".smtp.username");
		password = config.getProperty(tenant + ".smtp.password");
		sender = config.getProperty(tenant + ".smtp.sender");
		authEncripted = "true".equals(config.getProperty(tenant + ".smtp.authEncripted"));
		connectionSecurity = config.getInt(tenant + ".smtp.connectionSecurity");
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
	 */
	public void sendAsync(EMail email, String templateName, Map<String, Object> dictionary) {
		new Thread(() -> {
			try {
				send(email, templateName, dictionary);
			} catch (Exception e) {
				log.error(e.getMessage(), e);
			}
		}).start();
	}

	/**
	 * Sends an email by using a given template
	 * 
	 * @param email The email to send
	 * @param templateName Name of the template to be applied
	 * @param dictionary The dictionary to be used in the template
	 * @throws Exception
	 */
	public void send(EMail email, String templateName, Map<String, Object> dictionary) throws Exception {
		MessageTemplateDAO templateDao = (MessageTemplateDAO) Context.get().getBean(MessageTemplateDAO.class);
		MessageTemplate template = templateDao.findByNameAndLanguage(templateName, email.getLocale().toString(),
				email.getTenantId());
		if (template == null) {
			log.error("Template " + templateName + " was not found");
			return;
		}

		dictionary.put(Automation.LOCALE, email.getLocale());
		email.setSubject(template.getFormattedSubject(dictionary));
		email.setMessageText(template.getFormattedBody(dictionary));

		send(email);
	}

	/**
	 * Same as send(EMail) but executes in another thread
	 */
	public void sendAsync(EMail email) {
		new Thread(() -> {
			try {
				send(email);
			} catch (Exception e) {
				log.error(e.getMessage(), e);
			}
		}).start();
	}

	/**
	 * This method sends an email using the smtp-protocol. The email can be a
	 * simple mail or a multipart mail containing up to 5 attachments.
	 * 
	 * @param account E-Mail account of the sender.
	 * @param email E-Mail which should be sent.
	 * @throws Exception
	 */
	public void send(EMail email) throws Exception {
		Properties props = new Properties();
		if (!StringUtils.isEmpty(username))
			props.put("mail.smtp.auth", "true");

		if (authEncripted) {
			// The 'smtps' protocol must be used
			props.put("mail.transport.protocol", "smtps");
			props.put("mail.smtps.host", host);
			props.put("mail.smtps.port", port);
			if (connectionSecurity == SECURITY_TLS_IF_AVAILABLE)
				props.put("mail.smtps.starttls.enable", "true");
			if (connectionSecurity == SECURITY_TLS)
				props.put("mail.smtps.starttls.required", "true");
			if (connectionSecurity == SECURITY_SSL) {
				// Necessary property to send e-mails with SSL
				props.put("mail.smtps.starttls.enable", "true");
				props.put("mail.smtps.ssl.enable", "true");
			}
		} else {
			props.put("mail.transport.protocol", "smtp");
			props.put("mail.smtp.host", host);
			props.put("mail.smtp.port", port);
			if (connectionSecurity == SECURITY_TLS_IF_AVAILABLE)
				props.put("mail.smtp.starttls.enable", "true");
			if (connectionSecurity == SECURITY_TLS)
				props.put("mail.smtp.starttls.required", "true");
			if (connectionSecurity == SECURITY_SSL) {
				// Necessary property to send e-mails with SSL
				props.put("mail.smtp.starttls.enable", "true");
				props.put("mail.smtp.ssl.enable", "true");
			}
		}

		props.put("mail.smtp.ssl.checkserveridentity", "false");
		props.put("mail.smtp.ssl.trust", "*");
		// props.put("mail.debug", "true");

		Session sess = null;

		try {
			if (!StringUtils.isEmpty(username))
				sess = Session.getDefaultInstance(props, new Authenticator() {
					protected PasswordAuthentication getPasswordAuthentication() {
						return new PasswordAuthentication(username, password);
					}
				});
			else
				sess = Session.getDefaultInstance(props);
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
		if (authEncripted)
			trans = sess.getTransport("smtps");
		else
			trans = sess.getTransport("smtp");

		if (StringUtils.isEmpty(username)) {
			trans.connect(host, port, null, null);
		} else {
			trans.connect(host, port, username, password);
		}

		Address[] adr = message.getAllRecipients();
		trans.sendMessage(message, adr);
		trans.close();
	}

	public boolean isAuthEncripted() {
		return authEncripted;
	}

	public void setAuthEncripted(boolean authEncripted) {
		this.authEncripted = authEncripted;
	}

	public int getConnectionSecurity() {
		return connectionSecurity;
	}

	public void setConnectionSecurity(int connectionSecurity) {
		this.connectionSecurity = connectionSecurity;
	}
}