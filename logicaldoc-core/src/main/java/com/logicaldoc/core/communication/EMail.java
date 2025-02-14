package com.logicaldoc.core.communication;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.stream.Collectors;

import javax.mail.internet.AddressException;
import javax.mail.internet.InternetAddress;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.folder.Folder;

/**
 * A represetnation of an Email message.
 * 
 * 
 * @author Michael Scholz
 * @author Marco Meschieri - LogicalDOC
 */
public class EMail extends Message {

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(EMail.class);

	private String authorAddress = "";

	private String username = "";

	/**
	 * Name of the folder inside the mail server
	 */
	private String folder = "";

	/**
	 * Refers to the original email id
	 */
	private String emailId = "";

	/**
	 * The e-mail account used to fetch this message
	 */
	private long accountId;

	/**
	 * Optional headers to put in the email
	 */
	private Map<String, String> headers = new HashMap<>();

	private Map<Integer, EMailAttachment> attachments = new HashMap<>();

	private Recipient from;

	private Set<Recipient> recipientsCC = new HashSet<>();

	private Set<Recipient> recipientsBCC = new HashSet<>();

	private Set<Recipient> replyTo = new HashSet<>();

	/**
	 * This images are added as part of the body, each image will be identified
	 * with 'image_i'. Use an URL to reference them.
	 */
	private Set<String> images = new HashSet<>();

	private int attachmentsCount = 0;

	private int signed = 0;

	/**
	 * Indicates if the email must be discarded by the elaboration
	 */
	private boolean skip = false;

	/**
	 * If this message supports the historycization in case of ourgoing message
	 */
	private boolean historicyze = true;

	/**
	 * Folder where the document representing this email will be saved in
	 */
	private Folder targetFolder;

	public String getEmailId() {
		return emailId;
	}

	public void setEmailId(String emailId) {
		this.emailId = emailId;
	}

	public long getAccountId() {
		return accountId;
	}

	public void setAccountId(long accountId) {
		this.accountId = accountId;
	}

	public String getAuthorAddress() {
		return authorAddress;
	}

	public String getUsername() {
		return username;
	}

	public void setAuthorAddress(String address) {
		authorAddress = address;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public void addRecipient(Recipient rec) {
		getRecipients().add(rec);
	}

	public String getFolder() {
		return folder;
	}

	public void setFolder(String string) {
		folder = string;
	}

	public Map<Integer, EMailAttachment> getAttachments() {
		return attachments;
	}

	public EMailAttachment getAttachment(int partId) {
		return attachments.get(partId);
	}

	public void addAttachment(int partId, EMailAttachment attachment) {
		attachments.put(partId, attachment);
	}

	public void addAttachment(EMailAttachment attachment) {
		Integer max = 0;
		if (!getAttachments().isEmpty())
			max = Collections.max(getAttachments().keySet());
		attachments.put(max.intValue() + 1, attachment);
	}

	public Set<InternetAddress> getAddresses() {
		return getAddresses(getRecipients());
	}

	public Set<InternetAddress> getAddressesCC() {
		return getAddresses(recipientsCC);
	}

	public Set<InternetAddress> getAddressesBCC() {
		return getAddresses(recipientsBCC);
	}

	private Set<InternetAddress> getAddresses(Collection<Recipient> recipients) {
		Set<InternetAddress> addresses = new HashSet<>();
		for (Recipient recipient : recipients)
			try {
				addresses.add(new InternetAddress(recipient.getAddress()));
			} catch (AddressException e) {
				log.warn(e.getMessage());
			}
		return addresses;
	}

	public int getAttachmentsCount() {
		if (attachments.isEmpty())
			return attachmentsCount;
		else
			return attachments.size();
	}

	public void setAttachments(Map<Integer, EMailAttachment> attachments) {
		this.attachments = attachments;
	}

	public void parseRecipients(String str) {
		parseRecipients(str, getRecipients());
	}

	public void parseRecipientsCC(String str) {
		parseRecipients(str, recipientsCC);
	}

	public void parseRecipientsBCC(String str) {
		parseRecipients(str, recipientsBCC);
	}

	public void parseReplyTo(String str) {
		parseRecipients(str, replyTo);
	}

	private void parseRecipients(String str, Collection<Recipient> recipients) {
		StringTokenizer st = new StringTokenizer(str.trim().toLowerCase(), ", ;", false);
		recipients.clear();
		while (st.hasMoreTokens()) {
			String token = st.nextToken();
			Recipient recipient = new Recipient();
			recipient.setAddress(token);
			recipient.setName(token);
			recipient.setType(Recipient.TYPE_EMAIL);
			recipients.add(recipient);
		}
	}

	public Set<Recipient> getRecipientsCC() {
		return recipientsCC;
	}

	public void setRecipientsCC(Set<Recipient> recipientsCC) {
		this.recipientsCC = recipientsCC;
	}

	public Set<Recipient> getRecipientsBCC() {
		return recipientsBCC;
	}

	public void setRecipientsBCC(Set<Recipient> recipientsBCC) {
		this.recipientsBCC = recipientsBCC;
	}

	public Set<Recipient> getReplyTo() {
		return replyTo;
	}

	public void setReplyTo(Set<Recipient> replyTo) {
		this.replyTo = replyTo;
	}

	public boolean isHtml() {
		return html == 1;
	}

	public Set<String> getImages() {
		return images;
	}

	public void setImages(Set<String> images) {
		this.images = images;
	}

	public void setAttachmentsCount(int attachmentCount) {
		this.attachmentsCount = attachmentCount;
	}

	public Recipient getFrom() {
		return from;
	}

	public void setFrom(Recipient from) {
		this.from = from;
	}

	public boolean isSigned() {
		return signed == 1;
	}

	public int getSigned() {
		return signed;
	}

	public void setSigned(int signed) {
		this.signed = signed;
	}

	public boolean isSkip() {
		return skip;
	}

	public void setSkip(boolean skip) {
		this.skip = skip;
	}

	public Folder getTargetFolder() {
		return targetFolder;
	}

	public void setTargetFolder(Folder targetFolder) {
		this.targetFolder = targetFolder;
	}

	/**
	 * Retrieves the set of all the recipients of the message, does not matter
	 * if they are direct recipient or CC or BCC
	 * 
	 * @return the email addresses that will receive the message
	 */
	public Set<String> getAllRecipientsEmails() {
		Set<String> emails = new HashSet<>();
		emails.addAll(
				getRecipients().stream().map(r -> r.getAddress().trim().toLowerCase()).collect(Collectors.toSet()));
		emails.addAll(recipientsCC.stream().map(r -> r.getAddress().trim().toLowerCase()).collect(Collectors.toSet()));
		emails.addAll(recipientsBCC.stream().map(r -> r.getAddress().trim().toLowerCase()).collect(Collectors.toSet()));
		return emails;
	}

	public boolean isHistoricyze() {
		return historicyze;
	}

	public void setHistoricyze(boolean historicyze) {
		this.historicyze = historicyze;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + (int) (accountId ^ (accountId >>> 32));
		result = prime * result + ((emailId == null) ? 0 : emailId.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		EMail other = (EMail) obj;
		if (accountId != other.accountId)
			return false;
		if (emailId == null) {
			if (other.emailId != null)
				return false;
		} else if (!emailId.equals(other.emailId))
			return false;
		return true;
	}

	public Map<String, String> getHeaders() {
		return headers;
	}

	public void setHeaders(Map<String, String> headers) {
		this.headers = headers;
	}
}