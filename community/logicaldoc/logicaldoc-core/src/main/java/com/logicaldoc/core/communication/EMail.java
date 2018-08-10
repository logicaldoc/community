package com.logicaldoc.core.communication;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import javax.mail.internet.AddressException;
import javax.mail.internet.InternetAddress;

/**
 * @author Michael Scholz
 * @author Marco Meschieri - LogicalDOC
 */
public class EMail extends Message {

	private String authorAddress = "";

	private String username = "";

	private String folder = "";

	// Refers to the original email id
	private String emailId = "";

	// The e-mail account used to fetch this message
	private long accountId;

	private Map<Integer, EMailAttachment> attachments = new HashMap<Integer, EMailAttachment>();

	private Recipient from;
	
	private Recipient replyTo;

	private Set<Recipient> recipientsCC = new HashSet<Recipient>();

	private Set<Recipient> recipientsBCC = new HashSet<Recipient>();

	/**
	 * This images are added as part of the body, each image will be identified
	 * with 'image_i'. Use an URL to reference them.
	 */
	private Set<String> images = new HashSet<String>();

	private int attachmentsCount = 0;

	public EMail() {
	}

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
		recipients.add(rec);
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

	public InternetAddress[] getAddresses() throws Exception {
		return getAddresses(recipients);
	}

	public InternetAddress[] getAddressesCC() throws Exception {
		return getAddresses(recipientsCC);
	}

	public InternetAddress[] getAddressesBCC() throws Exception {
		return getAddresses(recipientsBCC);
	}

	private InternetAddress[] getAddresses(Collection<Recipient> recipients) throws AddressException {
		InternetAddress[] recs = new InternetAddress[recipients.size()];
		Iterator<Recipient> iter = recipients.iterator();
		int i = 0;

		while (iter.hasNext()) {
			Recipient rec = iter.next();
			recs[i] = new InternetAddress(rec.getAddress());
			i++;
		}

		return recs;
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
		parse(str, recipients);
	}

	public void parseRecipientsCC(String str) {
		parse(str, recipientsCC);
	}

	public void parseRecipientsBCC(String str) {
		parse(str, recipientsBCC);
	}

	private void parse(String str, Collection<Recipient> recipients) {
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

	public Recipient getReplyTo() {
		return replyTo;
	}

	public void setReplyTo(Recipient replyTo) {
		this.replyTo = replyTo;
	}

	public Recipient getFrom() {
		return from;
	}

	public void setFrom(Recipient from) {
		this.from = from;
	}
}