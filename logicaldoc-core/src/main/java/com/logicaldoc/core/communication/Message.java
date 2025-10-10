package com.logicaldoc.core.communication;

import java.util.Date;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

import com.logicaldoc.core.PersistentObject;

import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.MappedSuperclass;
import jakarta.persistence.Transient;

/**
 * This is the parent class for email and system message.
 * 
 * @author Michael Scholz
 * @author Marco Meschieri
 * @version 1.0
 */
@MappedSuperclass
public abstract class Message extends PersistentObject {

	private static final long serialVersionUID = 1L;

	public static final int TYPE_SYSTEM = 0;

	public static final int TYPE_NOTIFICATION = 1;

	@Column(name = "ld_author", length = 255)
	private String author = "";
	
	@Column(name = "ld_messagetext")
	private String messageText = "";

	@Column(name = "ld_subject", length = 255)
	private String subject = "";

	/**
	 * When the message was sent
	 */
	@Column(name = "ld_sentdate", nullable = false, columnDefinition = "DATETIME(3)")
	private Date sentDate = new Date();

	/**
	 * When the message was received
	 */
	@Transient
	private Date receivedDate = new Date();

	@Column(name = "ld_type", nullable = false)
	private int type = Message.TYPE_SYSTEM;

	@ElementCollection
	@CollectionTable(name = "ld_recipient", joinColumns = @JoinColumn(name = "ld_messageid"))
	private Set<Recipient> recipients = new HashSet<>();

	/**
	 * The locale in which the message is written
	 */
	@Transient
	protected Locale locale;

	@Column(name = "ld_html", nullable = false)
	protected int html = 0;

	/**
	 * A flag that can be used to specify if the message has to be notified
	 */
	@Transient
	protected boolean notify = true;

	public boolean isNotify() {
		return notify;
	}

	public void setNotify(boolean notify) {
		this.notify = notify;
	}

	public String getMessageText() {
		return messageText;
	}

	public String getAuthor() {
		return author;
	}

	public String getSubject() {
		return subject;
	}

	public void setMessageText(String mess) {
		messageText = mess;
	}

	public void setAuthor(String auth) {
		author = auth;
	}

	public void setSubject(String subj) {
		subject = subj;
	}

	public Date getSentDate() {
		return sentDate;
	}

	public void setSentDate(Date sentDate) {
		this.sentDate = sentDate;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public Recipient getRecipient(String name) {
		for (Recipient recipient : recipients) {
			if (name.equals(recipient.getName()))
				return recipient;
		}
		return null;
	}

	public boolean wasReadBy(String name) {
		Recipient rec = getRecipient(name);
		if (rec != null)
			return rec.getRead() == 1;
		else
			return false;
	}

	public Set<Recipient> getRecipients() {
		return recipients;
	}

	public void setRecipients(Set<Recipient> recipients) {
		this.recipients = recipients;
	}

	public Locale getLocale() {
		return locale;
	}

	public void setLocale(Locale locale) {
		this.locale = locale;
	}

	public int getHtml() {
		return html;
	}

	public void setHtml(int html) {
		this.html = html;
	}

	public Date getReceivedDate() {
		return receivedDate;
	}

	public void setReceivedDate(Date receivedDate) {
		this.receivedDate = receivedDate;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((author == null) ? 0 : author.hashCode());
		result = prime * result + ((subject == null) ? 0 : subject.hashCode());
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
		Message other = (Message) obj;
		if (author == null) {
			if (other.author != null)
				return false;
		} else if (!author.equals(other.author))
			return false;
		if (subject == null) {
			if (other.subject != null)
				return false;
		} else if (!subject.equals(other.subject))
			return false;
		return true;
	}
}
