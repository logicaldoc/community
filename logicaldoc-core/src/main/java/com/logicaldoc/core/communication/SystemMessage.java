package com.logicaldoc.core.communication;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

/**
 * SystemMessages are messages which an user only can send to other system
 * users.
 * 
 * @author Michael Scholz
 * @author Marco Meschieri - LogicalDOC
 * @since 1.0
 */
public class SystemMessage extends Message {

	private static final long serialVersionUID = 1L;

	public static final int STATUS_NEW = 0;

	public static final int STATUS_DELIVERED = 1;

	public static final int STATUS_ERROR = 2;

	private int dateScope = 10;

	/**
	 * A priority: <b>0</b> = low, <b>1</b> = medium, <b>2</b> = high
	 */
	private int prio = 0;

	private int confirmation = 0; // 0 - false; 1 - true

	/**
	 * The date this message was last notified
	 */
	private Date lastNotified = null;

	private int status = STATUS_NEW;

	/**
	 * The number of notification trials
	 */
	private int trials = 0;

	public SystemMessage() {
	}

	public SystemMessage(SystemMessage source) {
		this.dateScope = source.dateScope;
		this.prio = source.prio;
		this.confirmation = source.confirmation;
		this.lastNotified = source.lastNotified;
		this.status = source.status;
		this.trials = source.trials;

		setAuthor(source.getAuthor());
		setHtml(source.getHtml());
		setLastNotified(source.getLastNotified());
		setLocale(source.getLocale());
		setMessageText(source.getMessageText());
		Set<Recipient> recs = new HashSet<>();
		for (Recipient rec : source.getRecipients())
			recs.add(new Recipient(rec));

		setRecipients(recs);
		setSentDate(source.getSentDate());
		setSubject(source.getSubject());
		setTenantId(source.getTenantId());
		setType(source.getType());
		setNotify(source.isNotify());
	}

	public int getDateScope() {
		return dateScope;
	}

	public int getPrio() {
		return prio;
	}

	public int getConfirmation() {
		return confirmation;
	}

	public void setDateScope(int scope) {
		dateScope = scope;
	}

	public void setPrio(int pri) {
		prio = pri;
	}

	public void setConfirmation(int conf) {
		confirmation = conf;
	}

	public Date getLastNotified() {
		return lastNotified;
	}

	public void setLastNotified(Date lastNotified) {
		this.lastNotified = lastNotified;
	}

	public int getStatus() {
		return status;
	}

	public void setStatus(int status) {
		this.status = status;
	}

	public int getTrials() {
		return trials;
	}

	public void setTrials(int trials) {
		this.trials = trials;
	}
}