package com.logicaldoc.core.communication;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import jakarta.persistence.Cacheable;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

/**
 * SystemMessages are messages which an user only can send to other system
 * users.
 * 
 * @author Michael Scholz
 * @author Marco Meschieri - LogicalDOC
 * @since 1.0
 */
@Entity
@Table(name = "ld_systemmessage")
@Cacheable
public class SystemMessage extends Message {

	private static final long serialVersionUID = 1L;

	public static final int STATUS_NEW = 0;

	public static final int STATUS_DELIVERED = 1;

	public static final int STATUS_ERROR = 2;

	@Column(name = "ld_datescope")
	private int dateScope = 10;

	/**
	 * A priority: <b>0</b> = low, <b>1</b> = medium, <b>2</b> = high
	 */
	@Column(name = "ld_prio")
	private int prio = 0;

	@Column(name = "ld_confirmation")
	private int confirmation = 0; // 0 - false; 1 - true

	/**
	 * The date this message was last notified
	 */
	@Column(name = "ld_lastnotified", columnDefinition = "DATETIME(3)")
	private Date lastNotified = null;

	@Column(name = "ld_status", nullable = false)
	private int status = STATUS_NEW;

	/**
	 * The number of notification trials
	 */
	@Column(name = "ld_trials")
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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + status;
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
		SystemMessage other = (SystemMessage) obj;
		return status == other.status;
	}
}