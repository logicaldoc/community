package com.logicaldoc.core.ticket;

import java.util.Date;
import java.util.UUID;

import com.logicaldoc.core.PersistentObject;

/**
 * Represents ticket, most of the time this is used to model download tickets.
 * 
 * @author Michael Scholz
 * @author Marco Meschieri
 */
public class Ticket extends PersistentObject {

	private static final long serialVersionUID = 1L;

	public static final int DOWNLOAD = 0;

	public static final int PSW_RECOVERY = 1;

	public static final int VIEW = 2;

	private String ticketId = UUID.randomUUID().toString();

	private long docId = 0;

	private long userId = -1;

	private int type = DOWNLOAD;

	/**
	 * A date when this ticket expires
	 */
	private Date expired = null;

	private int count = 0;

	/**
	 * Maximum number of downloads
	 */
	private Integer maxCount;

	private int enabled = 1;

	private int views = 0;

	/**
	 * Maximum number of views
	 */
	private Integer maxViews;

	private String suffix;

	/*
	 * Not persistent field
	 */
	private String url;

	/**
	 * Not persistent field
	 */
	private Integer expireHours;

	public long getDocId() {
		return docId;
	}

	public void setDocId(long docId) {
		this.docId = docId;
	}

	/**
	 * @return Returns the ticketId.
	 */
	public String getTicketId() {
		return ticketId;
	}

	/**
	 * @param ticketId The ticketId to set.
	 */
	public void setTicketId(String ticketId) {
		this.ticketId = ticketId;
	}

	public long getUserId() {
		return userId;
	}

	public void setUserId(long userId) {
		this.userId = userId;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public Date getExpired() {
		return expired;
	}

	public void setExpired(Date expired) {
		this.expired = expired;
	}

	public int getCount() {
		return count;
	}

	public void setCount(int count) {
		this.count = count;
	}

	public boolean isTicketExpired() {
		return enabled == 0 || (expired != null && new Date().getTime() > expired.getTime())
				|| (maxCount != null && maxCount > 0 && count >= maxCount);
	}

	public boolean isTicketViewExpired() {
		return enabled == 0 || (expired != null && new Date().getTime() > expired.getTime())
				|| (maxViews != null && maxViews > 0 && views >= maxViews);
	}

	public String getSuffix() {
		return suffix;
	}

	public void setSuffix(String suffix) {
		this.suffix = suffix;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public Integer getMaxCount() {
		return maxCount;
	}

	public void setMaxCount(Integer maxCount) {
		this.maxCount = maxCount;
	}

	public int getEnabled() {
		return enabled;
	}

	public void setEnabled(int enabled) {
		this.enabled = enabled;
	}

	public int getViews() {
		return views;
	}

	public void setViews(int views) {
		this.views = views;
	}

	public Integer getMaxViews() {
		return maxViews;
	}

	public void setMaxViews(Integer maxViews) {
		this.maxViews = maxViews;
	}

	public Integer getExpireHours() {
		return expireHours;
	}

	public void setExpireHours(Integer expireHours) {
		this.expireHours = expireHours;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((ticketId == null) ? 0 : ticketId.hashCode());
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
		Ticket other = (Ticket) obj;
		if (ticketId == null) {
			if (other.ticketId != null)
				return false;
		} else if (!ticketId.equals(other.ticketId))
			return false;
		return true;
	}
}