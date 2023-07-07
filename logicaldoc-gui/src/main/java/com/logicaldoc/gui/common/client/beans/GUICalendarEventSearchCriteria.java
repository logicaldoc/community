package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

/**
 * The criteria to search calendar events
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.5
 */
public class GUICalendarEventSearchCriteria implements Serializable {

	private static final long serialVersionUID = 1L;

	/**
	 * Start date (optional)
	 */
	private Date startDate;

	/**
	 * End date (optional)
	 */
	private Date endDate;

	/**
	 * optional
	 */
	private Date expireFrom;

	/**
	 * optional
	 */
	private Date expireTo;

	/**
	 * The frequency of the event (1,15, 30 ... optional)
	 */
	private Integer frequency;

	/**
	 * The title (used with like operator, optional)
	 */
	private String title;

	/**
	 * The type (used with like operator, optional)
	 */
	private String type;

	/**
	 * The subtype (used with like operator, optional)
	 */
	private String subtype;

	/**
	 * The status (used with like operator, optional)
	 */
	private Integer status;

	/**
	 * Maximum number of records (optional)
	 */
	private Integer maxRecords;

	public GUICalendarEventSearchCriteria() {
	}

	public GUICalendarEventSearchCriteria(Date startDate, Date endDate, Date expireFrom, Date expireTo,
			Integer frequency) {
		super();
		this.startDate = startDate;
		this.endDate = endDate;
		this.expireFrom = expireFrom;
		this.expireTo = expireTo;
		this.frequency = frequency;
	}

	public Date getStartDate() {
		return startDate;
	}

	public void setStartDate(Date startDate) {
		this.startDate = startDate;
	}

	public Date getEndDate() {
		return endDate;
	}

	public void setEndDate(Date endDate) {
		this.endDate = endDate;
	}

	public Date getExpireFrom() {
		return expireFrom;
	}

	public void setExpireFrom(Date expireFrom) {
		this.expireFrom = expireFrom;
	}

	public Date getExpireTo() {
		return expireTo;
	}

	public void setExpireTo(Date expireTo) {
		this.expireTo = expireTo;
	}

	public Integer getFrequency() {
		return frequency;
	}

	public void setFrequency(Integer frequency) {
		this.frequency = frequency;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getSubtype() {
		return subtype;
	}

	public void setSubtype(String subtype) {
		this.subtype = subtype;
	}

	public Integer getStatus() {
		return status;
	}

	public void setStatus(Integer status) {
		this.status = status;
	}

	public Integer getMaxRecords() {
		return maxRecords;
	}

	public void setMaxRecords(Integer maxRecords) {
		this.maxRecords = maxRecords;
	}
}