package com.logicaldoc.core.searchengine;

import java.util.Date;
import java.util.Locale;

/**
 * Search options specialization for the Full text search.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
public class FulltextSearchOptions extends SearchOptions {

	private static final long serialVersionUID = 1L;

	private String expressionLanguage = Locale.ENGLISH.getLanguage();

	// Min size in bytes
	private Long sizeMin = null;

	// Max size in bytes
	private Long sizeMax = null;

	private String format = "";

	private String[] fields = null;

	private String language = null;

	private Date dateFrom = null;

	private Date dateTo = null;

	private Date creationFrom = null;

	private Date creationTo = null;

	public FulltextSearchOptions() {
		super(SearchOptions.TYPE_FULLTEXT);
	}

	public String getFormat() {
		return format;
	}

	public String[] getFields() {
		return fields;
	}

	public void setFormat(String form) {
		format = form;
	}

	public void setFields(String[] flds) {
		fields = flds;
	}

	public void addField(String s) {
		fields[fields.length] = s;
	}

	public Date getDateTo() {
		return dateTo;
	}

	public void setDateTo(Date dateTo) {
		this.dateTo = dateTo;
	}

	public Long getSizeMin() {
		return sizeMin;
	}

	public void setSizeMin(Long sizeMin) {
		this.sizeMin = sizeMin;
	}

	public Long getSizeMax() {
		return sizeMax;
	}

	public void setSizeMax(Long sizeMax) {
		this.sizeMax = sizeMax;
	}

	public Date getCreationTo() {
		return creationTo;
	}

	public void setCreationTo(Date creationTo) {
		this.creationTo = creationTo;
	}

	public Date getCreationFrom() {
		return creationFrom;
	}

	public void setCreationFrom(Date creationFrom) {
		this.creationFrom = creationFrom;
	}

	public Date getDateFrom() {
		return dateFrom;
	}

	public void setDateFrom(Date dateFrom) {
		this.dateFrom = dateFrom;
	}

	public String getLanguage() {
		return language;
	}

	public void setLanguage(String language) {
		this.language = language;
	}

	public String getExpressionLanguage() {
		return expressionLanguage;
	}

	public void setExpressionLanguage(String expressionLanguage) {
		this.expressionLanguage = expressionLanguage;
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof FulltextSearchOptions))
			return false;
		FulltextSearchOptions other = (FulltextSearchOptions) obj;
		return getName().equals(other.getName());
	}

	@Override
	public int hashCode() {
		return getName().hashCode();
	}
}