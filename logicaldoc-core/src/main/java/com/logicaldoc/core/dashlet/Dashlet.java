package com.logicaldoc.core.dashlet;

import com.logicaldoc.core.PersistentObject;

/**
 * Represents a sub-window of a dashboard
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
public class Dashlet extends PersistentObject {

	private static final long serialVersionUID = 1L;

	public static final String TYPE_DOCEVENT = "docevent";

	public static final String TYPE_DOCUMENT = "document";
	
	public static final String TYPE_BOOKMARK = "bookmark";

	public static final String TYPE_NOTE = "note";

	public static final String TYPE_CONTENT = "content";

	private String type = TYPE_DOCEVENT;

	private String query;

	private String content;

	private String name;

	private String title;

	private Integer max;

	/**
	 * To mark that it must display just unique records
	 */
	private int unique = 0;

	private String columns;

	public Dashlet() {
	}

	public Dashlet(Dashlet source) {
		super();
		this.type = source.type;
		this.query = source.query;
		this.content = source.content;
		this.name = source.name;
		this.title = source.title;
		this.max = source.max;
		this.unique = source.unique;
		this.columns = source.columns;

		setTenantId(source.getTenantId());
	}

	public String getType() {
		return type;
	}

	public String getQuery() {
		return query;
	}

	public String getContent() {
		return content;
	}

	public String getName() {
		return name;
	}

	public void setType(String type) {
		this.type = type;
	}

	public void setQuery(String query) {
		this.query = query;
	}

	public void setContent(String content) {
		this.content = content;
	}

	public void setName(String name) {
		this.name = name;
	}

	@Override
	public String toString() {
		return super.toString();
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public Integer getMax() {
		return max;
	}

	public void setMax(Integer max) {
		this.max = max;
	}

	public String getColumns() {
		return columns;
	}

	public void setColumns(String columns) {
		this.columns = columns;
	}

	public int getUnique() {
		return unique;
	}

	public void setUnique(int unique) {
		this.unique = unique;
	}
}