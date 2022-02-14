package com.logicaldoc.core.dashlet;

import com.logicaldoc.core.PersistentObject;

/**
 * Represents a sub-window of a dashboard
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
public class Dashlet extends PersistentObject implements Cloneable {

	public final static String TYPE_DOCEVENT = "docevent";

	public final static String TYPE_DOCUMENT = "document";

	public final static String TYPE_NOTE = "note";

	public final static String TYPE_CONTENT = "content";

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

	@Override
	public Object clone() throws CloneNotSupportedException {
		Dashlet dashlet = new Dashlet();
		dashlet.setId(getId());
		dashlet.setContent(content);
		dashlet.setMax(max);
		dashlet.setName(name);
		dashlet.setQuery(query);
		dashlet.setTenantId(getTenantId());
		dashlet.setTitle(title);
		dashlet.setType(type);
		dashlet.setColumns(columns);
		dashlet.setUnique(unique);
		return super.clone();
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