package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * Setting for the dashlet visualization.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.6
 */
public class GUIDashlet implements Serializable {

	public final static String TYPE_DOCEVENT = "docevent";

	public final static String TYPE_DOCUMENT = "document";

	public final static String TYPE_NOTE = "note";

	public final static String TYPE_CONTENT = "content";
	
	private static final long serialVersionUID = 1L;

	private long id;

	private int column;

	private int row;

	private int index;

	private String query;

	private String content;

	private String name;

	private String title;
	
	private String type;

	private Integer max;
	
	public GUIDashlet() {
	}

	public GUIDashlet(String name, int column, int row, int index) {
		super();
		this.name = name;
		this.column = column;
		this.row = row;
		this.index = index;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public int getColumn() {
		return column;
	}

	public void setColumn(int column) {
		this.column = column;
	}

	public int getRow() {
		return row;
	}

	public void setRow(int row) {
		this.row = row;
	}

	public int getIndex() {
		return index;
	}

	public void setIndex(int index) {
		this.index = index;
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

	public String getTitle() {
		return title;
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

	public void setTitle(String title) {
		this.title = title;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public Integer getMax() {
		return max;
	}

	public void setMax(Integer max) {
		this.max = max;
	}
}