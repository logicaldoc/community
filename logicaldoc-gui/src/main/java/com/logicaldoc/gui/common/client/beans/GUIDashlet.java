package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Setting for the dashlet visualization.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.6
 */
public class GUIDashlet implements Serializable {

	private static List<String> systemDashlets = Arrays.asList("checkin", "checkout", "locked", "download", "locket",
			"change", "tagcloud", "notes", "lastaccessed");

	public static final String TYPE_DOCEVENT = "docevent";

	public static final String TYPE_DOCUMENT = "document";

	public static final String TYPE_NOTE = "note";

	public static final String TYPE_CONTENT = "content";

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

	private String columns;

	private boolean unique = false;

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

	public boolean isSystemDashlet() {
		return isSystemDashlet(name);
	}

	public static boolean isSystemDashlet(String name) {
		return systemDashlets.contains(name);
	}

	public String getColumns() {
		return columns;
	}

	public void setColumns(String columns) {
		this.columns = columns;
	}

	public List<String> getColumnsList() {
		List<String> set = new ArrayList<>();
		if (columns != null && !columns.isEmpty()) {
			if (!columns.contains(",")) {
				set.add(columns.trim());
			} else {
				String[] tokens = columns.split(",");
				for (String token : tokens)
					set.add(token.trim());
			}
		}
		return set;
	}

	/**
	 * Retrieve the names of those columns that refer to extended attributes
	 * 
	 * @return the extended attribute names
	 */
	public List<String> getExtendedAttributes() {
		List<String> set = new ArrayList<>();
		for (String col : getColumnsList()) {
			if (col.startsWith("ext_"))
				set.add(col.substring(4));
		}
		return set;
	}

	public boolean isUnique() {
		return unique;
	}

	public void setUnique(boolean unique) {
		this.unique = unique;
	}
}