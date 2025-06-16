package com.logicaldoc.core.dashlet;

import com.logicaldoc.core.PersistentObject;

import jakarta.persistence.Cacheable;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

/**
 * Represents a sub-window of a dashboard
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
@Entity
@Table(name = "ld_dashlet")
@Cacheable
public class Dashlet extends PersistentObject {

	private static final long serialVersionUID = 1L;

	public static final String TYPE_DOCEVENT = "docevent";

	public static final String TYPE_DOCUMENT = "document";

	public static final String TYPE_BOOKMARK = "bookmark";

	public static final String TYPE_NOTE = "note";

	public static final String TYPE_CONTENT = "content";

	@Column(name = "ld_name", length = 255, nullable = false)
	private String name;

	@Column(name = "ld_title", length = 255, nullable = false)
	private String title;

	@Column(name = "ld_type", length = 255, nullable = false)
	private String type = TYPE_DOCEVENT;

	@Column(name = "ld_max")
	private Integer max;

	/**
	 * To mark that it must display just unique records
	 */
	@Column(name = "ld_unique", nullable = false)
	private int unique = 0;

	@Column(name = "ld_query")
	private String query;

	@Column(name = "ld_content")
	private String content;

	@Column(name = "ld_columns")
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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((name == null) ? 0 : name.hashCode());
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
		Dashlet other = (Dashlet) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return true;
	}
}