package com.logicaldoc.core.searchengine.saved;

import java.io.Serializable;
import java.util.Date;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.searchengine.SearchOptions;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.IOUtil;

/**
 * A search saved in the database
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 *
 */
public class SavedSearch extends PersistentObject implements Serializable, Comparable<SavedSearch> {

	private static final long serialVersionUID = 1L;

	private long userId;

	private String name = "";

	private String description = "";

	private int type = SearchOptions.TYPE_FULLTEXT;

	private Date date = new Date();

	private String options = "";

	public SavedSearch() {
	}

	public SavedSearch(SavedSearch source) {
		this.userId = source.userId;
		this.name = source.name;
		this.description = source.description;
		this.type = source.type;
		this.date = source.date;
		this.options = source.options;
	}

	public void saveOptions(SearchOptions opt) throws PersistenceException {
		this.setType(opt.getType());

		TenantDAO tenantDao = Context.get(TenantDAO.class);
		String tenantName = tenantDao.getTenantName(getTenantId());
		String charset = Context.get().getProperties().getProperty(tenantName + ".charset", "UTF-8");

		setOptions(IOUtil.serialize(opt, charset));
	}

	public SearchOptions readOptions() {
		SearchOptions searchOptions = (SearchOptions) IOUtil.deserialize(getOptions());
		searchOptions.setName(getName());
		searchOptions.setDescription(getDescription());
		return searchOptions;
	}

	@Override
	public int compareTo(SavedSearch other) {
		return this.getOptions().compareTo(other.getOptions());
	}
	
	public long getUserId() {
		return userId;
	}

	public void setUserId(long userId) {
		this.userId = userId;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Date getDate() {
		return date;
	}

	public void setDate(Date date) {
		this.date = date;
	}

	public String getOptions() {
		return options;
	}

	public void setOptions(String options) {
		this.options = options;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + (int) (userId ^ (userId >>> 32));
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
		SavedSearch other = (SavedSearch) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return userId == other.userId;
	}
}