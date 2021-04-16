package com.logicaldoc.core.searchengine.saved;

import java.beans.XMLDecoder;
import java.beans.XMLEncoder;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.Serializable;
import java.util.Date;

import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.searchengine.SearchOptions;
import com.logicaldoc.util.io.StringOutputStream;

/**
 * A search saved in the database
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 *
 */
public class SavedSearch extends PersistentObject implements Serializable, Comparable<SavedSearch>, Cloneable {

	private static final long serialVersionUID = 1L;

	private long userId;

	private String name = "";

	private String description = "";

	private int type = SearchOptions.TYPE_FULLTEXT;

	private Date date = new Date();

	private String options = "";

	public void saveOptions(SearchOptions opt) throws IOException {
		this.setType(opt.getType());
		StringBuffer sb = new StringBuffer();
		try (StringOutputStream out = new StringOutputStream(sb);
				XMLEncoder encoder = new XMLEncoder(out, "UTF-8", false, 0)) {
			encoder.writeObject(opt);
		}
		setOptions(sb.toString());
	}

	public SearchOptions readOptions() throws IOException {
		try (XMLDecoder decoder = new XMLDecoder(new ByteArrayInputStream(getOptions().getBytes()))) {
			return (SearchOptions) decoder.readObject();
		}
	}

	@Override
	public int compareTo(SavedSearch other) {
		return this.options.compareTo(other.options);
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
	public SavedSearch clone() {
		SavedSearch newSearch = new SavedSearch();
		newSearch.setDate(date);
		newSearch.setDescription(description);
		newSearch.setName(name);
		newSearch.setOptions(options);
		newSearch.setTenantId(getTenantId());
		newSearch.setUserId(userId);
		newSearch.setType(type);
		return newSearch;
	}
}