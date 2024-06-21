package com.logicaldoc.core.searchengine.saved;

import java.beans.XMLDecoder;
import java.beans.XMLEncoder;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.Date;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.searchengine.SearchOptions;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;

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

	public void saveOptions(SearchOptions opt) throws IOException, PersistenceException {
		this.setType(opt.getType());

		TenantDAO tenantDao = (TenantDAO) Context.get().getBean(TenantDAO.class);
		String tenantName = tenantDao.getTenantName(getTenantId());
		String charset = Context.get().getProperties().getProperty(tenantName + ".charset", "UTF-8");

		File tmpFile = FileUtil.createTempFile("ser", ".txt");
		try (OutputStream out = new FileOutputStream(tmpFile);
				XMLEncoder encoder = new XMLEncoder(out, charset, false, 0)) {
			encoder.writeObject(opt);
		} catch (IOException ioe) {
			FileUtil.delete(tmpFile);
			throw ioe;
		}

		try {
			setOptions(FileUtil.readFile(tmpFile).trim());
		} finally {
			FileUtil.delete(tmpFile);
		}
	}

	public SearchOptions readOptions() {
		try (XMLDecoder decoder = new XMLDecoder(new ByteArrayInputStream(getOptions().getBytes()))) {
			SearchOptions searchOptions = (SearchOptions) decoder.readObject();
			searchOptions.setName(getName());
			searchOptions.setDescription(getDescription());
			return searchOptions;
		}
	}

	@Override
	public int compareTo(SavedSearch other) {
		return this.getOptions().compareTo(other.getOptions());
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof SavedSearch))
			return false;
		SavedSearch other = (SavedSearch) obj;
		return getOptions().equals(other.getOptions());
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
}