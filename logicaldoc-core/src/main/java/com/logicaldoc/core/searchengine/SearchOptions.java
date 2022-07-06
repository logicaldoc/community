package com.logicaldoc.core.searchengine;

import java.beans.XMLDecoder;
import java.beans.XMLEncoder;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

/**
 * Search options
 * 
 * @author Michael Scholz
 */
public class SearchOptions implements Serializable, Comparable<SearchOptions> {

	private static final long serialVersionUID = 1L;

	public static final int TYPE_FULLTEXT = 0;

	public static final int TYPE_TAG = 1;

	public static final int TYPE_PARAMETRIC = 2;

	public static final int TYPE_FOLDERS = 3;

	protected int page = 1;
	
	protected int maxHits = 40;

	private int type = TYPE_FULLTEXT;

	protected String expression = "";

	protected String name = "";

	protected String description = "";

	protected Object[] parameters = null;

	protected long userId = -1;

	protected String topOperator;

	protected boolean caseSensitive = true;

	protected boolean retrieveAliases = true;

	/**
	 * Optional set of document ids. If specified only documents inside this set
	 * will be returned.
	 */
	protected Set<Long> filterIds = new HashSet<Long>();

	protected Long folderId = null;

	protected boolean searchInSubPath = false;

	protected Long template = null;

	protected Long tenantId = null;

	public Long getTemplate() {
		return template;
	}

	public void setTemplate(Long template) {
		this.template = template;
	}

	public Object[] getParameters() {
		return parameters;
	}

	public void setParameters(Object[] parameters) {
		this.parameters = parameters;
	}

	/**
	 * Creates a new instance of SearchOptions
	 * 
	 * @param type the type of search
	 */
	public SearchOptions(int type) {
		this.type = type;
	}

	/** Necessary constructor for the Search Web Service */
	protected SearchOptions() {
	}

	public void setExpression(String expr) {
		this.expression = expr;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public static SearchOptions read(File file) throws FileNotFoundException, IOException, ClassNotFoundException {
		SearchOptions searchOptions = null;

		// Deserialize from a file (binay format)
		try (ObjectInputStream in = new ObjectInputStream(new FileInputStream(file))) {
			// Deserialize the object
			searchOptions = (SearchOptions) in.readObject();
		} catch (Throwable t) {
			// Now try to deserialize using then XML format
			try (XMLDecoder decoder = new XMLDecoder(new FileInputStream(file))) {
				searchOptions = (SearchOptions) decoder.readObject();
			}
		}
		return searchOptions;
	}

	public void write(File file) throws FileNotFoundException, IOException {
		try (XMLEncoder encoder = new XMLEncoder(new BufferedOutputStream(new FileOutputStream(file)))) {
			encoder.writeObject(this);
		} catch (FileNotFoundException fileNotFound) {
		}
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

	@Override
	public int compareTo(SearchOptions o) {
		return this.getName().compareTo(o.getName());
	}

	public int getMaxHits() {
		return maxHits;
	}

	public void setMaxHits(int maxHits) {
		this.maxHits = maxHits;
	}

	public boolean isFulltext() {
		return getType() == TYPE_FULLTEXT;
	}

	public String getExpression() {
		return expression;
	}

	public long getUserId() {
		return userId;
	}

	public void setUserId(long userId) {
		this.userId = userId;
	}

	public String getTopOperator() {
		return topOperator;
	}

	public void setTopOperator(String topOperator) {
		this.topOperator = topOperator;
	}

	public Set<Long> getFilterIds() {
		return filterIds;
	}

	public void setFilterIds(Set<Long> filterIds) {
		this.filterIds = filterIds;
	}

	public boolean isCaseSensitive() {
		return caseSensitive;
	}

	public void setCaseSensitive(boolean caseSensitive) {
		this.caseSensitive = caseSensitive;
	}

	public Long getFolderId() {
		return folderId;
	}

	public void setFolderId(Long folderId) {
		this.folderId = folderId;
	}

	public boolean isSearchInSubPath() {
		return searchInSubPath;
	}

	public void setSearchInSubPath(boolean searchInSubPath) {
		this.searchInSubPath = searchInSubPath;
	}

	public Long getTenantId() {
		return tenantId;
	}

	public void setTenantId(Long tenantId) {
		this.tenantId = tenantId;
	}

	public boolean isRetrieveAliases() {
		return retrieveAliases;
	}

	public void setRetrieveAliases(boolean retrieveAliases) {
		this.retrieveAliases = retrieveAliases;
	}

	public int getPage() {
		return page;
	}

	public void setPage(int page) {
		this.page = page;
	}
}