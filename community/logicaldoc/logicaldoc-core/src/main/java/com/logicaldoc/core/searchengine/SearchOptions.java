package com.logicaldoc.core.searchengine;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
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

	protected int maxHits = 40;

	private int type = TYPE_FULLTEXT;

	protected String expression = "";

	protected String name = "";

	protected String description = "";

	protected Object[] parameters = null;

	protected long userId = -1;

	protected String topOperator;

	protected boolean caseSensitive = true;

	protected int retrieveAliases = 0;

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

	/** Creates a new instance of SearchOptions */
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
		// Deserialize from a file
		ObjectInputStream in = new ObjectInputStream(new FileInputStream(file));
		try {
			// Deserialize the object
			searchOptions = (SearchOptions) in.readObject();
		} finally {
			in.close();
		}
		return searchOptions;
	}

	public void write(File file) throws FileNotFoundException, IOException {
		// Serialize to a file
		ObjectOutput out = new ObjectOutputStream(new FileOutputStream(file));
		try {
			out.writeObject(this);
		} finally {
			out.flush();
			out.close();
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

	public int getRetrieveAliases() {
		return retrieveAliases;
	}

	public void setRetrieveAliases(int retrieveAliases) {
		this.retrieveAliases = retrieveAliases;
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
}