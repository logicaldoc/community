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
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import jakarta.persistence.Transient;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import com.logicaldoc.core.security.user.UserHistory;

/**
 * Search options
 * 
 * @author Michael Scholz
 */
public abstract class SearchOptions implements Serializable, Comparable<SearchOptions> {

	private static final long serialVersionUID = 2L;

	public static final int TYPE_FULLTEXT = 0;

	public static final int TYPE_TAG = 1;

	public static final int TYPE_PARAMETRIC = 2;

	public static final int TYPE_FOLDERS = 3;

	private int page = 1;

	private int maxHits = 40;

	private int type = TYPE_FULLTEXT;

	private String expression = "";

	private String name = "";

	private String description = "";

	private transient Map<String, Object> parameters = null;

	private long userId = -1;

	private String topOperator;

	private boolean caseSensitive = true;

	private boolean retrieveAliases = true;

	/**
	 * Optional set of document ids. If specified only documents inside this set
	 * will be returned.
	 */
	private Set<Long> filterIds = new HashSet<>();

	private Long folderId = null;

	private boolean searchInSubPath = false;

	private Long template = null;

	private Long tenantId = null;

	@Transient
	private UserHistory transaction;

	public Long getTemplate() {
		return template;
	}

	public void setTemplate(Long template) {
		this.template = template;
	}

	/**
	 * Creates a new instance of SearchOptions
	 * 
	 * @param type the type of search
	 */
	protected SearchOptions(int type) {
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

	public static SearchOptions read(File file) throws IOException, ClassNotFoundException {
		SearchOptions searchOptions = null;

		// Deserialize from a file (binary format)
		try (ObjectInputStream in = new ObjectInputStream(new FileInputStream(file))) {
			// Deserialize the object
			searchOptions = (SearchOptions) in.readObject();
		} catch (Exception t) {
			// Now try to deserialize using then XML format
			try (XMLDecoder decoder = new XMLDecoder(new FileInputStream(file))) {
				searchOptions = (SearchOptions) decoder.readObject();
			}
		}
		return searchOptions;
	}

	public void write(File file) throws IOException {
		try (XMLEncoder encoder = new XMLEncoder(new BufferedOutputStream(new FileOutputStream(file)))) {
			encoder.writeObject(this);
		} catch (FileNotFoundException fileNotFound) {
			// Nothing to do
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

	public Map<String, Object> getParameters() {
		return parameters;
	}

	public void setParameters(Map<String, Object> parameters) {
		this.parameters = parameters;
	}

	public UserHistory getTransaction() {
		return transaction;
	}

	public void setTransaction(UserHistory transaction) {
		this.transaction = transaction;
	}

	@Override
	public int compareTo(SearchOptions o) {
		return this.getName().compareTo(o.getName());
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof SearchOptions))
			return false;
		SearchOptions other = (SearchOptions) obj;
		return getName().equals(other.getName());
	}

	@Override
	public int hashCode() {
		return getName().hashCode();
	}

	@Override
	public String toString() {
		return this.getClass().getSimpleName().replace("Options", "")
				+ new ReflectionToStringBuilder(this, ToStringStyle.NO_CLASS_NAME_STYLE) {
					@Override
					protected boolean accept(Field field) {
						try {
							Object value = field.get(getObject());
							// avoid to print null values and empty collections
							return super.accept(field) && !field.getName().equals("name")
									&& !field.getName().equals("description") && !field.getName().equals("transaction")
									&& value != null && StringUtils.isNotEmpty(value.toString())
									&& (!field.getType().isArray() || Array.getLength(value) > 0)
									&& (!Collection.class.isAssignableFrom(field.getType()) || Boolean.FALSE
											.equals(value.getClass().getDeclaredMethod("isEmpty").invoke(value)));
						} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException
								| NoSuchMethodException | SecurityException e) {
							return false;
						}
					}
				}.toString();
	}
}