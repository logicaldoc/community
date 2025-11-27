package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentGridUtil;
import com.logicaldoc.gui.frontend.client.search.SearchObserver;

/**
 * Search options
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUISearchOptions implements Serializable {
	private static final long serialVersionUID = 1L;

	public static final int TYPE_FULLTEXT = 0;

	public static final int TYPE_TAGS = 1;

	public static final int TYPE_PARAMETRIC = 2;

	public static final int TYPE_FOLDERS = 3;
	
	public static final int TYPE_SEMANTIC = 4;

	private int maxHits = 40;

	private int type = TYPE_FULLTEXT;

	private String expression = "";

	private String expressionLanguage = "en";

	// Min size in bytes
	private Long sizeMin = null;

	// Max size in bytes
	private Long sizeMax = null;

	private String format = "";

	private boolean searchInSubPath = false;

	private int depth = 1;

	private long userId = -1;

	private Long folder = null;

	private String language = null;

	private Date dateFrom = null;

	private Date dateTo = null;

	private Date creationFrom = null;

	private Date creationTo = null;

	private Long template = null;

	private String name = "";

	private String description = "";

	private String folderName;

	private String topOperator;

	private boolean caseSensitive = true;

	private boolean retrieveAliases = false;

	private List<Long> filterIds = new ArrayList<>();

	private List<GUICriterion> criteria = new ArrayList<>();

	private List<String> fields = Constants.getFulltextDefaultFields();

	// The eventual observer that originated such search options
	private transient SearchObserver source;

	/** Creates a new instance of SearchOptions */
	public GUISearchOptions() {
		if (Session.get() != null && Session.get().getSession() != null) {
			Integer pageSize = null;
			if (Session.get().getUser() != null)
				pageSize = DocumentGridUtil.getPageSizeFromSpec(Session.get().getUser().getHitsGrid());
			if (pageSize == null)
				pageSize = Session.get().getConfigAsInt("search.hits");
			maxHits = pageSize;
		}
	}

	public Long getTemplate() {
		return template;
	}

	public void setTemplate(Long template) {
		this.template = template;
	}

	public String getExpression() {
		return expression;
	}

	public String getFormat() {
		return format;
	}

	public long getUserId() {
		return userId;
	}

	public List<String> getFields() {
		return fields;
	}

	public void setExpression(String expression) {
		this.expression = expression;
	}

	public void setFormat(String form) {
		format = form;
	}

	public void setUserId(long userId) {
		this.userId = userId;
	}

	public void setFields(List<String> flds) {
		fields = flds;
	}

	public void addField(String s) {
		fields.add(s);
	}

	public String getLanguage() {
		return language;
	}

	public void setLanguage(String language) {
		this.language = language;
	}

	public Date getDateTo() {
		return dateTo;
	}

	public void setDateTo(Date dateTo) {
		this.dateTo = dateTo;
	}

	public Long getSizeMin() {
		return sizeMin;
	}

	public void setSizeMin(Long sizeMin) {
		this.sizeMin = sizeMin;
	}

	public Long getSizeMax() {
		return sizeMax;
	}

	public void setSizeMax(Long sizeMax) {
		this.sizeMax = sizeMax;
	}

	public boolean isSearchInSubPath() {
		return searchInSubPath;
	}

	public void setSearchInSubPath(boolean searchInSubPath) {
		this.searchInSubPath = searchInSubPath;
	}

	public Date getCreationTo() {
		return creationTo;
	}

	public void setCreationTo(Date creationTo) {
		this.creationTo = creationTo;
	}

	public Date getCreationFrom() {
		return creationFrom;
	}

	public void setCreationFrom(Date creationFrom) {
		this.creationFrom = creationFrom;
	}

	public Date getDateFrom() {
		return dateFrom;
	}

	public void setDateFrom(Date dateFrom) {
		this.dateFrom = dateFrom;
	}

	public Long getFolder() {
		return folder;
	}

	public void setFolder(Long folder) {
		this.folder = folder;
		if (folder == null)
			folderName = null;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public String getExpressionLanguage() {
		return expressionLanguage;
	}

	public void setExpressionLanguage(String queryLanguage) {
		this.expressionLanguage = queryLanguage;
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

	public String getFolderName() {
		return folderName;
	}

	public void setFolderName(String folderName) {
		this.folderName = folderName;
	}

	public List<GUICriterion> getCriteria() {
		return criteria;
	}

	public GUICriterion getCriterion(String field) {
		return criteria.stream().filter(c -> c.getField().equals(field)).findFirst().orElse(null);
	}

	public void setCriteria(List<GUICriterion> criteria) {
		this.criteria = criteria;
	}

	public String getTopOperator() {
		return topOperator;
	}

	public void setTopOperator(String topOperator) {
		this.topOperator = topOperator;
	}

	public List<Long> getFilterIds() {
		return filterIds;
	}

	public void setFilterIds(List<Long> filterIds) {
		this.filterIds = filterIds;
	}

	public int getDepth() {
		return depth;
	}

	public void setDepth(int depth) {
		this.depth = depth;
	}

	public SearchObserver getSource() {
		return source;
	}

	public void setSource(SearchObserver source) {
		this.source = source;
	}

	public boolean isCaseSensitive() {
		return caseSensitive;
	}

	public void setCaseSensitive(boolean caseSensitive) {
		this.caseSensitive = caseSensitive;
	}

	public boolean isRetrieveAliases() {
		return retrieveAliases;
	}

	public void setRetrieveAliases(boolean retrieveAliases) {
		this.retrieveAliases = retrieveAliases;
	}	
}
