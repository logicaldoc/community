package com.logicaldoc.core.searchengine.folder;

import com.logicaldoc.core.searchengine.SearchOptions;

/**
 * Search options specialization for the folder search.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.4
 */
public class FolderSearchOptions extends SearchOptions {
	private static final long serialVersionUID = 1L;

	private FolderCriterion[] criteria = null;

	/**
	 * List of order criteria eg: lastmodified asc, title desc
	 */
	private String[] order = null;

	public FolderCriterion[] getCriteria() {
		return criteria;
	}

	public void setCriteria(FolderCriterion[] criteria) {
		this.criteria = criteria;
	}

	public FolderSearchOptions() {
		super(SearchOptions.TYPE_FOLDERS);
	}

	public String[] getOrder() {
		return order;
	}

	public void setOrder(String[] order) {
		this.order = order;
	}
}
