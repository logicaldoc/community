package com.logicaldoc.core.searchengine.folder;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import com.logicaldoc.core.searchengine.SearchOptions;

/**
 * Search options specialization for the folder search.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
public class FolderSearchOptions extends SearchOptions {
	private static final long serialVersionUID = 1L;

	private List<FolderCriterion> criteria = new ArrayList<>();

	/**
	 * List of order criteria eg: lastmodified asc, title desc
	 */
	private List<String> order = new ArrayList<>();

	public List<FolderCriterion> getCriteria() {
		return criteria;
	}

	public List<FolderCriterion> getNotEmptyCriteria() {
		return criteria.stream().filter(c -> !c.isEmpty()).collect(Collectors.toList());
	}
	
	public void setCriteria(List<FolderCriterion> criteria) {
		this.criteria = criteria;
	}

	public FolderSearchOptions() {
		super(SearchOptions.TYPE_FOLDERS);
	}

	public List<String> getOrder() {
		return order;
	}

	public void setOrder(List<String> order) {
		this.order = order;
	}
}
