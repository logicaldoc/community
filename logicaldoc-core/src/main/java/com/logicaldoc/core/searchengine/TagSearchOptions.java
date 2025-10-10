package com.logicaldoc.core.searchengine;

/**
 * Search options specialization for the Tag text search.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.2
 */
public class TagSearchOptions extends SearchOptions {

	private static final long serialVersionUID = 1L;

	public TagSearchOptions() {
		super(SearchOptions.TYPE_TAG);
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof TagSearchOptions))
			return false;
		TagSearchOptions other = (TagSearchOptions) obj;
		return getName().equals(other.getName());
	}

	@Override
	public int hashCode() {
		return getName().hashCode();
	}
}