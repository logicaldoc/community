package com.logicaldoc.core.searchengine.indexer;

/**
 * Statistics of an indexing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class IndexerStats {

	private long indexed = 0;

	private long errors = 0;

	private long indexingTime = 0;

	private long parsingTime = 0;

	public IndexerStats() {
		super();
	}

	public long getIndexed() {
		return indexed;
	}

	public void setIndexed(long indexed) {
		this.indexed = indexed;
	}

	public long getErrors() {
		return errors;
	}

	public void setErrors(long errors) {
		this.errors = errors;
	}

	public long getIndexingTime() {
		return indexingTime;
	}

	public void setIndexingTime(long indexingTime) {
		this.indexingTime = indexingTime;
	}

	public long getParsingTime() {
		return parsingTime;
	}

	public void setParsingTime(long parsingTime) {
		this.parsingTime = parsingTime;
	}
}