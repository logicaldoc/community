package com.logicaldoc.core.searchengine.indexer;

import com.logicaldoc.core.task.DocumentProcessorStats;

/**
 * Statistics of an indexing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class IndexerStats extends DocumentProcessorStats {

	private long indexingTime = 0;

	private long parsingTime = 0;

	public IndexerStats() {
		super();
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