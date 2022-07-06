package com.logicaldoc.core.searchengine;

import java.util.List;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.task.Task;

/**
 * This task optimises all indexes
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.5.0
 */
public class IndexOptimizer extends Task {
	public static final String NAME = "IndexOptimizer";

	private SearchEngine indexer;

	private DocumentDAO documentDao;

	public IndexOptimizer() {
		super(NAME);
		log = LoggerFactory.getLogger(IndexOptimizer.class);
	}

	public SearchEngine getIndexer() {
		return indexer;
	}

	public void setIndexer(SearchEngine indexer) {
		this.indexer = indexer;
	}

	@Override
	protected void runTask() throws Exception {
		if (indexer.isLocked()) {
			log.warn("Index locked, skipping optimization");
			return;
		}

		log.info("Start index optimization");
		try {
			deleteOrphaned();
			indexer.optimize();
		} finally {
			indexer.unlock();
		}
		log.info("End of index optimization");
	}

	/**
	 * Removes from index all documents deleted in the database
	 */
	private void deleteOrphaned() {
		List<Long> ids = documentDao.findDeletedDocIds();
		indexer.deleteHits(ids);
	}

	@Override
	public boolean isIndeterminate() {
		return true;
	}

	@Override
	public boolean isConcurrent() {
		return true;
	}

	public void setDocumentDao(DocumentDAO documentDao) {
		this.documentDao = documentDao;
	}
}