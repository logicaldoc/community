package com.logicaldoc.core.searchengine;

import java.util.List;

import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.task.Task;
import com.logicaldoc.core.task.TaskException;

/**
 * This task optimizes all indexes
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.5.0
 */
@Component("indexOptimizer")
public class IndexOptimizer extends Task {
	public static final String NAME = "IndexOptimizer";

	private SearchEngine indexer;

	private DocumentDAO documentDao;

	@Autowired
	public IndexOptimizer(SearchEngine indexer, DocumentDAO documentDao) {
		super(NAME);
		log = LoggerFactory.getLogger(IndexOptimizer.class);
		this.indexer = indexer;
		this.documentDao = documentDao;
	}

	public SearchEngine getIndexer() {
		return indexer;
	}

	@Override
	protected void runTask() throws TaskException {
		if (indexer.isLocked()) {
			log.warn("Index locked, skipping optimization");
			return;
		}

		log.info("Start index optimization");
		try {
			deleteOrphaned();
			indexer.optimize();
		} catch (PersistenceException e) {
			throw new TaskException(e.getMessage(), e);
		} finally {
			indexer.unlock();
		}
		log.info("End of index optimization");
	}

	/**
	 * Removes from index all documents deleted in the database
	 * 
	 * @throws PersistenceException error at data layer
	 */
	private void deleteOrphaned() throws PersistenceException {
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
}