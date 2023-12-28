package com.logicaldoc.core.searchengine;

import java.util.List;

import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.task.Task;
import com.logicaldoc.core.task.TaskException;

/**
 * This task optimises all indexes
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.5.0
 */
public class IndexOptimizer extends Task {
	public static final String NAME = "IndexOptimizer";

	@Autowired
	private SearchEngine indexer;

	@Autowired
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

	public void setDocumentDao(DocumentDAO documentDao) {
		this.documentDao = documentDao;
	}
}