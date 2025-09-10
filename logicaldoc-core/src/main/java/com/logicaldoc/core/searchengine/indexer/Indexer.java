package com.logicaldoc.core.searchengine.indexer;

import java.io.IOException;
import java.util.Date;
import java.util.List;

import org.slf4j.Logger;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.document.IndexingStatus;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.task.DocumentProcessorCallable;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.spring.Context;
import com.logicaldoc.util.time.TimeDiff;
import com.logicaldoc.util.time.TimeDiff.TimeField;

/**
 * A {@link DocumentProcessorCallable} to index a segment of documents
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.1
 */
class Indexer extends DocumentProcessorCallable<IndexerStats> {

	private ContextProperties config;

	private DocumentDAO documentDao;

	private TenantDAO tenantDao;

	private DocumentManager documentManager;

	Indexer(List<Long> docIds, IndexerTask task, Logger log) {
		super(docIds, task, log);
		this.config = Context.get().getProperties();
		this.documentDao = Context.get(DocumentDAO.class);
		this.tenantDao = Context.get(TenantDAO.class);
		this.documentManager = Context.get(DocumentManager.class);
	}

	private void increaseParsingTime(long increase) {
		stats.setParsingTime(stats.getParsingTime() + increase);
	}

	private void increaseIndexingTime(long increase) {
		stats.setIndexingTime(stats.getIndexingTime() + increase);
	}

	public boolean isCompleted() {
		return completed;
	}

	public IndexerStats getStats() {
		return stats;
	}

	@Override
	protected void processDocument(Document document, User user) throws PersistenceException, IOException {
		try {
			Tenant tenant = tenantDao.findById(document.getTenantId());

			// Check if this document must be marked for skipping
			if (!FileUtil.matches(document.getFileName(),
					config.getProperty(tenant.getName() + ".index.includes") == null ? ""
							: config.getProperty(tenant.getName() + ".index.includes"),
					config.getProperty(tenant.getName() + ".index.excludes") == null ? ""
							: config.getProperty(tenant.getName() + ".index.excludes"))) {
				documentDao.initialize(document);
				document.setIndexingStatus(IndexingStatus.SKIP);
				documentDao.store(document);
				log.warn("Document {} with filename '{}' marked as unindexable", document.getId(),
						document.getFileName());
			} else {
				Date beforeIndexing = new Date();
				DocumentHistory transaction = new DocumentHistory();
				transaction.setUser(user);

				increaseParsingTime(documentManager.index(document.getId(), null, transaction));
				long indexingDiff = TimeDiff.getTimeDifference(beforeIndexing, new Date(), TimeField.MILLISECOND);
				increaseIndexingTime(indexingDiff);
				if (log.isDebugEnabled())
					log.debug("Indexed document {} in {}ms", document, indexingDiff);
			}
		} catch (Exception e) {
			throw new IOException("Problem indexing document " + document, e);
		}
	}

	@Override
	protected IndexerStats prepareStats() {
		return new IndexerStats();
	}
}