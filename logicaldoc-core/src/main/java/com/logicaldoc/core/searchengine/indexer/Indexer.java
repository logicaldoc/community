package com.logicaldoc.core.searchengine.indexer;

import java.util.Date;
import java.util.List;

import org.slf4j.Logger;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.document.IndexingStatus;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
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

	private static final String DEFAULT_USERNAME = "_system";

	private UserDAO userDao;

	private ContextProperties config;

	private DocumentDAO documentDao;

	private TenantDAO tenantDao;

	private DocumentManager documentManager;

	Indexer(List<Long> docIds, IndexerTask task, Logger log) {
		super(docIds, task, log);
		this.config = Context.get().getProperties();
		this.userDao = Context.get(UserDAO.class);
		this.documentDao = Context.get(DocumentDAO.class);
		this.tenantDao = Context.get(TenantDAO.class);
		this.documentManager = Context.get(DocumentManager.class);
	}

	@Override
	public IndexerStats call() throws Exception {
		try {
			/*
			 * Prepare the master transaction object
			 */
			DocumentHistory transaction = new DocumentHistory();
			transaction.setUser(userDao.findByUsername(DEFAULT_USERNAME));

			log.info("Indexing {} documents", docIds.size());
			for (Long id : docIds) {
				try {
					log.debug("Indexing document {}", id);

					Document doc = documentDao.findById(id);
					Tenant tenant = tenantDao.findById(doc.getTenantId());

					// Check if this document must be marked for skipping
					if (!FileUtil.matches(doc.getFileName(),
							config.getProperty(tenant.getName() + ".index.includes") == null ? ""
									: config.getProperty(tenant.getName() + ".index.includes"),
							config.getProperty(tenant.getName() + ".index.excludes") == null ? ""
									: config.getProperty(tenant.getName() + ".index.excludes"))) {
						documentDao.initialize(doc);
						doc.setIndexingStatus(IndexingStatus.SKIP);
						documentDao.store(doc);
						log.warn("Document {} with filename '{}' marked as unindexable", id, doc.getFileName());
					} else {
						Date beforeIndexing = new Date();
						increaseParsingTime(documentManager.index(id, null, new DocumentHistory(transaction)));
						long indexingDiff = TimeDiff.getTimeDifference(beforeIndexing, new Date(),
								TimeField.MILLISECOND);
						increaseIndexingTime(indexingDiff);
						log.debug("Indexed document {} in {}ms", doc, indexingDiff);
					}
					increaseIndexed();
				} catch (Exception e) {
					log.error("There was a problem indexing document {}", id);
					log.error(e.getMessage(), e);
					increaseErrors();
				} finally {
					((IndexerTask) task).onDocumentProcessed();
				}

				if (interrupt) {
					log.debug("Interrupt requested");
					break;
				}
			}

			log.info("Completed");
			return stats;
		} finally {
			completed = true;
		}
	}

	private void increaseErrors() {
		stats.setErrors(stats.getErrors() + 1);
	}

	private void increaseIndexed() {
		stats.setProcessed(stats.getProcessed() + 1);
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
	protected void processDocument(Document document, User user) {
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
				log.debug("Indexed document {} in {}ms", document, indexingDiff);
			}
			increaseIndexed();
		} catch (Exception e) {
			increaseErrors();
			log.error("There was a problem indexing document {}", document);
			log.error(e.getMessage(), e);
		}
	}

	@Override
	protected IndexerStats prepareStats() {
		return new IndexerStats();
	}
}