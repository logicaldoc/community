package com.logicaldoc.core.searchengine;

import java.util.Date;
import java.util.List;
import java.util.Locale;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.core.task.Task;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.time.TimeDiff;
import com.logicaldoc.util.time.TimeDiff.TimeField;

/**
 * This task enlists all non-indexed documents and performs the indexing
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 4.0
 */
public class IndexerTask extends Task {
	public static final String NAME = "IndexerTask";

	private DocumentManager documentManager;

	private DocumentDAO documentDao;

	private TenantDAO tenantDao;

	private SearchEngine indexer;

	private long indexed = 0;

	private long errors = 0;

	private long indexingTime = 0;

	private long parsingTime = 0;

	public IndexerTask() {
		super(NAME);
		log = LoggerFactory.getLogger(IndexerTask.class);
	}

	public void setDocumentManager(DocumentManager documentManager) {
		this.documentManager = documentManager;
	}

	public void setDocumentDao(DocumentDAO documentDao) {
		this.documentDao = documentDao;
	}

	@Override
	public boolean isIndeterminate() {
		return false;
	}

	@Override
	public boolean isConcurrent() {
		return true;
	}

	@Override
	protected void runTask() throws Exception {
		if (indexer.isLocked()) {
			log.warn("Index locked, skipping indexing");
			return;
		}

		if (!lockManager.get(getName(), transactionId)) {
			log.warn("Unable to acquire lock " + getName() + ", skipping indexing");
			return;
		}

		log.info("Start indexing of all documents");

		errors = 0;
		indexed = 0;
		indexingTime = 0;
		try {
			ContextProperties config = Context.get().getProperties();
			Integer max = config.getProperty("index.batch") != null ? new Integer(config.getProperty("index.batch"))
					: null;

			if (max != null && max.intValue() < size && max.intValue() > 0)
				size = max.intValue();

			if (max != null && max.intValue() < 1)
				max = null;

			// Retrieve the actual transactions
			List<String> transactionIds = lockManager.getAllTransactions();
			String transactionIdsStr = transactionIds.toString().replace("[", "('").replace("]", "')")
					.replace(", ", "','");

			// First of all find documents to be indexed and not already
			// involved into a transaction
			List<Long> ids = documentDao.findIdsByWhere("_entity.indexed = " + AbstractDocument.INDEX_TO_INDEX
					+ " and (_entity.transactionId is null or _entity.transactionId not in " + transactionIdsStr
					+ ") and not _entity.status=" + AbstractDocument.DOC_ARCHIVED, null, max);
			size = ids.size();
			log.info("Found a total of " + size + " documents to be processed");

			// Must take into account start and end of the transaction
			size += 2;

			if (!ids.isEmpty()) {
				// Mark all these documents as belonging to the current
				// transaction. This may require time
				String idsStr = ids.toString().replace('[', '(').replace(']', ')');
				documentDao.bulkUpdate(" set ld_transactionid = ?1 where ld_transactionid is null and ld_id in "
						+ idsStr, new Object[] { transactionId });
			}
			log.info("Documents marked for indexing in transaction " + transactionId);

			// First step done
			next();

			// Now we can release the lock
			lockManager.release(getName(), transactionId);

			for (Long id : ids) {
				try {
					log.debug("Indexing document " + id);

					Document doc = documentDao.findById(id);
					Tenant tenant = tenantDao.findById(doc.getTenantId());

					// Check if this document must be marked for skipping
					if (!FileUtil.matches(
							doc.getFileName(),
							config.getProperty(tenant.getName() + ".index.includes") == null ? "" : config
									.getProperty(tenant.getName() + ".index.includes"),
							config.getProperty(tenant.getName() + ".index.excludes") == null ? "" : config
									.getProperty(tenant.getName() + ".index.excludes"))) {
						documentDao.initialize(doc);
						doc.setIndexed(Document.INDEX_SKIP);
						documentDao.store(doc);
						log.warn("Document {} with filename '{}' maked as unindexable ", id, doc.getFileName());
					} else {
						Date beforeIndexing = new Date();
						parsingTime += documentManager.reindex(id, null);
						indexingTime += TimeDiff.getTimeDifference(beforeIndexing, new Date(), TimeField.MILLISECOND);
						log.debug("Indexed document " + id);
					}
					indexed++;
				} catch (Throwable e) {
					log.error("There was a problem indexing document {}", id);
					log.error(e.getMessage(), e);
					errors++;
				} finally {
					next();
				}
				if (interruptRequested)
					return;
			}
		} finally {
			log.info("Indexing finished");
			log.info("Indexing time: " + TimeDiff.printDuration(indexingTime));
			log.info("Parsing time: " + TimeDiff.printDuration(parsingTime));
			log.info("Indexed documents: " + indexed);
			log.info("Errors: " + errors);

			indexer.unlock();

			// To be safer always release the lock
			lockManager.release(getName(), transactionId);

			// Remove the transaction reference
			documentDao.bulkUpdate("set ld_transactionid = null where ld_transactionId = ?1",
					new Object[] { transactionId });

			// Last step done
			next();
			log.info("Documents released from transaction " + transactionId);
		}
	}

	public void setIndexer(SearchEngine indexer) {
		this.indexer = indexer;
	}

	@Override
	protected String prepareReport(Locale locale) {
		StringBuffer sb = new StringBuffer();
		sb.append(I18N.message("indexationtime", locale) + ": ");
		sb.append(TimeDiff.printDuration(indexingTime));
		sb.append("\n");
		sb.append(I18N.message("indexeddocs", locale) + ": ");
		sb.append(indexed);
		sb.append("\n");
		sb.append(I18N.message("errors", locale) + ": ");
		sb.append(errors);
		return sb.toString();
	}

	public void setTenantDao(TenantDAO tenantDao) {
		this.tenantDao = tenantDao;
	}
}