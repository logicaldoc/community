package com.logicaldoc.core.searchengine.indexer;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

import javax.annotation.Resource;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentStatus;
import com.logicaldoc.core.document.IndexingStatus;
import com.logicaldoc.core.searchengine.SearchEngine;
import com.logicaldoc.core.task.Task;
import com.logicaldoc.core.task.TaskException;
import com.logicaldoc.core.threading.ThreadPools;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.CollectionUtil;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.concurrent.SerialFuture;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.time.TimeDiff;

/**
 * This task enlists all non-indexed documents and performs the indexing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
@Component("indexerTask")
public class IndexerTask extends Task {

	public static final String NAME = "IndexerTask";

	@Resource(name = "documentDAO")
	protected DocumentDAO documentDao;

	@Resource(name = "SearchEngine")
	protected SearchEngine searchEngine;

	private long totalIndexedDocuments = 0;

	private long totalErrors = 0;

	private long totalIndexingTime = 0;

	private long totalParsingTime = 0L;

	private List<Indexer> threads = new ArrayList<>();

	public IndexerTask() {
		super(NAME);
		log = LoggerFactory.getLogger(IndexerTask.class);
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
	public synchronized void interrupt() {
		super.interrupt();
		for (Indexer thread : threads)
			thread.interrupt();
	}

	void onDocumentProcessed() {
		next();
	}

	@Override
	protected void runTask() throws TaskException {

		if (searchEngine.isLocked()) {
			log.warn("Index locked, skipping indexing");
			return;
		}

		if (!lockManager.get(getName(), transactionId)) {
			log.warn("Unable to acquire lock {}, skipping indexing", getName());
			return;
		}

		log.info("Start indexing of all documents");

		totalErrors = 0;
		totalIndexedDocuments = 0;
		totalIndexingTime = 0;
		totalParsingTime = 0;
		try {
			Integer max = getMax();

			setSize(max);

			ContextProperties config = Context.get().getProperties();

			// Retrieve the actual transactions
			String transactionIdsStr = lockManager.getAllTransactions().stream().map(t -> "'" + t + "'")
					.collect(Collectors.joining(","));

			// First of all find documents to be indexed and not already
			// involved into a transaction
			String[] query = IndexerTask.prepareQuery();
			List<Long> docIds = documentDao.findIdsByWhere(
					query[0] + " and (" + PersistentObjectDAO.ENTITY + ".transactionId is null or "
							+ PersistentObjectDAO.ENTITY + ".transactionId not in (" + transactionIdsStr + "))",
					query[1], max);
			size = docIds.size();
			log.info("Found a total of {} documents to be processed", size);

			// Must take into account start and end of the transaction
			size += 2;

			// Mark all the documents as belonging to the current
			// transaction. This may require time
			assignTransaction(docIds);

			// First step done
			next();

			// Now we can release the lock
			lockManager.release(getName(), transactionId);

			if (!docIds.isEmpty()) {
				int threadsTotal = config.getInt("threadpool." + NAME + ".max", 1);
				log.info("Distribute the indexing among {} threads", threadsTotal);

				// Divide the documents in segments of N
				Collection<List<Long>> segments = CollectionUtil.partition(docIds,
						(int) Math.ceil((double) docIds.size() / (double) threadsTotal));

				// Prepare the threads and launch them
				List<Future<IndexerStats>> futures = new ArrayList<>();
				for (List<Long> segment : segments) {
					Indexer indexer = new Indexer(segment, this, log);
					threads.add(indexer);
					futures.add(Context.get(ThreadPools.class).schedule(indexer, NAME, 1));
					log.debug("Launched the indexer for documents {}", segment);
				}

				// Wait for the completion of all the indexers
				waitForIndexersCompletion(futures);

				log.info("All indexers have completed");
			}
		} catch (PersistenceException e) {
			throw new TaskException(e.getMessage(), e);
		} finally {
			if (log.isInfoEnabled()) {
				log.info("Indexing finished");
				log.info("Indexing time: {}", TimeDiff.printDuration(totalIndexingTime));
				log.info("Parsing time: {}", TimeDiff.printDuration(totalParsingTime));
				log.info("Indexed documents: {}", totalIndexedDocuments);
				log.info("Errors: {}", totalErrors);
			}

			searchEngine.unlock();

			try {
				// To be safer always release the lock
				lockManager.release(getName(), transactionId);

				// Remove the transaction reference
				Map<String, Object> params = new HashMap<>();
				params.put("transactionId", transactionId);

				documentDao.jdbcUpdate(
						"update ld_document set ld_transactionid = null where ld_transactionId = :transactionId",
						params);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}

			// Last step done
			next();
			if (log.isInfoEnabled())
				log.info("Documents released from transaction {}", transactionId);
		}
	}

	/**
	 * Stays waiting for the completion of all the indexers, collecting the
	 * total time spent in indexing.
	 * 
	 * @param futures The futures containing the stats from each one of the
	 *        running indexers
	 */
	protected void waitForIndexersCompletion(List<Future<IndexerStats>> futures) {
		try {
			List<IndexerStats> stats = new SerialFuture<IndexerStats>(futures).getAll();
			for (IndexerStats stat : stats) {
				totalErrors += stat.getErrors();
				totalIndexedDocuments += stat.getIndexed();
				totalIndexingTime += stat.getIndexingTime();
				totalParsingTime += stat.getParsingTime();
			}
		} catch (ExecutionException e) {
			log.error(e.getMessage(), e);
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
		}
	}

	private void assignTransaction(List<Long> docIds) throws PersistenceException {
		if (!docIds.isEmpty()) {
			// Mark all these documents as belonging to the current
			// transaction. This may require time
			String idsStr = docIds.stream().map(id -> Long.toString(id)).collect(Collectors.joining(","));

			Map<String, Object> params = new HashMap<>();
			params.put("transactionId", transactionId);

			documentDao.jdbcUpdate(
					" update ld_document set ld_transactionid = :transactionId where ld_transactionid is null and ld_id in ("
							+ idsStr + ")",
					params);
		}
		log.info("Documents marked for indexing in transaction {}", transactionId);
	}

	private Integer getMax() {
		Integer max = Context.get().getProperties().getProperty("index.batch") != null
				? Integer.parseInt(config.getProperty("index.batch"))
				: null;
		if (max != null && max.intValue() < 1)
			max = null;
		return max;
	}

	private void setSize(Integer max) {
		if (max != null && max.intValue() < size && max.intValue() > 0)
			size = max.intValue();
	}

	/**
	 * Prepares the query to search the documents to be indexed
	 * 
	 * @return array composed by the query to execute and the sorting expression
	 */
	public static String[] prepareQuery() {
		ContextProperties config = Context.get().getProperties();

		// Determine the sorting
		String sorting = config.getProperty("index.sorting");
		if (StringUtils.isNotEmpty(sorting)) {
			if ("oldestfirst".equals(sorting))
				sorting = PersistentObjectDAO.ENTITY + ".date asc";
			else if ("mostrecentfirst".equals(sorting))
				sorting = PersistentObjectDAO.ENTITY + ".date desc";
			else if ("smallestfirst".equals(sorting))
				sorting = PersistentObjectDAO.ENTITY + ".fileSize asc";
			else
				sorting = PersistentObjectDAO.ENTITY + ".fileSize desc";
		}

		// This hidden setting overrides the default sorting policy(some really
		// demanding users may need this optimization).
		String sortingCustom = config.getProperty("index.sorting.custom");
		if (StringUtils.isNotEmpty(sortingCustom))
			sorting = sortingCustom;

		String where = PersistentObjectDAO.ENTITY + ".deleted = 0 and (" + PersistentObjectDAO.ENTITY
				+ ".indexingStatus = " + IndexingStatus.TO_INDEX.ordinal() + " or " + PersistentObjectDAO.ENTITY
				+ ".indexingStatus = " + IndexingStatus.TO_INDEX_METADATA.ordinal() + ") and not "
				+ PersistentObjectDAO.ENTITY + ".status = " + DocumentStatus.ARCHIVED.ordinal();

		return new String[] { where, sorting };
	}

	@Override
	protected String prepareReport(Locale locale) {
		StringBuilder sb = new StringBuilder();
		sb.append(I18N.message("indexationtime", locale) + ": ");
		sb.append(TimeDiff.printDuration(totalIndexingTime));
		sb.append("\n");
		sb.append(I18N.message("indexeddocs", locale) + ": ");
		sb.append(totalIndexedDocuments);
		sb.append("\n");
		sb.append(I18N.message("errors", locale) + ": ");
		sb.append(totalErrors);
		return sb.toString();
	}
}