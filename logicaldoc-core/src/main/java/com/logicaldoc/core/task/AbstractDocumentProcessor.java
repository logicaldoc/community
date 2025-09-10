package com.logicaldoc.core.task;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.threading.ThreadPools;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.CollectionUtil;
import com.logicaldoc.util.concurrent.LaxSerialFuture;
import com.logicaldoc.util.spring.Context;

import jakarta.annotation.Resource;

/**
 * A base implementation for those tasks that process documents
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.3
 */
public abstract class AbstractDocumentProcessor extends Task {

	private List<DocumentProcessorCallable<? extends DocumentProcessorStats>> threads = new ArrayList<>();

	protected int processed = 0;

	protected int errors = 0;

	@Resource(name = "documentDAO")
	protected DocumentDAO documentDao;

	protected AbstractDocumentProcessor(String name) {
		super(name);
	}

	@Override
	public void runTask() throws TaskException {
		log.info("Start processing of all documents");
		errors = 0;
		processed = 0;
		threads.clear();
		try {
			int max = getBatchSize();

			// Retrieve the actual transactions
			String currentTransactionIds = lockManager.getAllTransactions().stream().map(t -> "'" + t + "'")
					.collect(Collectors.joining(","));

			// Select the IDs of those documents not involved in any transaction
			StringBuilder where = new StringBuilder(PersistentObjectDAO.ENTITY);
			where.append(".deleted = 0 and (");
			where.append(PersistentObjectDAO.ENTITY);
			where.append(".transactionId is null or ");
			where.append(PersistentObjectDAO.ENTITY);
			where.append(".transactionId not in (");
			where.append(StringUtils.defaultString(currentTransactionIds, "'unexisting'"));
			where.append(")) and ");

			StringBuilder sort = new StringBuilder();

			prepareQueueQuery(where, sort);

			List<Long> ids = documentDao.findIdsByWhere(where.toString(), sort.toString(), max);
			getSize(max, ids);

			if (size > 0)
				processDocuments(ids, max);
		} catch (PersistenceException e) {
			throw new TaskException(e.getMessage(), e);
		} finally {
			log.info("Processing completed");
			log.info("Processed documents: {}", processed);
			log.info("Errors: {}", errors);

			removeTransactionReference();
		}
	}

	protected void processDocuments(List<Long> docIds, int max) throws PersistenceException {
		String idsStr = "(" + docIds.stream().map(id -> Long.toString(id)).collect(Collectors.joining(",")) + ")";

		// Mark all these documents as belonging to the current
		// transaction
		int count = documentDao
				.jdbcUpdate("update ld_document set ld_transactionid='" + transactionId + "' where ld_id in " + idsStr);
		if (log.isInfoEnabled())
			log.info("{} documents assigned to transaction {}", count, transactionId);

		List<Object[]> records = documentDao
				.findByQuery("select A.id, A.fileName from Document A where A.id in " + idsStr, null, null);

		if (!records.isEmpty()) {
			int threadsTotal = config.getInt("threadpool." + getName() + ".max", 1);
			log.info("Distribute the processing among {} threads", threadsTotal);

			// Divide the documents in segments of N
			Collection<List<Long>> segments = CollectionUtil.partition(docIds,
					(int) Math.ceil((double) docIds.size() / (double) threadsTotal));

			// Prepare the threads and launch them
			List<Future<? extends DocumentProcessorStats>> futures = new ArrayList<>();
			for (List<Long> segment : segments) {
				if (interruptRequested)
					continue;

				DocumentProcessorCallable<? extends DocumentProcessorStats> callable = prepareCallable(segment);
				threads.add(callable);
				futures.add(Context.get(ThreadPools.class).schedule(callable, getName(), 1));
				log.debug("Launched the processing for documents {}", segment);
			}

			// Wait for the completion of all the indexers
			waitForCompletion(futures);

			log.info("All threads have completed");
		}
	}

	abstract protected DocumentProcessorCallable<? extends DocumentProcessorStats> prepareCallable(List<Long> segment);

	@Override
	public synchronized void interrupt() {
		super.interrupt();
		for (DocumentProcessorCallable<? extends DocumentProcessorStats> thread : threads)
			thread.interrupt();
	}

	/**
	 * Stays waiting for the completion of all the callables, collecting the
	 * total time spent in processing.
	 * 
	 * @param futures The futures containing the stats from each one of the
	 *        running callables
	 * @param futures The list of futures to wait for
	 */
	protected void waitForCompletion(Collection<Future<? extends DocumentProcessorStats>> futures) {
		try {
			List<DocumentProcessorStats> stats = new LaxSerialFuture<>(futures).getAll();
			for (DocumentProcessorStats stat : stats) {
				processed += stat.getProcessed();
				errors += stat.getErrors();
			}
			allCompleted(stats);
		} catch (ExecutionException e) {
			log.error(e.getMessage(), e);
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
		}
	}

	/**
	 * Invoked when all the callables completed
	 * 
	 * @param stats The stats from the callables
	 */
	protected void allCompleted(List<DocumentProcessorStats> stats) {
		// do nothing by default
	}

	private void removeTransactionReference() {
		try {
			int count = documentDao.jdbcUpdate(
					"update ld_document set ld_transactionid = null where ld_transactionId = :transactionId",
					Map.of("transactionId", transactionId));
			if (log.isInfoEnabled())
				log.info("{} documents released from transaction {}", count, transactionId);
		} catch (PersistenceException e) {
			log.warn(e.getMessage(), e);
		}
	}

	private void getSize(int max, List<Long> ids) {
		size = ids.size();
		if (max < size && max > 0)
			size = max;
		log.info("Found a total of {} documents to be processed", size);
	}

	@Override
	public boolean isIndeterminate() {
		return false;
	}

	@Override
	public boolean isConcurrent() {
		return false;
	}

	/**
	 * Retrieves the user name in whose name to operate
	 */
	protected String getDefaultUser() {
		return "_system";
	}

	/**
	 * Retrieves the batch size for each run
	 */
	protected abstract int getBatchSize();

	/**
	 * Prepares the query conditions for selecting the documents that have to be
	 * processed
	 * 
	 * @param where The query being prepared
	 * @param sort The sorting clause
	 */
	protected abstract void prepareQueueQuery(StringBuilder where, StringBuilder sort);

	@Override
	public String prepareReport(Locale locale) {
		StringBuilder sb = new StringBuilder();
		sb.append(I18N.message("processeddocs", locale) + ": ");
		sb.append(processed);
		sb.append("\n");
		sb.append(I18N.message("errors", locale) + ": ");
		sb.append(errors);
		return sb.toString();
	}
}