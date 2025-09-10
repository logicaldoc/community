package com.logicaldoc.core.document;

import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.task.Task;
import com.logicaldoc.core.task.TaskException;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.spring.Context;

import jakarta.annotation.Resource;

/**
 * This task takes care of calculating the documents digest
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
@Component("digestProcessor")
public class DigestProcessor extends Task {
	public static final String NAME = "DigestProcessor";

	@Resource(name = "documentDAO")
	protected DocumentDAO documentDao;

	private long processed = 0;

	private long errors = 0;

	public DigestProcessor() {
		super(NAME);
		log = LoggerFactory.getLogger(DigestProcessor.class);
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
	protected void runTask() throws TaskException {
		log.info("Start processing of digests");
		errors = 0;
		processed = 0;
		try {
			// Retrieve the actual transactions
			String transactionIdsStr = lockManager.getAllTransactions().stream().map(t -> "'" + t + "'")
					.collect(Collectors.joining(","));

			// First of all find documents to be processed and not already
			// involved into a transaction
			List<Long> docIds = documentDao.findIdsByWhere(
					"(" + PersistentObjectDAO.ENTITY + ".transactionId is null or " + PersistentObjectDAO.ENTITY
							+ ".transactionId not in (" + transactionIdsStr + ")) and " + PersistentObjectDAO.ENTITY
							+ ".digest is null" + " and " + PersistentObjectDAO.ENTITY + ".docRef is null",
					null, null, Context.get().getProperties().getInt("digest.batch", 500));
			size = docIds.size();
			log.info("Found a total of {} documents to process", size);

			assignTransaction(docIds);

			for (Long id : docIds) {
				processDocument(id);
				if (interruptRequested)
					return;
			}
		} catch (PersistenceException e) {
			throw new TaskException(e.getMessage(), e);
		} finally {
			log.info("Digest processing finished");
			log.info("Processed documents: {}", processed);
			log.info("Errors: {}", errors);

			try {
				// To be safer always release the lock
				lockManager.release(getName(), transactionId);

				// Remove the transaction reference
				documentDao.jdbcUpdate(
						"update ld_document set ld_transactionid = null where ld_transactionId = :transactionId",
						Map.of("transactionId", transactionId));
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}

		}
	}

	private void assignTransaction(List<Long> docIds) throws PersistenceException {
		if (!docIds.isEmpty()) {
			// Mark all these documents as belonging to the current
			// transaction. This may require time
			String idsStr = docIds.stream().map(id -> Long.toString(id)).collect(Collectors.joining(","));
			documentDao.jdbcUpdate(
					" update ld_document set ld_transactionid = :transactionId where ld_transactionid is null and ld_id in ("
							+ idsStr + ")",
					Map.of("transactionId", transactionId));
		}
		log.info("Documents marked for digest calculation in transaction {}", transactionId);
	}

	private void processDocument(Long id) {
		try {
			log.debug("Processing document {}", id);

			Document doc = documentDao.findById(id);
			documentDao.updateDigest(doc);

			log.debug("Processed document {}", id);
			processed++;
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			errors++;
		} finally {
			next();
		}
	}

	@Override
	protected String prepareReport(Locale locale) {
		StringBuilder sb = new StringBuilder();
		sb.append(I18N.message("processeddocs", locale) + ": ");
		sb.append(processed);
		sb.append("\n");
		sb.append(I18N.message("errors", locale) + ": ");
		sb.append(errors);
		return sb.toString();
	}
}