package com.logicaldoc.core.task;

import java.io.IOException;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.security.User;
import com.logicaldoc.i18n.I18N;

/**
 * A base implementation for those tasks that process documents
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.3
 */
public abstract class AbstractDocumentProcessor extends Task {

	protected int processed = 0;

	protected int errors = 0;

	protected DocumentDAO documentDao;

	protected AbstractDocumentProcessor(String name) {
		super(name);
	}

	@Override
	protected void runTask() throws TaskException {
		if (!lockManager.get(getName(), transactionId)) {
			log.warn("Unable to acquire lock {}, skipping processing", getName());
			return;
		}

		log.info("Start processing of all documents");
		errors = 0;
		processed = 0;

		try {
			int max = getBatchSize();

			@SuppressWarnings("unchecked")
			List<Long> ids = documentDao.queryForList("select ld_id from ld_document where " + prepareQueueQuery(null),
					null, Long.class, max);
			getSize(max, ids);

			if (size > 0)
				processDocuments(ids, max);
		} catch (PersistenceException e) {
			throw new TaskException(e.getMessage(), e);
		} finally {
			log.info("Processing completed");
			log.info("Processed documents: {}", processed);
			log.info("Errors: {}", errors);

			// To be safer always release the lock
			lockManager.release(getName(), transactionId);

			removeTransactionReference();
		}
	}

	private void processDocuments(List<Long> docIds, int max) throws PersistenceException {
		String idsStr = docIds.stream().map(id -> Long.toString(id)).collect(Collectors.joining(","));
		if (StringUtils.isNotEmpty(idsStr))
			idsStr = "(" + idsStr + ")";

		// Mark all these documents as belonging to the current
		// transaction
		documentDao.bulkUpdate("set ld_transactionid='" + transactionId + "' where ld_id in " + idsStr,
				(Map<String, Object>) null);

		// Now we can release the lock
		lockManager.release(getName(), transactionId);

		@SuppressWarnings("unchecked")
		List<Object[]> records = documentDao.query(
				"select ld_id, ld_filename from ld_document where ld_id in " + idsStr, new RowMapper<Object[]>() {
					@Override
					public Object[] mapRow(ResultSet rs, int row) throws SQLException {
						Object[] rec = new Object[2];
						rec[0] = rs.getLong(1);
						rec[1] = rs.getString(2);
						return rec;
					}
				}, max);

		User user = loadUser();
		for (Object[] cols : records) {
			long id = (Long) cols[0];
			log.debug("Process document {}", id);

			try {
				Document doc = documentDao.findById(id);
				if (doc == null)
					continue;
				documentDao.initialize(doc);

				processDocument(doc, user);

				log.debug("Processed document {}", id);
				processed++;
			} catch (Exception e) {
				log.error(e.getMessage(), e);
				errors++;
			} finally {
				next();
			}
			if (interruptRequested)
				return;
		}
	}

	private void removeTransactionReference() {
		try {
			documentDao.bulkUpdate("set ld_transactionid=null where ld_transactionId='" + transactionId + "'",
					(Map<String, Object>) null);
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

	private User loadUser() throws PersistenceException {
		User user = userDao.findByUsername(getDefaultUser());
		if (user == null)
			user = userDao.findByUsername("_system");
		return user;
	}

	@Override
	public boolean isIndeterminate() {
		return false;
	}

	@Override
	public boolean isConcurrent() {
		return true;
	}

	/**
	 * Retrieves the user name in whose name to operate
	 */
	protected abstract String getDefaultUser();

	/**
	 * Retrieves the batch size for each run
	 */
	protected abstract int getBatchSize();

	/**
	 * Concrete implementations put here the processing logic
	 * 
	 * @param document the document to be processed
	 * @param user the user to process the document in the name of
	 * 
	 * @throws IOException I/O error
	 * @throws PersistenceException Error in the persistence layer
	 */
	protected abstract void processDocument(Document document, User user) throws PersistenceException, IOException;

	/**
	 * Prepares the query conditions for selecting the documents that have to be
	 * processed
	 */
	protected abstract String prepareQueueQuery(Long tenantId);

	public void setDocumentDao(DocumentDAO documentDao) {
		this.documentDao = documentDao;
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