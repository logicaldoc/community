package com.logicaldoc.core.document;

import java.util.List;
import java.util.Locale;

import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.task.Task;
import com.logicaldoc.core.task.TaskException;
import com.logicaldoc.i18n.I18N;

/**
 * This task takes care of calculating the documents digest
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
public class DigestProcessor extends Task {
	public static final String NAME = "DigestProcessor";

	@Autowired
	private DocumentDAO documentDao;

	private long processed = 0;

	private long errors = 0;

	public DigestProcessor() {
		super(NAME);
		log = LoggerFactory.getLogger(DigestProcessor.class);
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
	protected void runTask() throws TaskException {
		log.info("Start processing of digests");
		errors = 0;
		processed = 0;
		try {
			// First of all find documents to be processed
			size = documentDao.queryForLong(
					"select count(*) from ld_document where ld_deleted = 0 and ld_docref is null and ld_digest is null");

			Integer max = config.getProperty("digest.batch") != null
					? Integer.parseInt(config.getProperty("digest.batch"))
					: null;

			if (max != null && max.intValue() < size && max.intValue() > 0)
				size = max.intValue();

			if (max != null && max.intValue() < 1)
				max = null;

			log.info("Found a total of {} documents to be processed", size);

			List<Long> ids = documentDao.findIdsByWhere(PersistentObjectDAO.ENTITY + ".docRef is null and "
					+ PersistentObjectDAO.ENTITY + ".digest is null and deleted = 0", null, max);
			for (Long id : ids) {
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
		}
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