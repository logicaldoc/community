package com.logicaldoc.core.document;

import java.util.List;
import java.util.Locale;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.task.Task;
import com.logicaldoc.i18n.I18N;

/**
 * This task takes care of calculating the documents digest
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.4
 */
public class DigestProcessor extends Task {
	public static final String NAME = "DigestProcessor";

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
	protected void runTask() throws Exception {
		log.info("Start processing of digests");
		errors = 0;
		processed = 0;
		try {
			// First of all find documents to be processed
			size = documentDao
					.queryForLong("select count(*) from ld_document where ld_deleted = 0 and ld_docref is null and ld_digest is null");

			Integer max = config.getProperty("digest.batch") != null ? new Integer(config.getProperty("digest.batch"))
					: null;

			if (max != null && max.intValue() < size && max.intValue() > 0)
				size = max.intValue();

			if (max != null && max.intValue() < 1)
				max = null;

			log.info("Found a total of " + size + " documents to be processed");

			List<Long> ids = documentDao.findIdsByWhere(
					"_entity.docRef is null and _entity.digest is null and deleted = 0", null, max);
			for (Long id : ids) {
				try {
					log.debug("Processing document " + id);

					Document doc = documentDao.findById(id);
					documentDao.updateDigest(doc);

					log.debug("Processed document " + id);
					processed++;
				} catch (Throwable e) {
					log.error(e.getMessage(), e);
					errors++;
				} finally {
					next();
				}
				if (interruptRequested)
					return;
			}
		} finally {
			log.info("Digest processing finished");
			log.info("Processed documents: " + processed);
			log.info("Errors: " + errors);
		}
	}

	@Override
	protected String prepareReport(Locale locale) {
		StringBuffer sb = new StringBuffer();
		sb.append(I18N.message("processeddocs", locale) + ": ");
		sb.append(processed);
		sb.append("\n");
		sb.append(I18N.message("errors", locale) + ": ");
		sb.append(errors);
		return sb.toString();
	}
}