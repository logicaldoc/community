package com.logicaldoc.core.document;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.task.Task;

/**
 * This task generate all data needed by the tag cloud panel and the tags
 * drop-down lists.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public class TagsProcessor extends Task {

	public static final String NAME = "TagsProcessor";

	private DocumentDAO documentDao;

	public TagsProcessor() {
		super(NAME);
		log = LoggerFactory.getLogger(TagsProcessor.class);
	}

	@Override
	public boolean isIndeterminate() {
		return false;
	}

	@Override
	public boolean isConcurrent() {
		return false;
	}

	@Override
	public long getSize() {
		return 3;
	}

	@Override
	protected void runTask() throws Exception {
		log.info("Start tags processing");

		if (!interruptRequested) {
			log.info("Clean unexisting tags");
			documentDao.cleanUnexistingUniqueTags();
			next();
		}

		if (!interruptRequested) {
			log.info("Detect new unique tags");
			documentDao.insertNewUniqueTags();
			next();
		}

		if (!interruptRequested) {
			log.info("Count tags occurrences");
			documentDao.updateCountUniqueTags();
			next();
		}

		log.info("End of tags processing");
	}

	public void setDocumentDao(DocumentDAO documentDao) {
		this.documentDao = documentDao;
	}
}