package com.logicaldoc.core.document;

import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.task.Task;
import com.logicaldoc.core.task.TaskException;

/**
 * This task generate all data needed by the tag cloud panel and the tags
 * drop-down lists.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
@Component("tagsProcessor")
public class TagsProcessor extends Task {

	public static final String NAME = "TagsProcessor";

	private final DocumentDAO documentDao;

	@Autowired
	public TagsProcessor(DocumentDAO documentDao) {
		super(NAME);
		log = LoggerFactory.getLogger(TagsProcessor.class);
		this.documentDao = documentDao;
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
	protected void runTask() throws TaskException {
		log.info("Start tags processing");

		try {
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
		} catch (PersistenceException e) {
			throw new TaskException(e.getMessage(), e);
		}

		log.info("End of tags processing");
	}

}