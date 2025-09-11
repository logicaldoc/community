package com.logicaldoc.core.task;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;

import org.slf4j.Logger;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.util.spring.Context;

/**
 * A callable to provide for processing a segment of documents
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.1
 *
 * @param <T> The type of statistics
 */
public abstract class DocumentProcessorCallable<T extends DocumentProcessorStats> implements Callable<T> {

	/**
	 * IDs of the documents in the current segment
	 */
	protected List<Long> docIds = new ArrayList<>();

	/**
	 * Indicates the completion of this processing
	 */
	protected boolean completed = false;

	/**
	 * Indicates the request to interrupt the current elaboration
	 */
	protected boolean interrupt = false;

	/**
	 * The logger to use
	 */
	protected Logger log;

	/**
	 * The parent task
	 */
	protected Task task;

	protected T stats;

	public DocumentProcessorCallable(List<Long> docIds, Task task, Logger log) {
		this.docIds = docIds;
		this.log = log;
		this.task = task;
		this.stats = prepareStats();
	}

	@Override
	public T call() throws Exception {
		User user = loadUser();
		DocumentDAO documentDao = Context.get(DocumentDAO.class);
		for (Long id : docIds) {
			log.debug("Process document {}", id);

			try {
				Document doc = documentDao.findById(id);
				if (doc == null)
					continue;
				documentDao.initialize(doc);

				processDocument(doc, user);

				stats.setProcessed(stats.getProcessed() + 1);
				task.next();
				log.debug("Processed document {}", doc);
			} catch (Exception e) {
				stats.setErrors(stats.getErrors() + 1);
				log.error("Error processing document {}", id);
				log.error(e.getMessage(), e);
			}

			if (interrupt)
				break;
		}

		return stats;
	}

	private User loadUser() throws PersistenceException {
		User user = Context.get(UserDAO.class).findByUsername(getDefaultUser());
		if (user == null)
			user = Context.get(UserDAO.class).findByUsername("_system");
		return user;
	}

	protected String getDefaultUser() {
		return "_system";
	}

	public boolean isCompleted() {
		return completed;
	}

	public void interrupt() {
		interrupt = true;
	}

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
	 * Instantiates the right stats
	 * 
	 * @return the stats
	 */
	protected abstract T prepareStats();
}