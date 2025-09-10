package com.logicaldoc.core.task;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;

import org.slf4j.Logger;

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

	// The logger to use
	protected Logger log;

	DocumentProcessorCallable(List<Long> docIds, Logger log) {
		this.docIds = docIds;
		this.log = log;
	}

	@Override
	public abstract T call() throws Exception;

	public boolean isCompleted() {
		return completed;
	}

	public void interrupt() {
		interrupt = true;
	}
}