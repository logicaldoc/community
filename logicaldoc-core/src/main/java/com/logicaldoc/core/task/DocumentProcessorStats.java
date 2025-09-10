package com.logicaldoc.core.task;

/**
 * Statistics related to a document processing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.1
 */
public class DocumentProcessorStats {

	private long processed = 0;

	private long errors = 0;

	private long processingTime = 0;
	
	public long getProcessed() {
		return processed;
	}

	public void setProcessed(long processed) {
		this.processed = processed;
	}

	public long getErrors() {
		return errors;
	}

	public void setErrors(long errors) {
		this.errors = errors;
	}

	public long getProcessingTime() {
		return processingTime;
	}

	public void setProcessingTime(long processingTime) {
		this.processingTime = processingTime;
	}	
}