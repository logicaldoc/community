package com.logicaldoc.core.task;

/**
 * An error happened during task esexcution
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.5
 */
public class TaskException extends Exception {
	private static final long serialVersionUID = 1L;

	public TaskException() {
		super();
	}

	public TaskException(String message, Throwable cause) {
		super(message, cause);
	}

	public TaskException(String message) {
		super(message);
	}

	public TaskException(Throwable cause) {
		this(cause.getMessage(), cause);
	}
}