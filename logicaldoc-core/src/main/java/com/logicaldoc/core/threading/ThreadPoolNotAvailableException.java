package com.logicaldoc.core.threading;

/**
 * An error happened accessing a thread pool
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.5
 */
public class ThreadPoolNotAvailableException extends Exception {
	private static final long serialVersionUID = 1L;

	public ThreadPoolNotAvailableException(String pool) {
		this(pool, null);
	}

	public ThreadPoolNotAvailableException(String pool, Throwable cause) {
		super(pool + " pool was shutdown or not is not available", cause);
	}
}