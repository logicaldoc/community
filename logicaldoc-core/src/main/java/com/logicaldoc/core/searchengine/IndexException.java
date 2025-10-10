package com.logicaldoc.core.searchengine;

/**
 * An error happened during indexing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.5
 */
public class IndexException extends Exception {
	private static final long serialVersionUID = 1L;

	public IndexException() {
		super();
	}

	public IndexException(String message, Throwable cause) {
		super(message, cause);
	}

	public IndexException(String message) {
		super(message);
	}

	public IndexException(Throwable cause) {
		this(cause.getMessage(), cause);
	}
}