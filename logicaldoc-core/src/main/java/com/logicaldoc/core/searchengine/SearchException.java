package com.logicaldoc.core.searchengine;

/**
 * An error happened during search
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.4
 *
 */
public class SearchException extends Exception {
	private static final long serialVersionUID = 1L;

	public SearchException() {
		super();
	}

	public SearchException(String message, Throwable cause) {
		super(message, cause);
	}

	public SearchException(String message) {
		super(message);
	}

	public SearchException(Throwable cause) {
		this(cause.getMessage(), cause);
	}
}