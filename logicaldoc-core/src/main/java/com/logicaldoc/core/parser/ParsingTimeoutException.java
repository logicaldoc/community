package com.logicaldoc.core.parser;

/**
 * A parsing error due to timeout
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.3
 */
public class ParsingTimeoutException extends ParsingException {

	private static final long serialVersionUID = 1L;

	public ParsingTimeoutException(String message, Throwable cause) {
		super(message, cause);
	}

	public ParsingTimeoutException(String message) {
		super(message);
	}

	public ParsingTimeoutException(Throwable cause) {
		super(cause);
	}
}