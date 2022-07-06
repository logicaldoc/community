package com.logicaldoc.core.parser;

/**
 * When an error happens during the parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 8.7.5
 */
public class ParseException extends Exception {

	private static final long serialVersionUID = 1L;

	public ParseException(String message, Throwable cause) {
		super(message, cause);
	}

	public ParseException(String message) {
		super(message);
	}

	public ParseException(Throwable cause) {
		super(cause);
	}
}