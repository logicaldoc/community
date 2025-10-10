package com.logicaldoc.core.parser;

/**
 * When an error happens during the parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 8.7.5
 */
public class ParsingException extends Exception {

	private static final long serialVersionUID = 1L;

	public ParsingException(String message, Throwable cause) {
		super(message, cause);
	}

	public ParsingException(String message) {
		super(message);
	}

	public ParsingException(Throwable cause) {
		super(cause);
	}
}