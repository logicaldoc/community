package com.logicaldoc.util.exception;

/**
 * An exception useful in cycles instead of using the continue keywords
 * 
 * @author Marco Meschieri - LogicalDOCs
 * @since 8.8.4
 */
public class SkipException extends Exception {

	private static final long serialVersionUID = 1L;

	public SkipException() {
		super("skip");
	}
	
	public SkipException(String message) {
		super(message);
	}
}