package com.logicaldoc.util.exception;

/**
 * Just our own runtime exception
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.4
 */
public class GenericRuntimeException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	public GenericRuntimeException(String message) {
		super(message);
	}

	public GenericRuntimeException(Throwable cause) {
		super(cause);
	}

	public GenericRuntimeException(String message, Throwable cause) {
		super(message, cause);
	}

	public GenericRuntimeException(String message, Throwable cause, boolean enableSuppression,
			boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}
}