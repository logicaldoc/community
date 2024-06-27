package com.logicaldoc.webservice.doc;

public class DocException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	public DocException() {
		super();
	}

	public DocException(String message) {
		super(message);
	}

	public DocException(Throwable cause) {
		super(cause);
	}

	public DocException(String message, Throwable cause) {
		super(message, cause);
	}

	public DocException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}
}