package com.logicaldoc.webservice;

/**
 * A generic exception raised by webservice methods
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.3
 */
public class WebserviceException extends Exception {

	private static final long serialVersionUID = 1L;

	public WebserviceException() {
	}

	public WebserviceException(String message) {
		super(message);
	}

	public WebserviceException(Throwable cause) {
		super(cause);
	}

	public WebserviceException(String message, Throwable cause) {
		super(message, cause);
	}
}
