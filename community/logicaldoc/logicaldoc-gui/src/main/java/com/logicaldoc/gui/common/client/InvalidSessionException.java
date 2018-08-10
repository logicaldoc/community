package com.logicaldoc.gui.common.client;

/**
 * Thrown in case of invalid session
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class InvalidSessionException extends ServerException {

	private static final long serialVersionUID = 1L;

	public InvalidSessionException() {
		super();
	}

	public InvalidSessionException(String message) {
		super(message);
	}
}
