package com.logicaldoc.gui.common.client;

/**
 * Thrown in case of invalid session
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class InvalidSessionServerException extends ServerException {

	private static final long serialVersionUID = 1L;

	public InvalidSessionServerException() {
		super();
	}

	public InvalidSessionServerException(String message) {
		super(message);
	}
}
