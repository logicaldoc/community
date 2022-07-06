package com.logicaldoc.gui.common.client;

/**
 * Thrown in case of error on the server
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1
 */
public class ServerException extends Exception {

	private static final long serialVersionUID = 1L;

	public ServerException() {
	}

	public ServerException(String message) {
		super(message);
	}
}
