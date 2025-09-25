package com.logicaldoc.gui.common.client;

/**
 * Exception used to inform the GUI about unhauthorized errors when using OAuth
 * external tools
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.1
 */
public class OAuthException extends ServerException {

	private static final long serialVersionUID = 1L;

	public OAuthException() {
		super();
	}

	public OAuthException(String message) {
		super(message);
	}
}