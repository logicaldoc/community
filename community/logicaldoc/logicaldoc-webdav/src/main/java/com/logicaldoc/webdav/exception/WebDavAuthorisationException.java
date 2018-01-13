package com.logicaldoc.webdav.exception;

/**
 * 
 * If a valid authentication header fails or being corrupted by something else.
 * 
 * @author Sebastian Wenzky
 * 
 */
@SuppressWarnings("serial")
public class WebDavAuthorisationException extends Exception {
	public WebDavAuthorisationException(String s) {
		super(s);
	}

	public WebDavAuthorisationException(Exception e) {
		super(e);
	}
}
