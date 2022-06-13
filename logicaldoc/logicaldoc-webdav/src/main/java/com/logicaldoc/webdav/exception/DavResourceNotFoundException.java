package com.logicaldoc.webdav.exception;

/**
 * If a Resource could not be logically found, a @link
 * DavResourceIOException will be thrown. This could happen, if a
 * request has been started matches no resource against logicalDOC
 * @author Sebastian Wenzky
 * 
 */
public class DavResourceNotFoundException extends RuntimeException{
	/**
	 * 
	 */
	private static final long serialVersionUID = -6258212946488552163L;

	public DavResourceNotFoundException(String s){
		super(s);
	}
}
