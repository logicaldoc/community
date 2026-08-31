package com.logicaldoc.webdav.exception;

/**
 * 
 * Same as @link {@link DavResourceNotFoundException} but its more specifically
 * against logicalDOC as will be checked whether this file physically exists
 * 
 * @author Sebastian Wenzky
 * 
 */
public class DavResourceIOException extends RuntimeException {

	private static final long serialVersionUID = -1696586922095377095L;

	public DavResourceIOException(String s) {
		super(s);
	}
}
