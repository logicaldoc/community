package com.logicaldoc.webdav.exception;

/**
 * Import as well as Export mechanism against logicaldoc and the client could
 * cause some issues, we do not expecting here.
 * 
 * @author Sebastian Wenzky
 * 
 */
@SuppressWarnings("serial")
public class WebDavStorageException extends Exception{
	public WebDavStorageException(String s){
		super(s);
	}
	
	public WebDavStorageException(Exception e){
		super(e);
	}
}
