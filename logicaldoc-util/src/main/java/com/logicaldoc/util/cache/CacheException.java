package com.logicaldoc.util.cache;

/**
 * An error happened in the cache
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.5
 */
public class CacheException extends RuntimeException {
	private static final long serialVersionUID = 1L;

	public CacheException() {
		super();
	}

	public CacheException(String message, Throwable cause) {
		super(message, cause);
	}

	public CacheException(String message) {
		super(message);
	}

	public CacheException(Throwable cause) {
		this(cause.getMessage(), cause);
	}
}