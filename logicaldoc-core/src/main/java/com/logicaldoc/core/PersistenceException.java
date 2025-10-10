package com.logicaldoc.core;

import java.sql.SQLException;

/**
 * Raised when a problem happens in the data layer.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.4
 */
public class PersistenceException extends SQLException {

	private static final long serialVersionUID = 1L;

	public PersistenceException() {
		super();
	}

	public PersistenceException(String message, Throwable cause) {
		super(message, cause);
	}

	public PersistenceException(String message) {
		super(message);
	}

	public PersistenceException(Throwable cause) {
		this(cause.getMessage(), cause);
	}
}