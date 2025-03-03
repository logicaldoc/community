package com.logicaldoc.core.document;

/**
 * Used to declare a duplicated insert
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.8
 */
public class DuplicateDocumentException extends Exception {

	private static final long serialVersionUID = 1L;

	private final String customId;

	public String getCustomId() {
		return customId;
	}
	
	public DuplicateDocumentException(String customId, Throwable cause) {
		super("Duplicate Document. customid: " + customId, cause);
		this.customId = customId;
	}

	public DuplicateDocumentException(String customId) {
		this(customId, null);
	}
}