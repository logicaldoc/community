package com.logicaldoc.core.document;

/**
 * Used to declare a duplicated insert
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.8
 */
public class DuplicateDocumentExeption extends Exception {

	private static final long serialVersionUID = 1L;

	private final String customId;

	public String getCustomId() {
		return customId;
	}
	
	public DuplicateDocumentExeption(String customId, Throwable cause) {
		super("Duplicate Document. customid: " + customId, cause);
		this.customId = customId;
	}

	public DuplicateDocumentExeption(String customId) {
		this(customId, null);
	}
}