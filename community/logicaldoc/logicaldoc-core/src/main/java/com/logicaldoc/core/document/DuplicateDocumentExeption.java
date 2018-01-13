package com.logicaldoc.core.document;

/**
 * Used to declare a duplicated insert
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.8
 */
public class DuplicateDocumentExeption extends Exception {

	private static final long serialVersionUID = 1L;

	private String customId;

	public String getCustomId() {
		return customId;
	}

	public void setCustomId(String customId) {
		this.customId = customId;
	}

	public DuplicateDocumentExeption(String customId, Throwable cause) {
		super("Duplicate Document. customid: " + customId, cause);
		this.customId = customId;
	}

	public DuplicateDocumentExeption(String customId) {
		this(customId, null);
	}
}
