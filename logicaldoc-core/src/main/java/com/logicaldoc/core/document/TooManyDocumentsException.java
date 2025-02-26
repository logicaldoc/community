package com.logicaldoc.core.document;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;

/**
 * Exception raised when you try to add more documents than allowed in the same
 * folder
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.1.1
 */
public class TooManyDocumentsException extends PersistenceException {
	private static final long serialVersionUID = 1L;

	private final long folderId;

	public TooManyDocumentsException(Folder folder, long max) {
		super(String.format("Folder %s [id=%d] cannot exceed the limit of %d documents configured in the settings",
				folder.getName(), folder.getId(), max));
		this.folderId = folder.getId();
	}

	public long getFolderId() {
		return folderId;
	}
}