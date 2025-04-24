package com.logicaldoc.core.document;

import javax.persistence.Embeddable;

import com.logicaldoc.core.security.ExtendedAccessControlEntry;

/**
 * Represents all the permissions granted to a group against a business object
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.1
 */
@Embeddable
public class DocumentAccessControlEntry extends ExtendedAccessControlEntry {

	private static final long serialVersionUID = 1L;

	public DocumentAccessControlEntry() {
	}

	public DocumentAccessControlEntry(DocumentAccessControlEntry source) {
		super(source);
	}

	public DocumentAccessControlEntry(long groupId) {
		super(groupId);
	}
}
