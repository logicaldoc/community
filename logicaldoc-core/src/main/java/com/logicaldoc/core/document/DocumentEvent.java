package com.logicaldoc.core.document;

/**
 * Possible events in the document's history
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.6
 */
public enum DocumentEvent {
	STORED, CHANGED, CHECKEDIN, CHECKEDOUT, IMMUTABLE, RENAMED, DOWNLOADED, INDEXED, INDEXED_ERROR, MOVED, LOCKED, UNLOCKED, ARCHIVED, DELETED, RESOURCE_DELETED, SENT, BARCODED, ZONALOCRD, WORKFLOWSTATUS, WORKFLOWAPPENDED, SHORTCUT_STORED, SHORTCUT_MOVED, SHORTCUT_DELETED, VIEWED, RESTORED, NEW_NOTE, SIGNED, EXPORTPDF, EXPORTED, ADDED_TO_CALEVENT, REMOVED_FROM_CALEVENT, SUBSCRIBED, STAMPED, TICKET_CREATED, PASSWORD_PROTECTED, PASSWORD_UNPROTECTED, RATING_NEW, CONVERTED, VERSION_DELETED, VERSION_REPLACED, COMPARED, COPYED, ESIGNED, FORM_SUBMITTED, FORM_EDITED, READING_CONFIRMED, READING_REQUESTRED, PERMISSION;

	/**
	 * Gets the right enumeration entry from the corresponding resource bundle
	 * key
	 * 
	 * @param key the resource bundle key
	 * 
	 * @return The corresponding entry
	 */
	public static DocumentEvent fromKey(String key) {
		return DocumentEvent
				.valueOf(key.contains(".") ? key.replace("event.", "").toUpperCase().replace(".", "_") : key);
	}

	/**
	 * Converts the enumeration entry to the corresponding key in the resource
	 * bundle
	 * 
	 * @return The key
	 */
	public String toKey() {
		return "event." + name().toLowerCase().replace("_", ".");
	}

	@Override
	public String toString() {
		return toKey();
	}
}
