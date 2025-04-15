package com.logicaldoc.core.folder;

/**
 * Possible events in the folder's history
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.6
 */
public enum FolderEvent {
	CREATED, RENAMED, PERMISSION, DELETED, MOVED, CHANGED, SUBFOLDER_CREATED, SUBFOLDER_RENAMED, SUBFOLDER_PERMISSION, SUBFOLDER_DELETED, SUBFOLDER_CHANGED, SUBFOLDER_MOVED, SUBFOLDER_RESTORED, RESTORED, EXPORTED, SUBSCRIBED, QUOTA_OVERTHRESHOLD, ALIAS_CREATED, DOCUMENT_DESTROYED;

	/**
	 * Gets the right enumeration entry from the corresponding resource bundle
	 * key
	 * 
	 * @param key the resource bundle key
	 * 
	 * @return The corresponding entry
	 */
	public static FolderEvent fromKey(String key) {
		return FolderEvent
				.valueOf(key.contains(".") ? key.replace("event.folder.", "").toUpperCase().replace(".", "_") : key);
	}

	/**
	 * Converts the enumeration entry to the corresponding key in the resource
	 * bundle
	 * 
	 * @return The key
	 */
	public String toKey() {
		return "event.folder." + name().toLowerCase().replace("_", ".");
	}

	@Override
	public String toString() {
		return toKey();
	}

}
