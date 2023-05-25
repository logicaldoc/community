package com.logicaldoc.core.folder;

/**
 * Possible events in the folder's history
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.6
 */
public enum FolderEvent {
	
	CREATED("event.folder.created"),
	RENAMED("event.folder.renamed"),
	PERMISSION("event.folder.permission"),
	DELETED("event.folder.deleted"),
	MOVED("event.folder.moved"),
	CHANGED("event.folder.changed"),
	SUBFOLDER_CREATED("event.folder.subfolder.created"),
	SUBFOLDER_RENAMED("event.folder.subfolder.renamed"),
	SUBFOLDER_PERMISSION("event.folder.subfolder.permission"),
	SUBFOLDER_DELETED("event.folder.subfolder.deleted"),
	SUBFOLDER_CHANGED("event.folder.subfolder.changed"),
	SUBFOLDER_MOVED("event.folder.subfolder.moved"),
	SUBFOLDER_RESTORED("event.folder.subfolder.restored"),
	RESTORED("event.folder.restored"),
	EXPORTED("event.folder.exported"),
	SUBSCRIBED("event.folder.subscribed"),
	QUOTA_OVERTHRESHOLD("event.folder.quota.overthreshold"),
	ALIAS_CREATED("event.folder.alias.created");
	
    private String event;

	FolderEvent(String event) {
	    this.event = event;
	}

	@Override
	public String toString() {
	    return this.event;
	}

	public static FolderEvent fromString(String event) {
	    if (event != null) {
	      for (FolderEvent b : FolderEvent.values()) {
	        if (event.equalsIgnoreCase(b.event)) {
	          return b;
	        }
	      }
	    }
 	    return null;
    }
}
