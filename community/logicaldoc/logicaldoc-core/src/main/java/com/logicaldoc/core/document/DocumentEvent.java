package com.logicaldoc.core.document;

/**
 * Possible events in the document's history
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.6
 */
public enum DocumentEvent {
	STORED("event.stored"),
    CHANGED("event.changed"),
	CHECKEDIN("event.checkedin"),
	CHECKEDOUT("event.checkedout"),
	IMMUTABLE("event.immutable"),
	RENAMED("event.renamed"),
	DOWNLOADED("event.downloaded"),
	INDEXED("event.indexed"),
	MOVED("event.moved"),
	LOCKED("event.locked"),
	UNLOCKED("event.unlocked"),
	ARCHIVED("event.archived"),
	DELETED("event.deleted"),
	SENT("event.sent"),
	BARCODED("event.barcoded"),
	ZONALOCRD("event.zonalocrd"),
	WORKFLOWSTATUS("event.workflowstatus"),
	WORKFLOWAPPENDED("event.workflowappended"),
	SHORTCUT_STORED("event.shortcut.stored"),
	SHORTCUT_MOVED("event.shortcut.moved"),
	SHORTCUT_DELETED("event.shortcut.deleted"),
	VIEWED("event.viewed"),
	RESTORED("event.restored"),
	NEW_NOTE("event.newnote"),
	SIGNED("event.signed"),
	EXPORTPDF("event.exportpdf"),
	EXPORTED("event.exported"),
	ADDED_TO_CALEVENT("event.caladd"),
	REMOVED_FROM_CALEVENT("event.caldel"),
	SUBSCRIBED("event.subscribed"),
	STAMPED("event.stamped"),
	DTICKET_CREATED("event.dticket.created"),
	PASSWORD_PROTECTED("event.password.protected"),
	PASSWORD_UNPROTECTED("event.password.unprotected"),
	RATING_NEW("event.rating.new"),
	CONVERTED("event.converted"),
	VERSION_DELETED("event.version.deleted"),
	VERSION_REPLACED("event.version.replaced"),
	COMPARED("event.compared"),
	COPYED("event.copyed"),
	ESIGNED("event.esigned"),
	FORM_SUBMITTED("event.form.submitted"),
	FORM_EDITED("event.form.edited");
	
    private String event;

	DocumentEvent(String event) {
	    this.event = event;
	}

	public String toString() {
	    return this.event;
	}

	public static DocumentEvent fromString(String event) {
	    if (event != null) {
	      for (DocumentEvent b : DocumentEvent.values()) {
	        if (event.equalsIgnoreCase(b.event)) {
	          return b;
	        }
	      }
	    }
 	    return null;
    }
	
	public static String[] valuesAsString() {
		DocumentEvent[] tmp = DocumentEvent.values();
		String[] vals = new String[tmp.length];
		for (int i = 0; i < tmp.length; i++)
			vals[i] = tmp[i].toString();
		return vals;
	}
}