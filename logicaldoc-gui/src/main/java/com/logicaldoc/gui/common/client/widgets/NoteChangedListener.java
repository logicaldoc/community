package com.logicaldoc.gui.common.client.widgets;

import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;

/**
 * Listener invoked when the note has been changed/saved
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.3
 */
public interface NoteChangedListener {
	public void onChanged(GUIDocumentNote note);
}