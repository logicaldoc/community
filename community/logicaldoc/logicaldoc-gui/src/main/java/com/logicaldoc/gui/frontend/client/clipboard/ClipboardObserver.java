package com.logicaldoc.gui.frontend.client.clipboard;

import com.logicaldoc.gui.common.client.beans.GUIDocument;

/**
 * Listener on clipboard events
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public interface ClipboardObserver {

	public void onAdd(GUIDocument entry);

	public void onRemove(GUIDocument entry);
}
