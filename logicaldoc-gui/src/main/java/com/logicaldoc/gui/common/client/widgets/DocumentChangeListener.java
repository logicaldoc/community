package com.logicaldoc.gui.common.client.widgets;

import com.logicaldoc.gui.common.client.beans.GUIDocument;

/**
 * Listener invoked on new document seletion.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.1
 */
public interface DocumentChangeListener {
	public void onChanged(GUIDocument document);
}