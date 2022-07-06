package com.logicaldoc.gui.common.client.widgets;

import com.logicaldoc.gui.common.client.beans.GUIFolder;

/**
 * Listener invoked on new folder seletion.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public interface FolderChangeListener {
	public void onChanged(GUIFolder folder);
}