package com.logicaldoc.gui.frontend.client.folder;

import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Panel that contains both folder tool bar and folder navigator.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.3
 */
public class FolderNavigatorPanel extends VLayout {
	private static FolderNavigatorPanel instance = null;

	public static FolderNavigatorPanel get() {
		if (instance == null) {
			instance = new FolderNavigatorPanel();
		}
		return instance;
	}

	private FolderNavigatorPanel() {
		setMembers(FolderNavigator.get());
	}
}