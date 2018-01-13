package com.logicaldoc.gui.frontend.client.document;

import com.logicaldoc.gui.common.client.beans.GUIDocument;

/**
 * This panel shows the full preview of a document
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 7.7.1
 */
public class DetailsPreviewPanel extends DocumentDetailTab {

	private com.logicaldoc.gui.common.client.widgets.PreviewPanel panel = null;

	public DetailsPreviewPanel(final GUIDocument document) {
		super(document, null);
		setMembersMargin(1);
	}

	@Override
	public void onTabSelected() {
		if (panel == null) {
			panel = new com.logicaldoc.gui.common.client.widgets.PreviewPanel(document);
			setMembers(panel);
		}
	}
}