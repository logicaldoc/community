package com.logicaldoc.gui.frontend.client.document.selector;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.StickyWindow;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * A dialog shoing a mini-browser to select documents
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
public abstract class DocumentSelectorDialog extends StickyWindow {

	public DocumentSelectorDialog() {
		super("documentselector");
	}

	@Override
	protected WindowStatus getDefaultStatus() {
		return new WindowStatus(950, 600);
	}

	protected abstract void onSelection(GUIDocument[] selection);

	@Override
	protected void onDraw() {
		DocumentSelectorPanel selectionPanel = new DocumentSelectorPanel();
		addItem(selectionPanel);

		ToolStripButton useSelection = new ToolStripButton(I18N.message("useseleteddocuments"));
		useSelection.addClickHandler(event -> onSelection(selectionPanel.getSelection()));
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setWidth100();
		toolStrip.addButton(useSelection);
		addItem(toolStrip);
	}
}