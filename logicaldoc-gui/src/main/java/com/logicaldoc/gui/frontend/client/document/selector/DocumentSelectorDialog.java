package com.logicaldoc.gui.frontend.client.document.selector;

import java.util.List;

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

	private Long defaultFolderId;

	protected DocumentSelectorDialog(Long defaultFolderId) {
		super("documentselector");
		this.defaultFolderId = defaultFolderId;
	}

	protected DocumentSelectorDialog() {
		super("documentselector");
	}

	@Override
	protected WindowStatus getDefaultStatus() {
		return new WindowStatus(950, 600);
	}

	protected abstract void onSelection(List<GUIDocument> selection);

	@Override
	protected void onDraw() {
		DocumentSelectorPanel selectionPanel = new DocumentSelectorPanel(defaultFolderId);
		addItem(selectionPanel);

		ToolStripButton confirmSelection = new ToolStripButton(I18N.message("confirmselection"));
		confirmSelection.addClickHandler(event -> onSelection(selectionPanel.getSelection()));
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setWidth100();
		toolStrip.addButton(confirmSelection);
		addItem(toolStrip);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		return prime * result + ((defaultFolderId == null) ? 0 : defaultFolderId.hashCode());
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		DocumentSelectorDialog other = (DocumentSelectorDialog) obj;
		if (defaultFolderId == null) {
			if (other.defaultFolderId != null)
				return false;
		} else if (!defaultFolderId.equals(other.defaultFolderId))
			return false;
		return true;
	}
}