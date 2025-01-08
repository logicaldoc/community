package com.logicaldoc.gui.frontend.client.document.stamp;

import java.util.List;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIStamp;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ExtendedPropertiesPanel;
import com.logicaldoc.gui.common.client.widgets.StickyWindow;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This popup window is used get the stamp's fields values
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.3
 */
public class FillStamp extends StickyWindow {
	private ExtendedPropertiesPanel propertiesPanel;

	private boolean visualPositioning = false;

	private GUIStamp stamp;

	protected List<GUIDocument> documents;

	public FillStamp(List<GUIDocument> documents, GUIStamp stamp, boolean visualPositioning) {
		super("applystamp");
		this.documents = documents;
		this.visualPositioning = visualPositioning;
		this.stamp = stamp;

		setTitle(I18N.message("stamp") + " - " + Util.getBaseName(stamp.getName()));
	}

	@Override
	protected WindowStatus getDefaultStatus() {
		return new WindowStatus(500, 400);
	}

	@Override
	protected void onDraw() {
		super.onDraw();

		ToolStripButton save = new ToolStripButton(I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(event -> onSave());

		ToolStrip buttonsBar = new ToolStrip();
		buttonsBar.setWidth100();
		buttonsBar.addButton(save);

		propertiesPanel = new ExtendedPropertiesPanel(stamp, null, true, true, false, true);

		VLayout propertiesContainer = new VLayout();
		propertiesContainer.setWidth100();
		propertiesContainer.setHeight100();
		propertiesContainer.setMargin(3);
		propertiesContainer.setMembersMargin(3);
		propertiesContainer.setOverflow(Overflow.AUTO);
		propertiesContainer.setAlwaysShowScrollbars(false);
		propertiesContainer.setMembers(propertiesPanel);

		addItem(buttonsBar);
		addItem(propertiesContainer);
	}

	public void onSave() {
		if (!propertiesPanel.validate())
			return;

		StampDialog.applyStamp(stamp, visualPositioning, this, documents);
	}
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}