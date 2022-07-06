package com.logicaldoc.gui.frontend.client.document.stamp;

import com.logicaldoc.gui.common.client.beans.GUIStamp;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ExtendedPropertiesPanel;
import com.logicaldoc.gui.common.client.widgets.StickyWindow;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsGrid;
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

	protected DocumentsGrid sourceGrid;

	public FillStamp(DocumentsGrid sourceGrid, GUIStamp stamp, boolean visualPositioning) {
		super("applystamp");
		this.sourceGrid = sourceGrid;
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
		save.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				onSave();
			}
		});

		ToolStrip buttonsBar = new ToolStrip();
		buttonsBar.setWidth100();
		buttonsBar.addButton(save);

		propertiesPanel = new ExtendedPropertiesPanel(stamp, null, true, true, false);

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

		StampDialog.applyStamp(stamp, visualPositioning, this, sourceGrid);
	}
}