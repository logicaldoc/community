package com.logicaldoc.gui.frontend.client.document.form;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ExtendedPropertiesPanel;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This popup window is used get the form's fields values
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class FillForm extends Window {
	private ExtendedPropertiesPanel propertiesPanel;

	private GUIDocument document;

	public FillForm(GUIDocument document) {
		this.document = document;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		setTitle(I18N.message("form") + " - " + Util.getBaseName(document.getFileName()));
		setDefaultWidth(500);
		setDefaultHeight(400);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
	}

	@Override
	protected void onDraw() {
		ToolStripButton save = new ToolStripButton(I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(event -> onSave());

		ToolStrip buttonsBar = new ToolStrip();
		buttonsBar.setWidth100();
		buttonsBar.addButton(save);

		propertiesPanel = new ExtendedPropertiesPanel(document, null, true, true, false);

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

		LD.contactingServer();
		DocumentService.Instance.get().createWithContent((GUIDocument) propertiesPanel.getObject(), null, false,
				new DefaultAsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						LD.clearPrompt();
						super.onFailure(caught);
					}

					@Override
					public void onSuccess(GUIDocument doc) {
						LD.clearPrompt();
						DocumentsPanel.get().refresh();
						destroy();
					}
				});
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