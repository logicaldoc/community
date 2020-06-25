package com.logicaldoc.gui.frontend.client.document.form;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.common.client.widgets.ExtendedPropertiesPanel;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used get the form's fields values
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class FillForm extends Window {
	private ExtendedPropertiesPanel propertiesPanel;

	public FillForm(GUIDocument frm) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		setTitle(I18N.message("form") + " - " + Util.getBaseName(frm.getFileName()));
		setWidth(500);
		setHeight(400);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		
		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				onSave();
			}
		});

		HLayout buttonsBar = new HLayout();
		buttonsBar.setWidth100();
		buttonsBar.setHeight(25);
		buttonsBar.setMembers(save);
		
		
		propertiesPanel = new ExtendedPropertiesPanel(frm, null, true, true, false);
		
		VLayout layout = new VLayout();
		layout.setMargin(3);
		layout.setMembersMargin(3);
		layout.setMembers(propertiesPanel, buttonsBar);
		
		addItem(layout);
	}

	public void onSave() {
		if (!propertiesPanel.validate())
			return;

		ContactingServer.get().show();
		DocumentService.Instance.get().createWithContent((GUIDocument) propertiesPanel.getObject(), null,
				new AsyncCallback<GUIDocument>() {
					@Override
					public void onFailure(Throwable caught) {
						ContactingServer.get().hide();
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(GUIDocument doc) {
						ContactingServer.get().hide();
						DocumentsPanel.get().refresh();
						destroy();
					}
				});
	}
}