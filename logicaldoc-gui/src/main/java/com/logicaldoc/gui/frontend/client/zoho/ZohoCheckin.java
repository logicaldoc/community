package com.logicaldoc.gui.frontend.client.zoho;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.ZohoService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.BooleanItem;
import com.smartgwt.client.widgets.form.fields.SubmitItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;

/**
 * This popup window is used to perform the checkin of a Zoho document into
 * LogicalDOC.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.1
 */
public class ZohoCheckin extends Window {
	private static final String MAJORVERSION = "majorversion";

	private SubmitItem checkin;

	private DynamicForm form = new DynamicForm();

	public ZohoCheckin(final GUIDocument document, final ZohoEditor parentDialog) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("checkin"));
		setWidth(400);
		setHeight(140);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(2);

		BooleanItem versionItem = new BooleanItem();
		versionItem.setName(MAJORVERSION);
		versionItem.setTitle(I18N.message(MAJORVERSION));

		TextItem commentItem = ItemFactory.newTextItem("comment", null);
		commentItem.setRequired(true);
		commentItem.setWidth(240);

		checkin = new SubmitItem();
		checkin.setTitle(I18N.message("checkin"));
		checkin.setAlign(Alignment.RIGHT);
		checkin.addClickHandler((ClickEvent event) -> onCheckin(document, parentDialog));

		form.setItems(versionItem, commentItem, checkin);

		addItem(form);
	}

	public void onCheckin(final GUIDocument document, final ZohoEditor parentDialog) {
		if (!form.validate())
			return;
		LD.contactingServer();
		ZohoService.Instance.get().checkin(document.getId(), form.getValueAsString("comment"),
				Boolean.valueOf(form.getValueAsString(MAJORVERSION)), new DefaultAsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						super.onFailure(caught);
						destroy();
					}

					@Override
					public void onSuccess(GUIDocument result) {
						LD.clearPrompt();
						destroy();
						parentDialog.destroy();
						DocumentController.get().modified(result);
						DocumentController.get().setCurrentDocument(result);
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