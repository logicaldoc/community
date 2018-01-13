package com.logicaldoc.gui.frontend.client.zoho;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.ZohoService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;

/**
 * This popup window is used to handle Zoho settings.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.1
 */
public class ZohoSettings extends Window {

	private DynamicForm form = new DynamicForm();

	public ZohoSettings() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("zoho"));
		setAutoSize(true);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(2);

		form.setTitleOrientation(TitleOrientation.TOP);

		final TextItem authToken = ItemFactory.newTextItem("authtoken", "authtoken", null);
		authToken.setWidth(370);
		authToken.setRequired(true);
		authToken.setEndRow(true);

		ButtonItem save = new ButtonItem("save", I18N.message("save"));
		save.setAutoFit(true);

		save.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				onAuthenticate();
			}
		});

		form.setFields(authToken, save);

		addItem(form);

		ZohoService.Instance.get().loadAuthToken(new AsyncCallback<String>() {

			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(String setting) {
				authToken.setValue(setting);
			}
		});
	}

	public void onAuthenticate() {
		ZohoService.Instance.get().saveAuthToken(form.getValueAsString("authtoken"), new AsyncCallback<Void>() {
			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(Void ret) {
				destroy();
			}
		});
	}
}