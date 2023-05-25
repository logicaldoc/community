package com.logicaldoc.gui.frontend.client.zoho;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.frontend.client.services.ZohoService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * This popup window is used to handle Zoho settings.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.1
 */
public class ZohoAuthorization extends Window {
	private DynamicForm form = new DynamicForm();

	public ZohoAuthorization() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("zoho"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(2);
		setAutoSize(true);

		form.setTitleOrientation(TitleOrientation.TOP);

		final TextItem clientId = ItemFactory.newTextItem("clientid", null);
		clientId.setWidth(370);
		clientId.setRequired(true);
		clientId.setEndRow(true);

		final TextItem clientSecret = ItemFactory.newTextItem("clientsecret", null);
		clientSecret.setWidth(370);
		clientSecret.setRequired(true);
		clientSecret.setEndRow(true);

		ButtonItem authorize = new ButtonItem("authorize", I18N.message("authorize"));
		authorize.setAutoFit(true);

		authorize.addClickHandler(event -> onAuthenticate());

		form.setFields(clientId, clientSecret, authorize);

		addItem(form);

		ZohoService.Instance.get().loadSettings(new AsyncCallback<String[]>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(String[] settings) {
				clientId.setValue(settings[0]);
				clientSecret.setValue(settings[1]);
			}
		});
	}

	public void onAuthenticate() {
		ZohoService.Instance.get().saveSettings(form.getValueAsString("clientid"),
				form.getValueAsString("clientsecret"), new AsyncCallback<String>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(String consentUrl) {
						WindowUtils.openUrl(consentUrl, "_blank", null);
						destroy();
					}
				});
	}
}