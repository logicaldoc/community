package com.logicaldoc.gui.frontend.client.sharefile;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.frontend.client.services.ShareFileService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;

/**
 * This popup window is used to handle ShareFile settings.
 * 
 * @author Marco Meschieri - LogicalDOC since 7.2.1
 */
public class ShareFileSettings extends Window {
	private DynamicForm form = new DynamicForm();

	public ShareFileSettings() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("sharefile"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(2);
		setAutoSize(true);

		ShareFileService.Instance.get().loadSettings(new AsyncCallback<String[]>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(String[] settings) {
				initGUI(settings);
			}

		});
	}

	private void initGUI(String[] settings) {
		TextItem clientId = ItemFactory.newTextItem("clientId", "clientid", settings[0]);
		clientId.setRequired(true);
		clientId.setWidth(300);

		TextItem clientSecret = ItemFactory.newTextItem("clientSecret", "clientsecret", settings[1]);
		clientSecret.setRequired(true);
		clientSecret.setWidth(300);

		TextItem authBaseUrl = ItemFactory.newTextItem("authBaseUrl", "authbaseurl", settings[2]);
		authBaseUrl.setRequired(true);
		authBaseUrl.setDisabled(true);
		authBaseUrl.setWidth(300);

		TextItem callbackUrl = ItemFactory.newTextItem("callbackUrl", "callbackurl", settings[3]);
		callbackUrl.setRequired(false);
		callbackUrl.setDisabled(true);
		callbackUrl.setWidth(300);

		ButtonItem authorize = new ButtonItem("authorize", I18N.message("authorize"));
		authorize.setAutoFit(true);

		authorize.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				onAuthorize();
			}
		});

		form.setTitleOrientation(TitleOrientation.TOP);
		form.setFields(clientId, clientSecret, authBaseUrl, callbackUrl, authorize);

		addItem(form);
	}

	public void onAuthorize() {
		if (!form.validate())
			return;

		ShareFileService.Instance.get().authorize(form.getValueAsString("clientId"),
				form.getValueAsString("clientSecret"), new AsyncCallback<String>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(String authorizationUrl) {
						WindowUtils.openUrl(authorizationUrl, "_blank", null);
						destroy();
					}
				});
	}
}