package com.logicaldoc.gui.frontend.client.docusign;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocuSignSettings;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.frontend.client.services.DocuSignService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * This popup window is used to handle DocuSign settings.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5
 */
public class DocuSignSettings extends Window {

	private DynamicForm form = new DynamicForm();

	private GUIDocuSignSettings settings;

	public DocuSignSettings() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("docusign"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(2);
		setAutoSize(true);

		DocuSignService.Instance.get().loadSettings(new AsyncCallback<GUIDocuSignSettings>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIDocuSignSettings settings) {
				DocuSignSettings.this.settings = settings;
				initGui();
			}

		});
	}

	private void initGui() {
		TextItem accountId = ItemFactory.newTextItem("accountid", settings.getAccountId());
		accountId.setRequired(true);
		accountId.setWidth(300);

		TextItem integrationKey = ItemFactory.newTextItem("integrationkey", settings.getIntegrationKey());
		integrationKey.setRequired(true);
		integrationKey.setWidth(300);

		TextItem secretKey = ItemFactory.newTextItem("secretkey", settings.getSecretKey());
		secretKey.setRequired(true);
		secretKey.setWidth(300);

		SelectItem authBaseUrl = ItemFactory.newSelectItem("authbaseurl");
		authBaseUrl.setValue(settings.getAuthBaseUrl());
		authBaseUrl.setRequired(true);
		authBaseUrl.setValueMap("https://account.docusign.com", "https://account-d.docusign.com");
		authBaseUrl.setWidth(300);

		TextItem accountBaseUrl = ItemFactory.newTextItem("accountbaseurl", settings.getApiBaseUrl());
		accountBaseUrl.setRequired(true);
		accountBaseUrl.setWidth(300);

		TextItem callbackUrl = ItemFactory.newTextItem("callbackurl", settings.getCallbackUrl());
		callbackUrl.setRequired(false);
		callbackUrl.setDisabled(true);
		callbackUrl.setWidth(300);

		ButtonItem authorize = new ButtonItem("authorize", I18N.message("authorize"));
		authorize.setAutoFit(true);

		authorize.addClickHandler(event -> onAuthorize());

		form.setTitleOrientation(TitleOrientation.TOP);
		form.setFields(authBaseUrl, accountBaseUrl, callbackUrl, accountId, integrationKey, secretKey, authorize);

		addItem(form);
	}

	public void onAuthorize() {
		if (!form.validate())
			return;

		settings.setAccountId(form.getValueAsString("accountid"));
		settings.setAuthBaseUrl(form.getValueAsString("authbaseurl"));
		settings.setApiBaseUrl(form.getValueAsString("accountbaseurl"));
		settings.setIntegrationKey(form.getValueAsString("integrationkey"));
		settings.setSecretKey(form.getValueAsString("secretkey"));

		DocuSignService.Instance.get().authorize(settings, new AsyncCallback<String>() {
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