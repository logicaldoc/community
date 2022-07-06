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
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;

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
		TextItem accountId = ItemFactory.newTextItem("accountId", "accountid", settings.getAccountId());
		accountId.setRequired(true);
		accountId.setWidth(300);

		TextItem integrationKey = ItemFactory.newTextItem("integrationKey", "integrationkey",
				settings.getIntegrationKey());
		integrationKey.setRequired(true);
		integrationKey.setWidth(300);

		TextItem secretKey = ItemFactory.newTextItem("secretKey", "secretkey", settings.getSecretKey());
		secretKey.setRequired(true);
		secretKey.setWidth(300);

		SelectItem authBaseUrl = ItemFactory.newSelectItem("authBaseUrl", "authbaseurl");
		authBaseUrl.setValue(settings.getAuthBaseUrl());
		authBaseUrl.setRequired(true);
		authBaseUrl.setValueMap("https://account.docusign.com", "https://account-d.docusign.com");
		authBaseUrl.setWidth(300);
		
		TextItem accountBaseUrl = ItemFactory.newTextItem("accountBaseUrl", "accountbaseurl", settings.getApiBaseUrl());
		accountBaseUrl.setRequired(true);
		accountBaseUrl.setWidth(300);

		TextItem callbackUrl = ItemFactory.newTextItem("callbackUrl", "callbackurl",settings.getCallbackUrl());
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
		form.setFields(authBaseUrl, accountBaseUrl, callbackUrl, accountId, integrationKey, secretKey, authorize);

		addItem(form);
	}

	public void onAuthorize() {
		if (!form.validate())
			return;

		settings.setAccountId(form.getValueAsString("accountId"));
		settings.setAuthBaseUrl(form.getValueAsString("authBaseUrl"));
		settings.setApiBaseUrl(form.getValueAsString("accountBaseUrl"));
		settings.setIntegrationKey(form.getValueAsString("integrationKey"));
		settings.setSecretKey(form.getValueAsString("secretKey"));

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