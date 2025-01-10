package com.logicaldoc.gui.frontend.client.sharefile;

import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.frontend.client.services.ShareFileService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

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

		ShareFileService.Instance.get().loadSettings(new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(List<String> settings) {
				initGUI(settings);
			}
		});
	}

	private void initGUI(List<String> settings) {
		TextItem clientId = ItemFactory.newTextItem("clientid", settings.get(0));
		clientId.setRequired(true);
		clientId.setWidth(300);

		TextItem clientSecret = ItemFactory.newTextItem("clientsecret", settings.get(1));
		clientSecret.setRequired(true);
		clientSecret.setWidth(300);

		TextItem authBaseUrl = ItemFactory.newTextItem("authbaseurl", settings.get(2));
		authBaseUrl.setRequired(true);
		authBaseUrl.setDisabled(true);
		authBaseUrl.setWidth(300);

		TextItem callbackUrl = ItemFactory.newTextItem("callbackurl", settings.get(3));
		callbackUrl.setRequired(false);
		callbackUrl.setDisabled(true);
		callbackUrl.setWidth(300);

		ButtonItem authorize = new ButtonItem("authorize", I18N.message("authorize"));
		authorize.setAutoFit(true);

		authorize.addClickHandler(event -> onAuthorize());

		form.setTitleOrientation(TitleOrientation.TOP);
		form.setFields(clientId, clientSecret, authBaseUrl, callbackUrl, authorize);

		addItem(form);
	}

	public void onAuthorize() {
		if (!form.validate())
			return;

		ShareFileService.Instance.get().authorize(form.getValueAsString("clientid"),
				form.getValueAsString("clientsecret"), new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(String authorizationUrl) {
						WindowUtils.openUrl(authorizationUrl, "_blank", null);
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