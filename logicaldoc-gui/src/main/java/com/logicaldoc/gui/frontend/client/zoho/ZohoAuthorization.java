package com.logicaldoc.gui.frontend.client.zoho;

import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
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

		ZohoService.Instance.get().loadSettings(new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(List<String> settings) {
				clientId.setValue(settings.get(0));
				clientSecret.setValue(settings.get(1));
			}
		});
	}

	public void onAuthenticate() {
		ZohoService.Instance.get().saveSettings(form.getValueAsString("clientid"),
				form.getValueAsString("clientsecret"), new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(String consentUrl) {
						WindowUtils.openUrl(consentUrl, "_blank", null);
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