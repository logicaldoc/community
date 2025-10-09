package com.logicaldoc.gui.frontend.client.dropbox;

import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * This popup window is used to handle Dropbox API credentials.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.4
 */
public class DropboxAuthorization extends Window {

	private static final String CONSTANT_A = "apisecret";

	private static final String CONSTANT_B = "apikey";

	private static DropboxAuthorization instance = new DropboxAuthorization();

	private DynamicForm form = new DynamicForm();

	private TextItem apiKey = ItemFactory.newPasswordItem(CONSTANT_B, CONSTANT_B, null);

	private TextItem apiSecret = ItemFactory.newPasswordItem(CONSTANT_A, CONSTANT_A, null);

	private DropboxAuthorization() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("googleapi"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(2);
		setAutoSize(true);

		form.setTitleOrientation(TitleOrientation.TOP);

		apiKey.setWidth(370);
		apiKey.setRequired(true);
		apiKey.setEndRow(true);

		apiSecret.setWidth(370);
		apiSecret.setRequired(true);
		apiSecret.setEndRow(true);

		ButtonItem authorize = new ButtonItem("authorize", I18N.message("authorize"));
		authorize.setAutoFit(true);
		authorize.addClickHandler(event -> onAuthenticate());

		form.setFields(apiKey, apiSecret, authorize);

		addItem(form);
	}

	@Override
	protected void onDraw() {
		DropboxService.Instance.get().loadSettings(new DefaultAsyncCallback<>() {
			@Override
			public void handleSuccess(List<String> settings) {
				apiKey.setValue(settings.get(0));
				apiSecret.setValue(settings.get(1));
			}
		});
	}

	public void onAuthenticate() {
		DropboxService.Instance.get().saveSettings(form.getValueAsString(CONSTANT_B), form.getValueAsString(CONSTANT_A),
				new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(Void consentUrl) {
						hide();
						DropboxService.Instance.get().startAuthorization(new DefaultAsyncCallback<>() {
							@Override
							public void handleSuccess(String authorizationUrl) {
								new DropboxAccessTokenWizard(authorizationUrl).show();
							}
						});
					}
				});
	}

	public static DropboxAuthorization get() {
		return instance;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof DropboxAuthorization)
			return super.equals(obj);
		else
			return false;
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}