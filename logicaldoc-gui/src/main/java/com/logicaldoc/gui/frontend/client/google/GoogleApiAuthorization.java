package com.logicaldoc.gui.frontend.client.google;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * This popup window is used to handle Google API credentials.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class GoogleApiAuthorization extends Window {

	private static final String CONSTANT_A = "clientsecret";

	private static final String CONSTSNT_B = "clientid";

	private static GoogleApiAuthorization instance = new GoogleApiAuthorization();

	private DynamicForm form = new DynamicForm();

	private TextItem clientId = ItemFactory.newPasswordItem(CONSTSNT_B, CONSTSNT_B, null);

	private TextItem clientSecret = ItemFactory.newPasswordItem(CONSTANT_A, CONSTANT_A, null);

	private GoogleApiAuthorization() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("googleapi"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(2);
		setAutoSize(true);

		form.setTitleOrientation(TitleOrientation.TOP);

		clientId.setWidth(370);
		clientId.setRequired(true);
		clientId.setEndRow(true);

		clientSecret.setWidth(370);
		clientSecret.setRequired(true);
		clientSecret.setEndRow(true);

		ButtonItem authorize = new ButtonItem("authorize", I18N.message("authorize"));
		authorize.setAutoFit(true);
		authorize.addClickHandler(event -> onAuthenticate());
		authorize.setEnableWhen(new AdvancedCriteria("acceptPrivacyPolicy", OperatorId.EQUALS, true));

		String policyUrl = Session.get().getConfig("policy.google");
		if (policyUrl == null)
			policyUrl = "https://www.logicaldoc.com/google-integration-privacy-policy";
		CheckboxItem acceptPrivacyPolicy = ItemFactory.newCheckbox("acceptPrivacyPolicy",
				"<a href='" + policyUrl + "' target='_blank'>" + I18N.message("havereadprivacypolicy") + "</a>");
		acceptPrivacyPolicy.setRequired(true);

		form.setFields(clientId, clientSecret, acceptPrivacyPolicy, authorize);

		addItem(form);
	}

	@Override
	protected void onDraw() {
		GoogleService.Instance.get().loadSettings(new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(List<String> settings) {
				clientId.setValue(settings.get(0));
				clientSecret.setValue(settings.get(1));
			}
		});
	}

	public void onAuthenticate() {
		GoogleService.Instance.get().saveSettings(form.getValueAsString(CONSTSNT_B),
				form.getValueAsString(CONSTANT_A), new AsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(String consentUrl) {
						WindowUtils.openUrl(consentUrl, "_blank", null);
						hide();
					}
				});
	}

	public static GoogleApiAuthorization get() {
		return instance;
	}
}