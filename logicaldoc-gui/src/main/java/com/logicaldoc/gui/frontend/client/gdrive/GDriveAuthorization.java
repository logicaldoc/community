package com.logicaldoc.gui.frontend.client.gdrive;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.frontend.client.services.GDriveService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;

/**
 * This popup window is used to handle Google Drive settings.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class GDriveAuthorization extends Window {

	private DynamicForm form = new DynamicForm();

	public GDriveAuthorization() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("googledrive"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(2);
		setAutoSize(true);

		form.setTitleOrientation(TitleOrientation.TOP);

		final TextItem clientId = ItemFactory.newTextItem("clientid", "clientid", null);
		clientId.setWidth(370);
		clientId.setRequired(true);
		clientId.setEndRow(true);

		final TextItem clientSecret = ItemFactory.newTextItem("clientsecret", "clientsecret", null);
		clientSecret.setWidth(370);
		clientSecret.setRequired(true);
		clientSecret.setEndRow(true);

		ButtonItem authorize = new ButtonItem("authorize", I18N.message("authorize"));
		authorize.setAutoFit(true);
		authorize.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				onAuthenticate();
			}
		});
		authorize.setEnableWhen(new AdvancedCriteria("acceptPrivacyPolicy", OperatorId.EQUALS, true));

		String policyUrl = Session.get().getConfig("policy.google");
		if (policyUrl == null)
			policyUrl = "https://www.logicaldoc.com/google-integration-privacy-policy";
		CheckboxItem acceptPrivacyPolicy = ItemFactory.newCheckbox("acceptPrivacyPolicy",
				"<a href='" + policyUrl + "' target='_blank'>" + I18N.message("havereadprivacypolicy") + "</a>");
		acceptPrivacyPolicy.setRequired(true);

//		acceptPrivacyPolicy.setHint("<a href='"+policyUrl+"' target='_blank'>"+ "I have read the privacy policy and authorize the processing of personal data" +"</a>");

		form.setFields(clientId, clientSecret, acceptPrivacyPolicy, authorize);

		addItem(form);

		GDriveService.Instance.get().loadSettings(new AsyncCallback<String[]>() {

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
		GDriveService.Instance.get().saveSettings(form.getValueAsString("clientid"),
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