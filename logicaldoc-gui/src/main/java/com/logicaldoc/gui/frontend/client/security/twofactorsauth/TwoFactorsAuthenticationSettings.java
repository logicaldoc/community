package com.logicaldoc.gui.frontend.client.security.twofactorsauth;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows general settings about the 2FA.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.3
 */
public class TwoFactorsAuthenticationSettings extends AdminPanel {

	private static final String TWOFA = ".2fa.";

	private static final String TWOFA_STAR = ".2fa.*";

	private static final String FALSE = "false";

	private static final String ENABLED = ".enabled";

	private static final String ALLOWTRUSTED = "allowtrusted";

	private ValuesManager vm = new ValuesManager();

	public TwoFactorsAuthenticationSettings() {
		super("twofactorsauth");

		SettingService.Instance.get().loadSettingsByNames(new String[] { Session.get().getTenantName() + TWOFA_STAR },
				new AsyncCallback<GUIParameter[]>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIParameter[] params) {
						init(params);
					}
				});
	}

	private void init(GUIParameter[] parameters) {
		DynamicForm form = new DynamicForm();
		form.setWidth(1);
		form.setValuesManager(vm);
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setNumCols(1);

		Map<String, String> settings = Util.convertToMap(parameters);
		final RadioGroupItem enable2fa = ItemFactory.newBooleanSelector("enable2fa");
		enable2fa.setValue("true".equals(settings.get("enabled")) ? "yes" : "no");
		enable2fa.setWrapTitle(false);
		enable2fa.setWrap(false);
		enable2fa.setRequired(true);
		enable2fa.setDisabled(Session.get().isDemo());

		final RadioGroupItem allowTrustedDevices = ItemFactory.newBooleanSelector(ALLOWTRUSTED,
				I18N.message("alwaysallowtrusteddev"));
		allowTrustedDevices.setValue("true".equals(settings.get(ALLOWTRUSTED)) ? "yes" : "no");
		allowTrustedDevices.setWrapTitle(false);
		allowTrustedDevices.setWrap(false);
		allowTrustedDevices.setRequired(true);

		form.setFields(enable2fa, allowTrustedDevices);

		/*
		 * EmailAuthenticator section
		 */
		DynamicForm emailForm = new DynamicForm();
		emailForm.setValuesManager(vm);
		emailForm.setTitleOrientation(TitleOrientation.TOP);
		emailForm.setIsGroup(true);
		emailForm.setGroupTitle("Email Authenticator");
		emailForm.setNumCols(1);

		final RadioGroupItem enableEmail = ItemFactory.newBooleanSelector("enableEmail",
				I18N.message("enableemailthenticator"));
		enableEmail.setValue("true".equals(settings.get(Constants.TWOFA_EMAIL_AUTHENTICATOR + ENABLED)) ? "yes" : "no");
		enableEmail.setWrapTitle(false);
		enableEmail.setWrap(false);
		enableEmail.setRequired(true);
		enableEmail.setDisabled(Session.get().isDemo());
		emailForm.setFields(enableEmail);

		/*
		 * GoogleAuthenticator section
		 */
		DynamicForm googleForm = new DynamicForm();
		googleForm.setValuesManager(vm);
		googleForm.setTitleOrientation(TitleOrientation.TOP);
		googleForm.setIsGroup(true);
		googleForm.setGroupTitle("Google Authenticator");
		googleForm.setNumCols(1);

		final RadioGroupItem enableGoolge = ItemFactory.newBooleanSelector("enableGoolge",
				I18N.message("enablegoogleauthenticator"));
		enableGoolge
				.setValue("true".equals(settings.get(Constants.TWOFA_GOOGLE_AUTHENTICATOR + ENABLED)) ? "yes" : "no");
		enableGoolge.setWrapTitle(false);
		enableGoolge.setWrap(false);
		enableGoolge.setRequired(true);
		enableGoolge.setDisabled(Session.get().isDemo());
		googleForm.setFields(enableGoolge);

		/*
		 * Yubikey section
		 */
		DynamicForm yubikeyForm = new DynamicForm();
		yubikeyForm.setValuesManager(vm);
		yubikeyForm.setTitleOrientation(TitleOrientation.TOP);
		yubikeyForm.setIsGroup(true);
		yubikeyForm.setGroupTitle("YubiKey");
		yubikeyForm.setNumCols(1);

		final RadioGroupItem enableYubikey = ItemFactory.newBooleanSelector("enableYubikey",
				I18N.message("enableyubikey"));
		enableYubikey.setValue("true".equals(settings.get("yubikey.enabled")) ? "yes" : "no");
		enableYubikey.setWrapTitle(false);
		enableYubikey.setWrap(false);
		enableYubikey.setRequired(true);
		enableYubikey.setDisabled(Session.get().isDemo());
		yubikeyForm.setFields(enableYubikey);

		/*
		 * Duo section
		 */
		DynamicForm duoForm = new DynamicForm();
		duoForm.setValuesManager(vm);
		duoForm.setTitleOrientation(TitleOrientation.TOP);
		duoForm.setIsGroup(true);
		duoForm.setGroupTitle("Duo");
		duoForm.setNumCols(1);

		RadioGroupItem enableDuo = ItemFactory.newBooleanSelector("enableDuo", I18N.message("enableduo"));
		enableDuo.setValue("true".equals(settings.get(Constants.TWOFA_DUO + ENABLED)) ? "yes" : "no");
		enableDuo.setWrapTitle(false);
		enableDuo.setWrap(false);
		enableDuo.setRequired(true);
		enableDuo.setDisabled(Session.get().isDemo());

		TextItem duoIntegrationKey = ItemFactory.newTextItem("duoIntegrationKey", I18N.message("integrationkey"),
				settings.get(Constants.TWOFA_DUO + ".integrationkey"));
		duoIntegrationKey.setWidth(350);
		TextItem duoSecretKey = ItemFactory.newPasswordItem("duoSecretKey", I18N.message("secretkey"),
				settings.get(Constants.TWOFA_DUO + ".secretkey"));
		duoSecretKey.setWidth(350);
		TextItem duoApiHostname = ItemFactory.newTextItem("duoApiHostname", I18N.message("apihostname"),
				settings.get(Constants.TWOFA_DUO + ".apihost"));
		duoApiHostname.setWidth(350);

		duoForm.setFields(enableDuo, duoIntegrationKey, duoSecretKey, duoApiHostname);

		IButton save = prepareSaveButton();

		VLayout panel = new VLayout();
		panel.setWidth100();
		panel.setMembersMargin(8);
		panel.setMembers(form, emailForm, googleForm, yubikeyForm, duoForm);

		body.setMembers(panel);
		addMember(save);
	}

	private IButton prepareSaveButton() {
		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				if (Boolean.FALSE.equals(vm.validate()))
					return;

				String tenant = Session.get().getTenantName();
				final List<GUIParameter> params = new ArrayList<>();
				params.add(new GUIParameter(tenant + ".2fa.enabled",
						vm.getValueAsString("enable2fa").equals("yes") ? "true" : FALSE));
				params.add(new GUIParameter(tenant + ".2fa.allowtrusted",
						vm.getValueAsString(ALLOWTRUSTED).equals("yes") ? "true" : FALSE));
				params.add(new GUIParameter(tenant + TWOFA + Constants.TWOFA_GOOGLE_AUTHENTICATOR + ENABLED,
						vm.getValueAsString("enableGoolge").equals("yes") ? "true" : FALSE));
				params.add(new GUIParameter(tenant + TWOFA + Constants.TWOFA_YUBIKEY + ENABLED,
						vm.getValueAsString("enableYubikey").equals("yes") ? "true" : FALSE));
				params.add(new GUIParameter(tenant + TWOFA + Constants.TWOFA_EMAIL_AUTHENTICATOR + ENABLED,
						vm.getValueAsString("enableEmail").equals("yes") ? "true" : FALSE));
				params.add(new GUIParameter(tenant + TWOFA + Constants.TWOFA_DUO + ENABLED,
						vm.getValueAsString("enableDuo").equals("yes") ? "true" : FALSE));
				params.add(new GUIParameter(tenant + TWOFA + Constants.TWOFA_DUO + ".integrationkey",
						vm.getValueAsString("duoIntegrationKey")));
				params.add(new GUIParameter(tenant + TWOFA + Constants.TWOFA_DUO + ".secretkey",
						vm.getValueAsString("duoSecretKey")));
				params.add(new GUIParameter(tenant + TWOFA + Constants.TWOFA_DUO + ".apihost",
						vm.getValueAsString("duoApiHostname")));
				doSaveSettings(params);
			}
		});
		return save;
	}

	private void doSaveSettings(final List<GUIParameter> params) {
		SettingService.Instance.get().saveSettings(params.toArray(new GUIParameter[0]), new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg) {
				Session.get().updateConfig(params);
				GuiLog.info(I18N.message("settingssaved"), null);
			}
		});
	}
}