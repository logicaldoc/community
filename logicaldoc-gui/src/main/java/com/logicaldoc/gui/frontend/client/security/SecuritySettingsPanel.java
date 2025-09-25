package com.logicaldoc.gui.frontend.client.security;

import java.util.Map;

import com.google.gwt.core.client.GWT;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUISecuritySettings;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.common.client.widgets.PasswordGenerator;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.system.SessionsPanel;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.ToggleItem;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel shows general security parameters.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class SecuritySettingsPanel extends AdminPanel {

	private static final String ALLOWCLIENTID = "allowclientid";

	private static final String ENABLEANONYMOUS = "enableanonymous";

	private static final String CONTENTSECURITYPOLICY = "contentsecuritypolicy";

	private static final String PWD_OCCURRENCE = "pwdOccurrence";

	private static final String PWD_SEQUENCE = "pwdSequence";

	private static final String PWD_SPECIAL = "pwdSpecial";

	private static final String PWD_DIGIT = "pwdDigit";

	private static final String PWD_LOWER_CASE = "pwdLowerCase";

	private static final String PWD_UPPER_CASE = "pwdUpperCase";

	private static final String PWD_SIZE = "pwdSize";
	
	private static final String PWD_CHECKLOGIN = "pwdCheckLogin";

	private ValuesManager vm = new ValuesManager();

	private GUISecuritySettings settings;

	private SelectItem anonymousUser;

	public SecuritySettingsPanel(GUISecuritySettings settings) {
		super("security");
		this.settings = settings;
	}

	@Override
	public void onDraw() {
		DynamicForm passwordForm = new DynamicForm();
		passwordForm.setIsGroup(true);
		passwordForm.setGroupTitle(I18N.message("passwordrules"));
		passwordForm.setNumCols(4);
		passwordForm.setWidth(1);
		passwordForm.setValuesManager(vm);

		final SpinnerItem pwdSize = ItemFactory.newSpinnerItem(PWD_SIZE, "size", settings.getPwdSize());
		pwdSize.setWrapTitle(false);
		pwdSize.setRequired(true);
		pwdSize.setWidth(50);
		pwdSize.setMin(6);
		pwdSize.setStep(1);

		final SpinnerItem pwdExp = ItemFactory.newSpinnerItem("pwdExp", "expiration", settings.getPwdExpiration());
		pwdExp.setHint(I18N.message("days"));
		pwdExp.setWrapTitle(false);
		pwdExp.setRequired(true);
		pwdExp.setWidth(50);
		pwdExp.setMin(0);
		pwdExp.setStep(10);

		final SpinnerItem pwdEnforce = ItemFactory.newSpinnerItem("pwdEnforce", "enforcepasswordhistory",
				settings.getPwdEnforceHistory());
		pwdEnforce.setHint(I18N.message("passwords"));
		pwdEnforce.setWrapTitle(false);
		pwdEnforce.setRequired(true);
		pwdEnforce.setWidth(50);
		pwdEnforce.setMin(0);
		pwdEnforce.setStep(1);

		final SpinnerItem pwUpperCase = ItemFactory.newSpinnerItem(PWD_UPPER_CASE, "uppercasechars",
				settings.getPwdUpperCase());
		pwUpperCase.setRequired(true);
		pwUpperCase.setWrapTitle(false);
		pwUpperCase.setWidth(50);
		pwUpperCase.setMin(1);
		pwUpperCase.setStep(1);

		final SpinnerItem pwLowerCase = ItemFactory.newSpinnerItem(PWD_LOWER_CASE, "lowercasechars",
				settings.getPwdLowerCase());
		pwLowerCase.setRequired(true);
		pwLowerCase.setWrapTitle(false);
		pwLowerCase.setWidth(50);
		pwLowerCase.setMin(1);
		pwLowerCase.setStep(1);

		final SpinnerItem pwdDigit = ItemFactory.newSpinnerItem(PWD_DIGIT, "digitchars", settings.getPwdDigit());
		pwdDigit.setRequired(true);
		pwdDigit.setWrapTitle(false);
		pwdDigit.setWidth(50);
		pwdDigit.setMin(1);
		pwdDigit.setStep(1);

		final SpinnerItem pwdSpecial = ItemFactory.newSpinnerItem(PWD_SPECIAL, "specialchars",
				settings.getPwdSpecial());
		pwdSpecial.setRequired(true);
		pwdSpecial.setWrapTitle(false);
		pwdSpecial.setWidth(50);
		pwdSpecial.setMin(1);
		pwdSpecial.setStep(1);

		final SpinnerItem pwdSequence = ItemFactory.newSpinnerItem(PWD_SEQUENCE, "maxsequencesize",
				settings.getPwdSequence());
		pwdSequence.setRequired(true);
		pwdSequence.setWrapTitle(false);
		pwdSequence.setWidth(50);
		pwdSequence.setMin(3);
		pwdSequence.setStep(1);

		final SpinnerItem pwdOccurrence = ItemFactory.newSpinnerItem(PWD_OCCURRENCE, "maxoccurrences",
				settings.getPwdOccurrence());
		pwdOccurrence.setRequired(true);
		pwdOccurrence.setWrapTitle(false);
		pwdOccurrence.setWidth(50);
		pwdOccurrence.setMin(1);
		pwdOccurrence.setStep(1);

		final ToggleItem pwdCheckLogin = ItemFactory.newToggleItem(PWD_CHECKLOGIN, "checkpwdlogin",
				settings.isPwdCheckLogin());
		pwdCheckLogin.setHint(I18N.message("checkpwdloginhint"));
		pwdCheckLogin.setWrapTitle(false);
		pwdCheckLogin.setWrapHintText(false);
		pwdCheckLogin.setRequired(true);

		ButtonItem generatePassword = prepareGeneratePasswordButton(passwordForm);

		ButtonItem tryPassword = prepareTryPasswordButton(passwordForm);

		passwordForm.setItems(pwdSize, pwdDigit, pwUpperCase, pwdSpecial, pwLowerCase, pwdSequence, pwdOccurrence,
				pwdExp, pwdEnforce, pwdCheckLogin, generatePassword, tryPassword);

		DynamicForm securityForm = new DynamicForm();
		securityForm.setValuesManager(vm);
		securityForm.setTitleOrientation(TitleOrientation.TOP);
		securityForm.setNumCols(1);

		final SpinnerItem maxInactivity = ItemFactory.newSpinnerItem("maxinactivity", settings.getMaxInactivity());
		maxInactivity.setRequired(false);
		maxInactivity.setHint(I18N.message("daysafteruserdisabled"));
		maxInactivity.setWidth(50);
		maxInactivity.setStep(1);

		ToggleItem savelogin = ItemFactory.newToggleItem("savelogin", settings.isSaveLogin());
		savelogin.setHint(I18N.message("saveloginhint"));
		savelogin.setWrapTitle(false);
		savelogin.setRequired(true);

		ToggleItem ignorelogincase = ItemFactory.newToggleItem("ignorelogincase", settings.isIgnoreLoginCase());
		ignorelogincase.setWrapTitle(false);
		ignorelogincase.setRequired(true);

		ToggleItem alertnewdevice = ItemFactory.newToggleItem("alertnewdevice", I18N.message("alertloginfromnewdevice"),
				settings.isAlertNewDevice());
		alertnewdevice.setWrapTitle(false);
		alertnewdevice.setRequired(true);

		ToggleItem allowSid = ItemFactory.newToggleItem("allowsid", I18N.message("allowsidinrequest"),
				settings.isAllowSidInRequest());
		allowSid.setWrapTitle(false);
		allowSid.setRequired(true);
		allowSid.setDisabled(Session.get().isDemo());

		ToggleItem allowClientId = ItemFactory.newToggleItem(ALLOWCLIENTID, I18N.message(ALLOWCLIENTID),
				settings.isAllowClientId());
		allowClientId.setWrapTitle(false);
		allowClientId.setRequired(true);
		allowClientId.setDisabled(Session.get().isDemo());

		final SelectItem cookiesSameSite = ItemFactory.newSelectItem("cookiessamesite");
		cookiesSameSite.setHint(I18N.message("cookiessamesitehint"));
		cookiesSameSite.setWidth(90);
		cookiesSameSite.setWrapTitle(false);
		cookiesSameSite.setRequired(true);
		cookiesSameSite.setDisabled(Session.get().isDemo());
		cookiesSameSite.setValueMap("unset", "lax", "strict");
		cookiesSameSite.setValue(settings.getCookiesSameSite());

		ToggleItem secureCookies = ItemFactory.newToggleItem("secureCookies", I18N.message("usesecurecookies"),
				settings.isCookiesSecure());
		secureCookies.setWrapTitle(false);
		secureCookies.setRequired(true);
		secureCookies.setDisabled(Session.get().isDemo());

		ToggleItem forceSsl = ItemFactory.newToggleItem("forcessl", settings.isForceSsl());
		forceSsl.setWrapTitle(false);
		forceSsl.setRequired(true);
		forceSsl.setDisabled(Session.get().isDemo());

		final TextAreaItem contentSecurityPolicy = ItemFactory.newTextAreaItem(CONTENTSECURITYPOLICY,
				settings.getContentSecurityPolicy());
		contentSecurityPolicy.setHint(I18N.message("contentsecuritypolicyhint"));
		contentSecurityPolicy.setWidth(450);
		contentSecurityPolicy.setHeight(150);

		if (Session.get().isDefaultTenant())
			securityForm.setFields(maxInactivity, savelogin, alertnewdevice, ignorelogincase, allowSid, allowClientId,
					cookiesSameSite, secureCookies, forceSsl, contentSecurityPolicy);
		else
			securityForm.setFields(maxInactivity, savelogin, alertnewdevice);

		body.setMembers(passwordForm, securityForm);

		Tab menus = new Tab();
		menus.setTitle(I18N.message("menus"));
		menus.setPane(new MenusPanel());
		tabs.addTab(menus);

		Tab anonymous = prepareAnonymousTab(settings);
		tabs.addTab(anonymous);

		if (Feature.visible(Feature.GEOLOCATION)) {
			Tab geolocation = prepareGeolocationTab(settings);
			tabs.addTab(geolocation);
		}

		Tab sessions = new Tab();
		sessions.setTitle(I18N.message("sessions"));
		sessions.setPane(new SessionsPanel());
		if (Menu.enabled(Menu.ADMIN_SESSIONS))
			tabs.addTab(sessions);

		addSaveButton();
	}

	private void addSaveButton() {
		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler((ClickEvent event) -> {
			if (Boolean.FALSE.equals(vm.validate()))
				return;

			save();
		});
		addMember(save);
	}

	private void save() {
		SecuritySettingsPanel.this.settings.setPwdExpiration((Integer) vm.getValue("pwdExp"));
		SecuritySettingsPanel.this.settings.setPwdSize((Integer) vm.getValue(PWD_SIZE));
		SecuritySettingsPanel.this.settings.setPwdUpperCase((Integer) vm.getValue(PWD_UPPER_CASE));
		SecuritySettingsPanel.this.settings.setPwdLowerCase((Integer) vm.getValue(PWD_LOWER_CASE));
		SecuritySettingsPanel.this.settings.setPwdDigit((Integer) vm.getValue(PWD_DIGIT));
		SecuritySettingsPanel.this.settings.setPwdSpecial((Integer) vm.getValue(PWD_SPECIAL));
		SecuritySettingsPanel.this.settings.setPwdSequence((Integer) vm.getValue(PWD_SEQUENCE));
		SecuritySettingsPanel.this.settings.setPwdOccurrence((Integer) vm.getValue(PWD_OCCURRENCE));
		SecuritySettingsPanel.this.settings.setPwdEnforceHistory((Integer) vm.getValue("pwdEnforce"));
		SecuritySettingsPanel.this.settings.setPwdCheckLogin((Boolean) vm.getValue(PWD_CHECKLOGIN));
		SecuritySettingsPanel.this.settings.setMaxInactivity((Integer) vm.getValue("maxinactivity"));
		SecuritySettingsPanel.this.settings.setSaveLogin(Boolean.valueOf(vm.getValueAsString("savelogin")));
		SecuritySettingsPanel.this.settings
				.setEnableAnonymousLogin(Boolean.valueOf(vm.getValueAsString(ENABLEANONYMOUS)));
		SecuritySettingsPanel.this.settings.setAlertNewDevice(Boolean.valueOf(vm.getValueAsString("alertnewdevice")));
		SecuritySettingsPanel.this.settings.setAnonymousKey(vm.getValueAsString("anonymousKey"));

		if (!SecuritySettingsPanel.this.settings.isEnableAnonymousLogin()) {
			SecuritySettingsPanel.this.settings.setAnonymousUser(null);
			anonymousUser.setValue((Long) null);
		} else if (SecuritySettingsPanel.this.settings.getAnonymousUser() == null) {
			SC.warn(I18N.message("selectanonymoususer"));
			return;
		}

		collectDefaultTenantSettings();

		doSaveSettings();
	}

	private void collectDefaultTenantSettings() {
		if (Session.get().isDefaultTenant()) {
			SecuritySettingsPanel.this.settings.setAllowSidInRequest(Boolean.valueOf(vm.getValueAsString("allowsid")));
			SecuritySettingsPanel.this.settings.setAllowClientId(Boolean.valueOf(vm.getValueAsString(ALLOWCLIENTID)));
			SecuritySettingsPanel.this.settings
					.setIgnoreLoginCase(Boolean.valueOf(vm.getValueAsString("ignorelogincase")));
			SecuritySettingsPanel.this.settings.setCookiesSecure(Boolean.valueOf(vm.getValueAsString("secureCookies")));
			SecuritySettingsPanel.this.settings.setCookiesSameSite(vm.getValueAsString("cookiessamesite"));
			SecuritySettingsPanel.this.settings.setForceSsl(Boolean.valueOf(vm.getValueAsString("forcessl")));
			SecuritySettingsPanel.this.settings.setContentSecurityPolicy(vm.getValueAsString(CONTENTSECURITYPOLICY));
			SecuritySettingsPanel.this.settings
					.setGeolocationEnabled(Boolean.valueOf(vm.getValueAsString("geoEnabled")));
			SecuritySettingsPanel.this.settings.setGeolocationCache(Boolean.valueOf(vm.getValueAsString("geoCache")));
			SecuritySettingsPanel.this.settings.setGeolocationKey(vm.getValueAsString("geoKey"));
		}
	}

	private void doSaveSettings() {
		SecurityService.Instance.get().saveSettings(SecuritySettingsPanel.this.settings, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(Boolean restartRequired) {
				GuiLog.info(I18N.message("settingssaved"), null);

				if (restartRequired.booleanValue())
					SC.warn(I18N.message("needrestart"));
			}
		});
	}

	private ButtonItem prepareGeneratePasswordButton(DynamicForm passwordForm) {
		ButtonItem generatePassword = new ButtonItem(I18N.message("generate"));
		generatePassword.setStartRow(false);
		generatePassword.setColSpan(4);
		generatePassword.setAlign(Alignment.RIGHT);
		generatePassword.addClickHandler(event -> {
			if (passwordForm.validate()) {
				@SuppressWarnings("unchecked")
				final Map<String, Object> values = vm.getValues();
				PasswordGenerator generator = new PasswordGenerator((Integer) values.get(PWD_SIZE),
						(Integer) values.get(PWD_UPPER_CASE), (Integer) values.get(PWD_LOWER_CASE),
						(Integer) values.get(PWD_DIGIT), (Integer) values.get(PWD_SPECIAL),
						(Integer) values.get(PWD_SEQUENCE), (Integer) values.get(PWD_OCCURRENCE));
				generator.show();
			}
		});
		return generatePassword;
	}

	private ButtonItem prepareTryPasswordButton(DynamicForm passwordForm) {
		ButtonItem tryPassword = new ButtonItem(I18N.message("tryapassword"));
		tryPassword.setStartRow(false);
		tryPassword.setColSpan(4);
		tryPassword.setAlign(Alignment.RIGHT);
		tryPassword.addClickHandler(event -> {
			if (passwordForm.validate()) {
				@SuppressWarnings("unchecked")
				final Map<String, Object> values = vm.getValues();
				PasswordTrial trial = new PasswordTrial((Integer) values.get(PWD_SIZE),
						(Integer) values.get(PWD_UPPER_CASE), (Integer) values.get(PWD_LOWER_CASE),
						(Integer) values.get(PWD_DIGIT), (Integer) values.get(PWD_SPECIAL),
						(Integer) values.get(PWD_SEQUENCE), (Integer) values.get(PWD_OCCURRENCE));
				trial.show();
			}
		});
		return tryPassword;
	}

	private Tab prepareGeolocationTab(GUISecuritySettings settings) {
		Tab geolocation = new Tab(I18N.message("geolocation"));

		if (!Feature.enabled(Feature.GEOLOCATION)) {
			geolocation.setPane(new FeatureDisabled());
			return geolocation;
		}

		DynamicForm geolocationForm = new DynamicForm();
		geolocationForm.setValuesManager(vm);
		geolocationForm.setTitleOrientation(TitleOrientation.TOP);
		geolocationForm.setNumCols(1);

		ToggleItem enableGeolocation = ItemFactory.newToggleItem("geoEnabled", I18N.message("enablegeolocation"),
				settings.isGeolocationEnabled());
		enableGeolocation.setWrapTitle(false);
		enableGeolocation.setRequired(true);

		ToggleItem useCache = ItemFactory.newToggleItem("geoCache", I18N.message("usecache"),
				settings.isGeolocationCache());
		useCache.setWrapTitle(false);
		useCache.setRequired(true);

		final TextItem licenseKey = ItemFactory.newTextItem("geoKey", "licensekey", settings.getGeolocationKey());
		licenseKey.setWidth(200);
		licenseKey.setHint(I18N.message("geolocationkeyhint"));

		final StaticTextItem geoDBversion = ItemFactory.newStaticTextItem("geoVersion", "dbversion",
				settings.getGeolocationDbVer());

		ButtonItem syncGeoDB = new ButtonItem("geoSyncDb", I18N.message("syncgeolocationdb"));
		syncGeoDB.addClickHandler((com.smartgwt.client.widgets.form.fields.events.ClickEvent event) -> {
			LD.contactingServer();
			SecurityService.Instance.get().syncGeolocationDB(licenseKey.getValueAsString(),
					new DefaultAsyncCallback<>() {

						@Override
						public void onFailure(Throwable caught) {
							super.onFailure(caught);
							LD.clearPrompt();
						}

						@Override
						public void onSuccess(String dbVer) {
							geoDBversion.setValue(dbVer);
							LD.clearPrompt();
						}
					});
		});

		geolocationForm.setFields(enableGeolocation, useCache, licenseKey, geoDBversion, syncGeoDB);

		geolocation.setPane(geolocationForm);
		return geolocation;
	}

	private Tab prepareAnonymousTab(GUISecuritySettings settings) {
		Tab anonymous = new Tab(I18N.message("anonymous"));

		DynamicForm anonymousForm = new DynamicForm();
		anonymousForm.setValuesManager(vm);
		anonymousForm.setTitleOrientation(TitleOrientation.TOP);
		anonymousForm.setNumCols(1);
		ToggleItem enableAnonymous = ItemFactory.newToggleItem(ENABLEANONYMOUS, settings.isEnableAnonymousLogin());
		enableAnonymous.setWrapTitle(false);
		enableAnonymous.setRequired(true);

		final StaticTextItem url = ItemFactory.newStaticTextItem("anonUrl", I18N.message("url"),
				GWT.getHostPageBaseURL() + "frontend.jsp?anonymous=login&tenant=" + Session.get().getTenantName()
						+ "&key=" + settings.getAnonymousKey());

		TextItem anonymousKey = ItemFactory.newSimpleTextItem("anonymousKey", "key", settings.getAnonymousKey());
		anonymousKey.setHintStyle("hint");
		anonymousKey.setRequired(true);
		anonymousKey.addChangedHandler(changed -> {
			if (changed.getValue() != null)
				url.setValue(Util.contextPath() + "frontend.jsp?anonymous=login&tenant=" + Session.get().getTenantName()
						+ "&key=" + changed.getValue().toString());
		});

		anonymousUser = ItemFactory.newUserSelector("anonymousUser", "user", null, false, false);
		anonymousUser.setHint(I18N.message("anonymoususerhint"));
		anonymousUser.setHintStyle("hint");
		anonymousUser.addChangedHandler(event -> {
			if (anonymousUser.getSelectedRecord() == null) {
				SecuritySettingsPanel.this.settings.setAnonymousUser(null);
			} else {
				GUIUser u = new GUIUser();
				u.setId(Long.parseLong(anonymousUser.getSelectedRecord().getAttribute("id")));
				u.setUsername(anonymousUser.getSelectedRecord().getAttribute("username"));
				SecuritySettingsPanel.this.settings.setAnonymousUser(u);
			}
		});
		if (SecuritySettingsPanel.this.settings.getAnonymousUser() != null)
			anonymousUser.setValue(Long.toString(SecuritySettingsPanel.this.settings.getAnonymousUser().getId()));

		anonymousForm.setItems(enableAnonymous, anonymousUser, anonymousKey, url);
		anonymous.setPane(anonymousForm);
		return anonymous;
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