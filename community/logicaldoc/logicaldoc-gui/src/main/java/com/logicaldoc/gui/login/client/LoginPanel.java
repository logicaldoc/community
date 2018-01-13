package com.logicaldoc.gui.login.client;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.google.gwt.core.client.GWT;
import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.RequestException;
import com.google.gwt.http.client.Response;
import com.google.gwt.http.client.URL;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIInfo;
import com.logicaldoc.gui.common.client.beans.GUIMessage;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.RequestInfo;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.common.client.widgets.MessageLabel;
import com.logicaldoc.gui.login.client.services.LoginService;
import com.logicaldoc.gui.login.client.services.LoginServiceAsync;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpacerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.KeyPressEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * The panel showing the login form
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class LoginPanel extends VLayout {

	protected static final int FORM_WIDTH = 280;

	private static final int COLUMN_WIDTH = FORM_WIDTH + 20;

	protected static final String PARAM_SUCCESSURL = "j_successurl";

	protected static final String PARAM_FAILUREURL = "j_failureurl";

	protected LoginServiceAsync loginService = (LoginServiceAsync) GWT.create(LoginService.class);

	protected TextItem username = new TextItem();

	protected PasswordItem password = new PasswordItem();

	protected TextItem secretKey = new TextItem();

	protected SelectItem language;

	protected CheckboxItem rememberMe = new CheckboxItem();

	protected VLayout mainPanel = null;

	// Flag used to handle double clicks on the login button
	protected static boolean loggingIn = false;

	protected DynamicForm form = new DynamicForm();

	protected GUIInfo info = null;

	public LoginPanel(GUIInfo info) {
		setDefaultLayoutAlign(Alignment.CENTER);
		setWidth100();
		setHeight100();
		setMembersMargin(0);

		this.info = info;
	}

	protected void initGUI(boolean saveLoginEnabled) {
		HLayout spacer15 = new HLayout();
		spacer15.setHeight(15);
		spacer15.setWidth(15);

		HLayout spacer10 = new HLayout();
		spacer10.setHeight(10);
		spacer10.setWidth(10);

		// Prepare the logo to show on the top of the page
		Img logoTop = ItemFactory.newBrandImg(info.getBranding().isLogoOemCustomized() ? "logo_head.png"
				: "logo_oem.png", info);
		logoTop.setWidth(205);
		logoTop.setHeight(40);

		/*
		 * This panel stays on top of the page
		 */
		HLayout top = new HLayout();
		top.setMargin(10);
		top.setHeight(60);
		top.setMembersMargin(0);
		top.setMembers(logoTop);

		// Prepare the logo to show in the login form
		Img logoLogin = ItemFactory.newBrandImg(
				!info.getBranding().isLogoOemCustomized() ? "logo.png" : "logo_oem.png", info);
		logoLogin.setWidth(205);
		logoLogin.setHeight(40);
		logoLogin.setAlign(Alignment.CENTER);
		logoLogin.setLayoutAlign(Alignment.CENTER);

		/*
		 * Panel containing product name and version
		 */
		String productInfoHtml = "<b>" + info.getBranding().getProductName() + " " + info.getRelease() + "</b>";
		if (info.getBranding().getUrl() != null && !"-".equals(info.getBranding().getUrl()))
			productInfoHtml = "<a href='" + info.getBranding().getUrl() + "' target='_blank' class='login-link'>"
					+ productInfoHtml + "</a>";
		HTMLFlow productInfo = new HTMLFlow(productInfoHtml);
		productInfo.setHeight(16);
		productInfo.setWidth(COLUMN_WIDTH);
		productInfo.setStyleName("login-product");

		// Prepare the Form and all its fields
		form = new DynamicForm();
		form.setAlign(Alignment.CENTER);
		form.setWidth(FORM_WIDTH);
		form.setNumCols(saveLoginEnabled ? 3 : 1);
		form.setTitleWidth(0);
		form.setMargin(0);
		form.setCellPadding(0);

		username.setTitle(I18N.message("username"));
		username.setShowTitle(false);
		username.setHint(I18N.message("username").toLowerCase());
		username.setShowHintInField(true);
		username.setWrapTitle(false);
		username.setRequired(true);
		username.setHeight(34);
		username.setWidth(FORM_WIDTH);
		username.setAlign(Alignment.LEFT);
		username.setTextBoxStyle("login-field");
		username.setColSpan(saveLoginEnabled ? 3 : 1);
		username.addKeyPressHandler(new KeyPressHandler() {
			@Override
			public void onKeyPress(KeyPressEvent event) {
				if (event.getKeyName() != null && "enter".equals(event.getKeyName().toLowerCase()))
					onSigninClicked();
			}
		});

		password = ItemFactory.newPasswordItem("password", "password", null);
		password.setShowTitle(false);
		password.setHint(I18N.message("password").toLowerCase());
		password.setShowHintInField(true);
		password.setRequired(true);
		password.setHeight(34);
		password.setWidth(FORM_WIDTH);
		password.setTextBoxStyle("login-field");
		password.setAlign(Alignment.LEFT);
		password.setWrapTitle(false);
		password.setColSpan(saveLoginEnabled ? 3 : 1);
		password.addKeyPressHandler(new KeyPressHandler() {
			@Override
			public void onKeyPress(KeyPressEvent event) {
				if (event.getKeyName() != null && "enter".equals(event.getKeyName().toLowerCase()))
					onSigninClicked();
			}
		});

		secretKey.setTitle(I18N.message("secretkey"));
		secretKey.setShowTitle(false);
		secretKey.setHint(I18N.message("secretkey").toLowerCase());
		secretKey.setShowHintInField(true);
		secretKey.setWrapTitle(false);
		secretKey.setHeight(34);
		secretKey.setWidth(FORM_WIDTH);
		secretKey.setAlign(Alignment.LEFT);
		secretKey.setTextBoxStyle("secretkey-field");
		secretKey.setColSpan(saveLoginEnabled ? 3 : 1);
		secretKey.addKeyPressHandler(new KeyPressHandler() {
			@Override
			public void onKeyPress(KeyPressEvent event) {
				if (event.getKeyName() != null && "enter".equals(event.getKeyName().toLowerCase()))
					onSigninClicked();
			}
		});

		language = ItemFactory.newLanguageSelector("language", true, true);
		language.setShowTitle(false);
		language.setDefaultValue("");
		language.setControlStyle("login-language");
		language.setWidth(FORM_WIDTH - 4);
		language.setHeight(34);
		language.setAlign(Alignment.LEFT);
		language.setHint(I18N.message("chooseyourlanguage"));
		language.setShowHintInField(true);
		language.setControlStyle("login-language");
		language.setTextBoxStyle("login-language-text");
		language.setPickerIconStyle("login-language-picker");
		language.setColSpan(saveLoginEnabled ? 3 : 1);

		RequestInfo request = WindowUtils.getRequestInfo();

		// If a parameter specifies a locale, we initialize the language
		// selector
		if (request.getParameter(Constants.LOCALE) != null && !request.getParameter(Constants.LOCALE).equals("")) {
			String lang = request.getParameter(Constants.LOCALE);
			Map<String, String> languages = I18N.getSupportedGuiLanguages(false);
			for (String l : languages.keySet()) {
				if (lang.equals(l)) {
					language.setValue(l);
					break;
				}
			}
		}

		SpacerItem spacerItem12 = new SpacerItem();
		spacerItem12.setHeight(12);
		spacerItem12.setColSpan(saveLoginEnabled ? 3 : 1);

		ButtonItem signIn = new ButtonItem(I18N.message("signin"));
		signIn.setBaseStyle("btn");
		signIn.setHoverStyle("btn");
		signIn.setHeight(34);
		signIn.setAlign(Alignment.RIGHT);
		signIn.setStartRow(false);
		signIn.setWidth(saveLoginEnabled ? 120 : FORM_WIDTH);
		signIn.setColSpan(saveLoginEnabled ? 1 : 3);
		signIn.addClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {
			@Override
			public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
				onSigninClicked();
			}
		});

		rememberMe.setTitle(I18N.message("rememberme"));
		rememberMe.setRequired(false);
		rememberMe.setShowTitle(false);
		rememberMe.setValue(CookiesManager.isSaveLogin());
		rememberMe.setTextBoxStyle("login-field");
		rememberMe.setAlign(Alignment.LEFT);
		rememberMe.setEndRow(false);
		rememberMe.setColSpan(2);

		List<FormItem> formItems = new ArrayList<FormItem>();
		formItems.add(username);
		formItems.add(spacerItem12);
		formItems.add(password);
		formItems.add(spacerItem12);

		if (Feature.enabled(Feature.TWO_FACTORS_AUTHENTICATION) && "true".equals(info.getConfig("2fa.enabled"))) {
			formItems.add(secretKey);
			formItems.add(spacerItem12);
		}
		formItems.add(language);
		formItems.add(spacerItem12);
		if (saveLoginEnabled)
			formItems.add(rememberMe);
		formItems.add(signIn);

		form.setItems(formItems.toArray(new FormItem[0]));

		HTMLFlow lostPassword = new HTMLFlow("<div><a href=\"javascript:showLostDialog('"
				+ info.getBranding().getProductName() + "')\" class='login-lost'>" + I18N.message("lostpassword")
				+ "</a></div>");
		lostPassword.setLayoutAlign(Alignment.RIGHT);
		lostPassword.setHoverDelay(0);
		lostPassword.setMargin(0);
		lostPassword.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				showLostDialog(info.getBranding().getProductName());
			}
		});

		/*
		 * Prepare the licensing canvas
		 */
		String copyrightHtml = "<div>\u00A9 " + info.getYear() + " " + info.getBranding().getVendor();
		if (info.getBranding().getUrl() != null && !"-".equals(info.getBranding().getUrl()))
			copyrightHtml = "<a href='" + info.getBranding().getUrl()
					+ "' target='_blank' class='login-copyright-link'>" + copyrightHtml + "</a></div>";
		String licenseeHtml = "";
		if (info.getLicensee() != null && !"".equals(info.getLicensee().trim()))
			licenseeHtml += "<div>" + I18N.message("licensedto") + " <b>" + info.getLicensee() + "</b></div>";

		HTMLFlow licensing = new HTMLFlow(copyrightHtml + licenseeHtml);
		licensing.setStyleName("login-copyright");

		// Panel containing the inputs
		VLayout inputsForm = new VLayout();
		inputsForm.setMargin(0);
		inputsForm.setWidth(FORM_WIDTH);
		inputsForm.setStyleName("control-group");
		inputsForm.setMembers(logoLogin, spacer15, form, lostPassword, licensing);

		// Panel containing the login form
		VLayout loginForm = new VLayout();
		loginForm.setLayoutAlign(VerticalAlignment.TOP);
		loginForm.setLayoutAlign(Alignment.CENTER);
		loginForm.setAlign(VerticalAlignment.TOP);
		loginForm.setMembersMargin(0);
		loginForm.setMargin(0);
		loginForm.setWidth(FORM_WIDTH);
		loginForm.setStyleName("login-form");
		loginForm.setMembers(inputsForm);

		// The login screen
		VLayout loginScreen = new VLayout();
		loginScreen.setMembersMargin(0);
		loginScreen.setMargin(0);
		loginScreen.setStyleName("login-screen");
		loginScreen.setMembers(loginForm);

		// The center column with login screen and product info
		VLayout centerColumn = new VLayout();
		centerColumn.setMembersMargin(0);
		centerColumn.setMargin(0);
		centerColumn.setWidth(COLUMN_WIDTH);
		centerColumn.setHeight(330);
		centerColumn.setLayoutAlign(VerticalAlignment.TOP);
		centerColumn.setLayoutAlign(Alignment.CENTER);
		centerColumn.setAlign(VerticalAlignment.TOP);
		centerColumn.setMembers(productInfo, loginScreen);

		// Main Panel (covers 100% of the screen)
		mainPanel = new VLayout();
		mainPanel.setLayoutAlign(VerticalAlignment.TOP);
		mainPanel.setMembers(top, centerColumn);
		addMember(mainPanel);

		prepareAlerts();

		prepareSwitchViewLink();

		// If the case, initialize the credentials from client's cookies
		if (saveLoginEnabled && rememberMe.getValueAsBoolean()) {
			String[] credentials = CookiesManager.getSavedCredentials();
			username.setValue(credentials[0]);
			password.setValue(credentials[1]);
		}
	}

	protected void initGUI() {
		boolean saveLoginEnabled = "true".equals(info.getConfig("gui.savelogin"));
		initGUI(saveLoginEnabled);
	}

	protected void prepareSwitchViewLink() {
		String url = "mobile".equals(Util.getJavascriptVariable("j_layout")) ? (Util.contextPath() + "login.jsp")
				: (Util.contextPath() + "login-mobile.jsp");
		url += "?tenant=" + Util.detectTenant();
		String label = "mobile".equals(Util.getJavascriptVariable("j_layout")) ? I18N.message("viewclassicweb") : I18N
				.message("viewmobileweb");

		/*
		 * A link to the alternative login page
		 */
		HTMLFlow switchLink = new HTMLFlow("<a href='" + url + "' class='login-switchview'>" + label + "</a>");
		switchLink.setHeight(16);
		switchLink.setWidth(COLUMN_WIDTH + 20);
		switchLink.setStyleName("login-switchview");

		HLayout spacer10 = new HLayout();
		spacer10.setHeight(10);
		spacer10.setWidth(10);

		VLayout link = new VLayout();
		link.setMembersMargin(0);
		link.setMargin(0);
		link.setStyleName("login-switchview");
		link.setMembers(spacer10, switchLink);
		link.setLayoutAlign(VerticalAlignment.TOP);
		link.setLayoutAlign(Alignment.CENTER);
		link.setWidth(COLUMN_WIDTH + 20);
		link.setHeight(18);

		mainPanel.addMember(link);
	}

	/**
	 * Prepares the panel to show messages
	 */
	protected void prepareAlerts() {
		List<MessageLabel> messages = new ArrayList<MessageLabel>();
		if (info.getAlerts() != null && info.getAlerts().length > 0) {
			for (GUIMessage alert : info.getAlerts()) {
				MessageLabel label = new MessageLabel(alert, info.getTenant().getId() == 1L);
				label.setStyleName("loginMemesage");
				if (alert.isShowInLogin()) {
					messages.add(label);
				}
			}
		}

		if (info.getRunLevel().equals("demo")) {
			GUIMessage demoRunLevelMessage = new GUIMessage();
			demoRunLevelMessage.setMessage(I18N.message("demomode"));
			demoRunLevelMessage.setPriority(GUIMessage.PRIO_INFO);
			MessageLabel demoRunLevel = new MessageLabel(demoRunLevelMessage, info.getTenant().getId() == 1L);
			messages.add(demoRunLevel);
		} else if (info.getRunLevel().equals("updated")) {
			GUIMessage confirmUpdateMessage = new GUIMessage();
			confirmUpdateMessage.setMessage(I18N.message("systemupdatedwarning", info.getRelease()));
			confirmUpdateMessage.setPriority(GUIMessage.PRIO_WARN);
			MessageLabel updatedRunLevel = new MessageLabel(confirmUpdateMessage, info.getTenant().getId() == 1L);
			messages.add(updatedRunLevel);
		}

		HLayout spacer15 = new HLayout();
		spacer15.setHeight(15);
		spacer15.setWidth(15);

		// The messages screen
		VLayout messagesScreen = new VLayout();
		messagesScreen.setMembersMargin(0);
		messagesScreen.setMargin(0);
		messagesScreen.setStyleName("login-screen");
		messagesScreen.setMembers(messages.toArray(new MessageLabel[0]));
		messagesScreen.setLayoutAlign(VerticalAlignment.TOP);
		messagesScreen.setLayoutAlign(Alignment.CENTER);
		messagesScreen.setWidth(COLUMN_WIDTH + 20);
		messagesScreen.setHeight(1);

		mainPanel.addMember(spacer15);

		if (!messages.isEmpty())
			mainPanel.addMember(messagesScreen);
	}

	protected void onSigninClicked() {
		if (!form.validate()) {
			onAuthenticationFailure();
			return;
		}

		if (loggingIn == true)
			return;
		else
			loggingIn = true;

		CookiesManager.removeLogin();

		RequestBuilder builder = new RequestBuilder(RequestBuilder.POST, Util.contextPath() + "j_spring_security_check");
		builder.setHeader("Content-type", "application/x-www-form-urlencoded");
		try {
			String data = "j_username=" + URL.encodeQueryString((String) username.getValue());
			data += "&j_password=" + URL.encodeQueryString((String) password.getValue());
			if (secretKey.getValue() != null)
				data += "&j_secretkey=" + URL.encodeQueryString((String) secretKey.getValue());
			data += "&" + PARAM_SUCCESSURL + "=" + URL.encodeQueryString(Util.getJavascriptVariable(PARAM_SUCCESSURL));
			data += "&" + PARAM_FAILUREURL + "=" + URL.encodeQueryString(Util.getJavascriptVariable(PARAM_FAILUREURL));

			builder.sendRequest(data, new RequestCallback() {
				public void onError(Request request, Throwable exception) {
					loggingIn = false;
					onAuthenticationFailure();
				}

				public void onResponseReceived(Request request, Response response) {
					loggingIn = false;
					if (response.getStatusCode() == 200) {
						onAuthenticationSuccess();
					} else {
						onAuthenticationFailure();
					}
				}
			});
		} catch (RequestException e) {
			SC.warn("Login request error: " + e.getMessage());
		}
	}

	public static void showLostDialog(String productName) {
		ResetPassword pwdReset = new ResetPassword(productName);
		pwdReset.show();
	}

	protected void onAuthenticationSuccess() {
		boolean saveLoginEnabled = "true".equals(info.getConfig("gui.savelogin"));
		CookiesManager.saveLogin(saveLoginEnabled, rememberMe.getValueAsBoolean(), username.getValueAsString(),
				password.getValueAsString());
		Util.redirectToSuccessUrl(language.getValueAsString());
	}

	protected void onAuthenticationFailure() {
		final String failure = CookiesManager.getFailure();
		CookiesManager.removeLogin();

		if (failure != null && !"".equals(failure)) {
			if ("usernameblocked".equals(failure)) {
				SC.warn(I18N.message("usernameblockedwarn", info.getConfig("throttle.username.wait")));
			} else if ("ipblocked".equals(failure)) {
				SC.warn(I18N.message("ipblockedwarn", info.getConfig("throttle.ip.wait")));
			} else {
				loginService.getUser((String) username.getValue(), new AsyncCallback<GUIUser>() {

					@Override
					public void onFailure(Throwable caught) {
						SC.warn(I18N.message("accesdenied"));
					}

					@Override
					public void onSuccess(GUIUser user) {
						if (user != null && (user.getQuotaCount() >= user.getQuota() && user.getQuota() >= 0)) {
							SC.warn(I18N.message("quotadocsexceeded"));
						} else if ("passwordexpired".equals(failure)) {
							ChangePassword change = new ChangePassword(user);
							change.show();
						} else {
							SC.warn(I18N.message("accesdenied"));
						}
					}
				});
			}
		} else {
			SC.warn(I18N.message("accesdenied"));
		}
	}
}