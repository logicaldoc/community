package com.logicaldoc.gui.login.client;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

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
import com.logicaldoc.gui.common.client.beans.GUISession;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.RequestInfo;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.common.client.widgets.MessageLabel;
import com.logicaldoc.gui.login.client.services.LoginService;
import com.logicaldoc.gui.login.client.services.TfaService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpacerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * The panel showing the login form
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class LoginPanel extends VLayout {

	private static final String PASSWORD_STR = "password";

	private static final String ENTER = "enter";

	private static final String LOGIN_FIELD = "login-field";

	private static final String A_HREF = "<a href='";

	protected static final int FORM_WIDTH = 280;

	private static final int COLUMN_WIDTH = FORM_WIDTH + 20;

	protected static final String PARAM_SUCCESSURL = "j_successurl";

	protected static final String PARAM_FAILUREURL = "j_failureurl";

	protected TextItem username = new TextItem();

	protected PasswordItem password = ItemFactory.newPasswordItem();

	protected TextItem secretKey = new TextItem();

	protected SelectItem language;

	protected CheckboxItem rememberMe = new CheckboxItem();

	protected VLayout mainPanel = null;

	protected DynamicForm credentialsForm = new DynamicForm();

	protected DynamicForm secretKeyForm = new DynamicForm();

	protected GUIInfo info = null;

	protected ButtonItem signIn = new ButtonItem(I18N.message("signin"));

	// Panel containing the inputs
	protected VLayout inputsPanel = new VLayout();

	// Panel containing just the credentials
	protected VLayout credentialsPanel = new VLayout();

	// Panel containing just the secret key
	protected VLayout secretKeyPanel = new VLayout();

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
		Img logoTop = ItemFactory
				.newBrandImg(info.getBranding().isLogoOemCustomized() ? "logo_head.png" : "logo_oem.png", info);
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
		Img logoLogin = ItemFactory.newBrandImg(!info.getBranding().isLogoOemCustomized() ? "logo.png" : "logo_oem.png",
				info);
		logoLogin.setWidth(205);
		logoLogin.setHeight(40);
		logoLogin.setAlign(Alignment.CENTER);
		logoLogin.setLayoutAlign(Alignment.CENTER);

		/*
		 * Panel containing product name and version
		 */
		String productInfoHtml = "<b>" + info.getBranding().getProductName() + " " + info.getRelease() + "</b>";
		if (info.getBranding().getUrl() != null && !"-".equals(info.getBranding().getUrl()))
			productInfoHtml = A_HREF + info.getBranding().getUrl() + "' target='_blank' class='login-link'>"
					+ productInfoHtml + "</a>";
		HTMLFlow productInfo = new HTMLFlow(productInfoHtml);
		productInfo.setHeight(16);
		productInfo.setWidth(COLUMN_WIDTH);
		productInfo.setStyleName("login-product");

		// Prepare the Forms
		credentialsForm = new DynamicForm();
		credentialsForm.setAlign(Alignment.CENTER);
		credentialsForm.setWidth(FORM_WIDTH);
		credentialsForm.setNumCols(3);
		credentialsForm.setTitleWidth(0);
		credentialsForm.setMargin(0);
		credentialsForm.setCellPadding(0);

		prepareUsername();

		preparePassword();

		prepareLanguage();

		SpacerItem spacerItem12 = new SpacerItem();
		spacerItem12.setHeight(12);
		spacerItem12.setColSpan(3);

		secretKeyForm = new DynamicForm();
		secretKeyForm.setTitleOrientation(TitleOrientation.TOP);
		secretKeyForm.setAlign(Alignment.CENTER);
		secretKeyForm.setWidth(FORM_WIDTH);
		secretKeyForm.setNumCols(3);
		secretKeyForm.setTitleWidth(0);
		secretKeyForm.setMargin(0);
		secretKeyForm.setCellPadding(0);

		prepareSecretKey();

		ButtonItem back = new ButtonItem(I18N.message("back"));
		back.setHeight(34);
		back.setAlign(Alignment.LEFT);
		back.setColSpan(2);
		back.setEndRow(false);
		back.addClickHandler(event -> toggleInputForm());

		signIn.setBaseStyle("btn");
		signIn.setHoverStyle("btn");
		signIn.setHeight(34);
		signIn.setAlign(Alignment.RIGHT);
		signIn.setStartRow(false);
		signIn.setWidth(120);
		signIn.addClickHandler(event -> onSignin());

		rememberMe.setTitle(I18N.message("rememberme"));
		rememberMe.setRequired(false);
		rememberMe.setShowTitle(false);
		rememberMe.setValue(CookiesManager.isSaveLogin());
		rememberMe.setTextBoxStyle(LOGIN_FIELD);
		rememberMe.setAlign(Alignment.LEFT);
		rememberMe.setEndRow(false);
		rememberMe.setColSpan(2);

		SpacerItem rememberMePlaceholder = new SpacerItem();
		rememberMePlaceholder.setEndRow(false);
		rememberMePlaceholder.setColSpan(2);
		rememberMePlaceholder.setWidth(rememberMe.getWidth());

		List<FormItem> formItems = new ArrayList<>();
		formItems.add(username);
		formItems.add(spacerItem12);
		formItems.add(password);
		formItems.add(spacerItem12);

		if ("true".equals(info.getConfig("gui.login.lang"))) {
			formItems.add(language);
			formItems.add(spacerItem12);
		}

		if (saveLoginEnabled)
			formItems.add(rememberMe);
		else
			formItems.add(rememberMePlaceholder);
		formItems.add(signIn);

		credentialsForm.setItems(formItems.toArray(new FormItem[0]));
		secretKeyForm.setItems(spacerItem12, spacerItem12, secretKey, spacerItem12, spacerItem12, back, signIn,
				spacerItem12);

		HTMLFlow lostPassword = new HTMLFlow(
				"<div><a href=\"javascript:showLostDialog('" + info.getBranding().getProductName()
						+ "')\" class='login-lost'>" + I18N.message("lostpassword") + "</a></div>");
		lostPassword.setLayoutAlign(Alignment.RIGHT);
		lostPassword.setHoverDelay(0);
		lostPassword.setMargin(0);
		lostPassword.addClickHandler(event -> showLostDialog(info.getBranding().getProductName()));

		/*
		 * Prepare the licensing canvas
		 */
		String copyrightHtml = "<div>\u00A9 " + info.getYear() + " " + info.getBranding().getVendor();
		if (info.getBranding().getUrl() != null && !"-".equals(info.getBranding().getUrl()))
			copyrightHtml = A_HREF + info.getBranding().getUrl() + "' target='_blank' class='login-copyright-link'>"
					+ copyrightHtml + "</a></div>";
		String licenseeHtml = "";
		if (info.getLicensee() != null && !"".equals(info.getLicensee().trim()))
			licenseeHtml += "<div>" + I18N.message("licensedto") + " <b>" + info.getLicensee() + "</b></div>";

		HTMLFlow licensing = new HTMLFlow(copyrightHtml + licenseeHtml);
		licensing.setStyleName("login-copyright");

		if ("true".equals(info.getConfig("gui.lostpassword.show")))
			credentialsPanel.setMembers(credentialsForm, lostPassword);
		else
			credentialsPanel.setMembers(credentialsForm);

		secretKeyPanel.setMembers(secretKeyForm);
		secretKeyPanel.setVisible(false);

		inputsPanel.setMargin(0);
		inputsPanel.setWidth(FORM_WIDTH);
		inputsPanel.setStyleName("control-group");
		inputsPanel.setMembers(logoLogin, spacer15, credentialsPanel, secretKeyPanel, licensing);

		// Panel containing the login form
		VLayout loginForm = new VLayout();
		loginForm.setLayoutAlign(VerticalAlignment.TOP);
		loginForm.setLayoutAlign(Alignment.CENTER);
		loginForm.setAlign(VerticalAlignment.TOP);
		loginForm.setMembersMargin(0);
		loginForm.setMargin(0);
		loginForm.setWidth(FORM_WIDTH);
		loginForm.setStyleName("login-form");
		loginForm.setMembers(inputsPanel);

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
		centerColumn.setHeight(200);
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
		if (saveLoginEnabled && Boolean.TRUE.equals(rememberMe.getValueAsBoolean())) {
			String[] credentials = CookiesManager.getSavedCredentials();
			username.setValue(credentials[0]);
			password.setValue(credentials[1]);
		}
	}

	private void prepareSecretKey() {
		secretKey.setTitle(I18N.message("secretkey"));
		secretKey.setShowTitle(true);
		secretKey.setHint(I18N.message("secretkey").toLowerCase());
		secretKey.setShowHintInField(true);
		secretKey.setWrapTitle(true);
		secretKey.setHeight(34);
		secretKey.setWidth(FORM_WIDTH);
		secretKey.setAlign(Alignment.LEFT);
		secretKey.setTextBoxStyle("secretkey-field");
		secretKey.setColSpan(3);
		secretKey.addKeyPressHandler(event -> {
			if (event.getKeyName() != null && ENTER.equalsIgnoreCase(event.getKeyName()))
				onSignin();
		});
	}

	private void prepareLanguage() {
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
		language.setColSpan(3);

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
	}

	private void preparePassword() {
		password = ItemFactory.newPasswordItem(PASSWORD_STR, PASSWORD_STR, null);
		password.setShowTitle(false);
		password.setHint(I18N.message(PASSWORD_STR).toLowerCase());
		password.setShowHintInField(true);
		password.setRequired(true);
		password.setHeight(34);
		password.setWidth(FORM_WIDTH);
		password.setTextBoxStyle(LOGIN_FIELD);
		password.setAlign(Alignment.LEFT);
		password.setWrapTitle(false);
		password.setColSpan(3);
		password.addKeyPressHandler(event -> {
			if (event.getKeyName() != null && ENTER.equalsIgnoreCase(event.getKeyName()))
				onSignin();
		});
	}

	private void prepareUsername() {
		username.setTitle(I18N.message("username"));
		username.setShowTitle(false);
		username.setHint(I18N.message("username").toLowerCase());
		username.setShowHintInField(true);
		username.setWrapTitle(false);
		username.setRequired(true);
		username.setHeight(34);
		username.setWidth(FORM_WIDTH);
		username.setAlign(Alignment.LEFT);
		username.setTextBoxStyle(LOGIN_FIELD);
		username.setColSpan(3);
		username.addKeyPressHandler(event -> {
			if (event.getKeyName() != null && ENTER.equalsIgnoreCase(event.getKeyName()))
				onSignin();
		});
	}

	protected void initGUI() {
		boolean saveLoginEnabled = "true".equals(info.getConfig("gui.savelogin"));
		initGUI(saveLoginEnabled);
	}

	protected void prepareSwitchViewLink() {
		String url = "mobile".equals(Util.getJavascriptVariable("j_layout")) ? (Util.contextPath() + "login.jsp")
				: (Util.contextPath() + "login-mobile.jsp");
		url += "?switch=true&tenant=" + Util.detectTenant();
		String label = "mobile".equals(Util.getJavascriptVariable("j_layout")) ? I18N.message("viewclassicweb")
				: I18N.message("viewmobileweb");

		/*
		 * A link to the alternative login page
		 */
		HTMLFlow switchLink = new HTMLFlow(A_HREF + url + "' class='login-switchview'>" + label + "</a>");
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
		List<MessageLabel> messages = new ArrayList<>();
		for (GUIMessage alert : info.getAlerts()) {
			MessageLabel label = new MessageLabel(alert, info.getTenant().getId() == 1L);
			label.setStyleName("loginMemesage");
			if (alert.isShowInLogin()) {
				messages.add(label);
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

	protected void onSignin() {
		// Reset any reference to past sessions
		CookiesManager.removeSid();

		lockInput();

		if (!credentialsForm.validate()) {
			onAuthenticationFailure();
			return;
		}

		if (Feature.enabled(Feature.TWO_FACTORS_AUTHENTICATION) && "true".equals(info.getConfig("2fa.enabled"))
				&& credentialsPanel.isVisible()) {
			String login = username.getValueAsString();
			LoginService.Instance.get().isSecretKeyRequired(login, CookiesManager.getSavedDevice(),
					new AsyncCallback<Boolean>() {

						@Override
						public void onFailure(Throwable caught) {
							SC.warn(caught.getMessage());
							unlockInput();
						}

						@Override
						public void onSuccess(Boolean required) {
							if (Boolean.TRUE.equals(required)) {
								LoginService.Instance.get().getUser(login, new AsyncCallback<>() {

									@Override
									public void onFailure(Throwable caught) {
										unlockInput();
										SC.warn(caught.getMessage());
									}

									@Override
									public void onSuccess(GUIUser user) {
										TfaService.Instance.get().generateKey(user.getUsername(),
												new AsyncCallback<>() {

													@Override
													public void onFailure(Throwable caught) {
														unlockInput();
														SC.warn(caught.getMessage());
													}

													@Override
													public void onSuccess(String transactionId) {
														String sfa = user.getSecondFactor().toLowerCase();
														secretKey.setTitle(
																I18N.message(sfa + ".inputsecretkey", user.getEmail()));
														secretKey.setHint(I18N.message(sfa + ".inputsecretkey.hint"));
														if (transactionId != null) {
															// Probably a push
															// message has been
															// sent to the
															// user's device
															secretKey.setValue(transactionId);

															LD.prompt(AwesomeFactory.getSpinnerIconHtml("pulse",
																	I18N.message(sfa + ".transaction.hint")));

															sendAuhtenticationRequest();
														} else {
															unlockInput();
															toggleInputForm();
														}
													}
												});
									}
								});
							} else {
								sendAuhtenticationRequest();
							}
						}
					});

		} else {
			sendAuhtenticationRequest();
		}
	}

	protected void lockInput() {
		secretKey.setDisabled(true);
		username.setDisabled(true);
		password.setDisabled(true);
		signIn.setDisabled(true);
	}

	protected void unlockInput() {
		secretKey.setDisabled(false);
		username.setDisabled(false);
		password.setDisabled(false);
		signIn.setDisabled(false);
	}

	/**
	 * Switches between the credentials form and the secret key form and
	 * vice-versa.
	 */
	protected void toggleInputForm() {
		if (credentialsPanel.isVisible()) {
			credentialsPanel.setVisible(false);
			secretKeyPanel.setVisible(true);
			secretKey.setValue((String) null);
		} else {
			credentialsPanel.setVisible(true);
			secretKeyPanel.setVisible(false);
		}
	}

	protected void sendAuhtenticationRequest() {
		CookiesManager.removeSid();

		RequestBuilder builder = new RequestBuilder(RequestBuilder.POST,
				Util.contextPath() + "j_spring_security_check");
		builder.setHeader("Content-type", "application/x-www-form-urlencoded");
		try {
			String data = prepareAuthenticationData();

			lockInput();
			builder.sendRequest(data, new RequestCallback() {
				public void onError(Request request, Throwable exception) {
					onAuthenticationFailure();
				}

				public void onResponseReceived(Request request, Response response) {
					if (response != null && response.getStatusCode() < 400) {
						SecurityService.Instance.get().getSession(language != null ? language.getValueAsString() : "en",
								response.getHeader("SID"), new AsyncCallback<>() {

									@Override
									public void onFailure(Throwable caught) {
										onAuthenticationFailure();
									}

									@Override
									public void onSuccess(GUISession sess) {
										if (sess != null)
											onAuthenticationSuccess(sess);
										else {
											onAuthenticationFailure();
										}
									}
								});
					} else {
						// status code 405 means that the request has been
						// received but the method used is not allowed
						onAuthenticationFailure();
					}
				}
			});
		} catch (RequestException e) {
			unlockInput();
			SC.warn("Login request error: {}", e.getMessage());
		}
	}

	private String prepareAuthenticationData() {
		String data = "j_username=" + URL.encodeQueryString((String) username.getValue());
		data += "&j_password=" + URL.encodeQueryString((String) password.getValue());
		if (secretKey != null && secretKey.getValue() != null)
			data += "&j_secretkey=" + URL.encodeQueryString((String) secretKey.getValue());
		if (CookiesManager.getSavedDevice() != null)
			data += "&device=" + URL.encodeQueryString(CookiesManager.getSavedDevice());
		return data;
	}

	public static void showLostDialog(String productName) {
		ResetPassword pwdReset = new ResetPassword(productName);
		pwdReset.show();
	}

	protected void onAuthenticationSuccess(GUISession session) {
		SC.clearPrompt();
		boolean saveLoginEnabled = "true".equals(info.getConfig("gui.savelogin"));
		CookiesManager.saveLogin(saveLoginEnabled, rememberMe.getValueAsBoolean(), username.getValueAsString(),
				password.getValueAsString());

		if (!"true".contentEquals(session.getInfo().getConfig("2fa.enabled"))
				|| !"true".contentEquals(session.getInfo().getConfig("2fa.allowtrusted"))) {
			Util.redirectToSuccessUrl(language.getValueAsString());
		} else {
			SecurityService.Instance.get().isTrustedDevice(CookiesManager.getSavedDevice(),
					new AsyncCallback<Boolean>() {

						@Override
						public void onFailure(Throwable caught) {
							Util.redirectToSuccessUrl(language.getValueAsString());
						}

						@Override
						public void onSuccess(Boolean trusted) {
							if (trusted.booleanValue()) {
								Util.redirectToSuccessUrl(language.getValueAsString());
							} else {
								SC.ask(I18N.message("trustdevice"), I18N.message("trustdevicequestion"), choice -> {
									if (Boolean.FALSE.equals(choice)) {
										Util.redirectToSuccessUrl(language.getValueAsString());
									} else {
										LD.askForString(I18N.message("trustdevice"), I18N.message("optlabeldevice"),
												null, value -> SecurityService.Instance.get().trustDevice(value,
														new AsyncCallback<>() {

															@Override
															public void onFailure(Throwable caught) {
																Util.redirectToSuccessUrl(language.getValueAsString());
															}

															@Override
															public void onSuccess(String deviceId) {
																CookiesManager.saveDevice(deviceId);
																Util.redirectToSuccessUrl(language.getValueAsString());
															}
														}));

									}
								});
							}

						}
					});
		}
	}

	protected void onAuthenticationFailure() {
		SC.clearPrompt();
		lockInput();
		LoginService.Instance.get().getUser((String) username.getValue(), new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				unlockInput();
				SC.warn(I18N.message("accesdenied"));
			}

			@Override
			public void onSuccess(GUIUser user) {
				unlockInput();

				String failure = user != null ? user.getLastLoginFailureReason() : null;
				if (user != null && user.isPasswordExpired()) {
					new ChangePassword(user, LoginPanel.this).show();
				} else if (user != null && !user.isEnabled()) {
					SC.warn(I18N.message("userdisabledwarn"));
				} else if (user != null && (user.getQuotaCount() >= user.getQuota() && user.getQuota() >= 0)) {
					SC.warn(I18N.message("quotadocsexceeded"));
				} else if ("usernameblocked".equals(failure)) {
					SC.warn(I18N.message("usernameblockedwarn", info.getConfig("throttle.username.wait")));
				} else if ("ipblocked".equals(failure)) {
					SC.warn(I18N.message("ipblockedwarn", info.getConfig("throttle.ip.wait")));
				} else
					SC.warn(I18N.message("accesdenied"));

			}
		});
	}

	/**
	 * Invoked when the user successfully changed his expired password
	 */
	public void onPasswordChanged() {
		if (!credentialsPanel.isVisible())
			toggleInputForm();
	}
}