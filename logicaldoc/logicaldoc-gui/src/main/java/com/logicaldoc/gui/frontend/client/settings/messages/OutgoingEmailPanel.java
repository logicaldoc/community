package com.logicaldoc.gui.frontend.client.settings.messages;

import java.util.LinkedHashMap;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIEmailSettings;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel shows the Email settings.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class OutgoingEmailPanel extends AdminPanel {

	private ValuesManager vm = new ValuesManager();

	private GUIEmailSettings emailSettings;

	private FolderSelector targetSelector;

	public OutgoingEmailPanel(GUIEmailSettings settings) {
		super("outgoingemail");
		this.emailSettings = settings;

		Tab templates = new Tab();
		templates.setTitle(I18N.message("messagetemplates"));
		templates.setPane(new MessageTemplatesPanel());

		DynamicForm emailForm = new DynamicForm();
		emailForm.setValuesManager(vm);
		emailForm.setTitleOrientation(TitleOrientation.LEFT);

		// SMTP Server
		TextItem smtpServer = ItemFactory.newTextItem("smtpServer", "smtpserver", this.emailSettings.getSmtpServer());
		smtpServer.setRequired(true);
		smtpServer.setWidth(350);
		smtpServer.setWrapTitle(false);

		// Port
		IntegerItem port = ItemFactory.newValidateIntegerItem("port", "port", this.emailSettings.getPort(), 1, null);
		port.setRequired(true);

		// Username
		TextItem username = ItemFactory.newTextItemPreventAutocomplete("username", "username", this.emailSettings.getUsername());
		username.setWidth(350);
		username.setWrapTitle(false);

		// Password
		PasswordItem password = ItemFactory.newPasswordItemPreventAutocomplete("password", "password", this.emailSettings.getPwd());
		password.setWrapTitle(false);

		// Connection Security
		SelectItem connSecurity = new SelectItem();
		LinkedHashMap<String, String> opts = new LinkedHashMap<String, String>();
		opts.put(GUIEmailSettings.SECURITY_NONE, I18N.message("none"));
		opts.put(GUIEmailSettings.SECURITY_STARTTLS, I18N.message("starttls"));
		opts.put(GUIEmailSettings.SECURITY_TLS, I18N.message("tls"));
		opts.put(GUIEmailSettings.SECURITY_SSL, I18N.message("ssl"));
		connSecurity.setValueMap(opts);
		connSecurity.setName("connSecurity");
		connSecurity.setTitle(I18N.message("connsecurity"));
		connSecurity.setValue(this.emailSettings.getConnSecurity());
		connSecurity.setWrapTitle(false);

		// Use Secure Authentication
		CheckboxItem secureAuth = new CheckboxItem();
		secureAuth.setName("secureAuth");
		secureAuth.setTitle(I18N.message("secureauth"));
		secureAuth.setRedrawOnChange(true);
		secureAuth.setWidth(50);
		secureAuth.setValue(emailSettings.isSecureAuth());
		secureAuth.setWrapTitle(false);

		// Sender Email
		TextItem senderEmail = ItemFactory.newEmailItem("senderEmail", "senderemail", false);
		senderEmail.setValue(this.emailSettings.getSenderEmail());
		senderEmail.setWidth(350);
		senderEmail.setWrapTitle(false);

		// Use the user's email as sender
		CheckboxItem userAsSender = new CheckboxItem();
		userAsSender.setName("userasfrom");
		userAsSender.setTitle(I18N.message("userasfrom"));
		userAsSender.setRedrawOnChange(true);
		userAsSender.setWidth(350);
		userAsSender.setValue(emailSettings.isUserAsFrom());
		userAsSender.setWrapTitle(false);

		// Target folder where outgoing messages are saved
		targetSelector = new FolderSelector("target", true);
		targetSelector.setTitle(I18N.message("targetfolder"));
		targetSelector.setHint(I18N.message("smtptargetfolderhint"));
		targetSelector.setWidth(250);
		targetSelector.setRequired(false);
		if (emailSettings.getTargetFolder() != null)
			targetSelector.setFolder(emailSettings.getTargetFolder());

		SelectItem foldering = ItemFactory.newEmailFolderingSelector("foldering", "foldering");
		foldering.setRequired(false);
		foldering.setValue("" + emailSettings.getFoldering());

		ButtonItem save = new ButtonItem("save", I18N.message("save"));
		save.setStartRow(true);
		save.setEndRow(false);
		save.addClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {
			@Override
			public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
				@SuppressWarnings("unchecked")
				Map<String, Object> values = (Map<String, Object>) vm.getValues();

				if (vm.validate()) {
					OutgoingEmailPanel.this.emailSettings.setSmtpServer((String) values.get("smtpServer"));
					if (values.get("port") instanceof Integer)
						OutgoingEmailPanel.this.emailSettings.setPort((Integer) values.get("port"));
					else
						OutgoingEmailPanel.this.emailSettings.setPort(Integer.parseInt(values.get("port").toString()));

					OutgoingEmailPanel.this.emailSettings.setUsername((String) values.get("username"));
					OutgoingEmailPanel.this.emailSettings.setPwd((String) values.get("password"));
					OutgoingEmailPanel.this.emailSettings.setConnSecurity((String) values.get("connSecurity"));
					OutgoingEmailPanel.this.emailSettings
							.setSecureAuth(values.get("secureAuth").toString().equals("true") ? true : false);
					OutgoingEmailPanel.this.emailSettings.setSenderEmail((String) values.get("senderEmail"));
					OutgoingEmailPanel.this.emailSettings
							.setUserAsFrom(values.get("userasfrom").toString().equals("true") ? true : false);
					OutgoingEmailPanel.this.emailSettings
							.setFoldering(Integer.parseInt(values.get("foldering").toString()));
					OutgoingEmailPanel.this.emailSettings.setTargetFolder(targetSelector.getFolder());

					SettingService.Instance.get().saveEmailSettings(OutgoingEmailPanel.this.emailSettings,
							new AsyncCallback<Void>() {

								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(Void ret) {
									Session.get().getInfo().setConfig(
											Session.get().getTenantName() + ".smtp.userasfrom",
											"" + OutgoingEmailPanel.this.emailSettings.isUserAsFrom());
									GuiLog.info(I18N.message("settingssaved"), null);
								}
							});
				}
			}
		});

		ButtonItem test = new ButtonItem("test", I18N.message("testconnection"));
		test.addClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {
			@Override
			public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
				if (vm.validate()) {
					LD.askForValue(I18N.message("email"), I18N.message("email"),
							(String) vm.getValueAsString("senderEmail"), new ValueCallback() {

								@Override
								public void execute(String value) {
									LD.contactingServer();
									SettingService.Instance.get().testEmail(value, new AsyncCallback<Boolean>() {
										@Override
										public void onFailure(Throwable caught) {
											GuiLog.serverError(caught);
											LD.clearPrompt();
										}

										@Override
										public void onSuccess(Boolean result) {
											LD.clearPrompt();
											if (result.booleanValue())
												SC.say(I18N.message("connectionestablished"));
											else
												SC.warn(I18N.message("connectionfailed"));
										}
									});
								}
							});
				}
			}
		});

		/*
		 * Two invisible fields to 'mask' the real credentials to the browser
		 * and prevent it to auto-fill the username and password we really use.
		 */
		TextItem fakeUsername = ItemFactory.newTextItem("prevent_autofill", "prevent_autofill",
				this.emailSettings.getUsername());
		fakeUsername.setCellStyle("nodisplay");
		fakeUsername.setTitleStyle("nodisplay");
		PasswordItem fakePassword = ItemFactory.newPasswordItem("password_fake", "password_fake",
				this.emailSettings.getPwd());
		fakePassword.setCellStyle("nodisplay");
		fakePassword.setTitleStyle("nodisplay");

		emailForm.setItems(smtpServer, port, fakeUsername, fakePassword, username, password, connSecurity, secureAuth,
				senderEmail, userAsSender, targetSelector, foldering, save, test);
		body.setMembers(emailForm);

		tabs.addTab(templates);
	}
}