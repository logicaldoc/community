package com.logicaldoc.gui.frontend.client.settings.messages;

import java.util.LinkedHashMap;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIEmailSettings;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.google.GoogleApiAuthorization;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel shows the Email settings.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class OutgoingEmailPanel extends AdminPanel {

	private static final String PROTOCOL = "protocol";

	private static final String USERASFROM = "userasfrom";

	private static final String SENDEREMAIL = "senderemail";

	private static final String PASSWORD = "password";

	private static final String USERNAME = "username";

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

		// Server
		TextItem server = ItemFactory.newTextItem("server", this.emailSettings.getServer());
		server.setRequired(true);
		server.setWidth(350);
		server.setWrapTitle(false);

		// Port
		IntegerItem port = ItemFactory.newValidateIntegerItem("port", "port", this.emailSettings.getPort(), 1, null);
		port.setRequired(true);

		// Username
		TextItem username = ItemFactory.newTextItemPreventAutocomplete(USERNAME, USERNAME,
				this.emailSettings.getUsername());
		username.setWidth(350);
		username.setWrapTitle(false);

		SelectItem protocol = ItemFactory.newSmtpProtocolSelector();
		protocol.setRequired(true);
		protocol.setValue(this.emailSettings.getProtocol());
		protocol.addChangedHandler(changed -> {
			if (changed.getValue().toString().contains("google")) {
				vm.setValue("server", "smtp.gmail.com");
				vm.setValue("port", 587);
				vm.setValue("connSecurity", GUIEmailSettings.SECURITY_STARTTLS);
				vm.setValue("secureAuth", false);
			} else if (changed.getValue().toString().contains("microsoft")) {
				vm.setValue("server", "smtp.office365.com");
				vm.setValue("port", 587);
				vm.setValue("connSecurity", GUIEmailSettings.SECURITY_STARTTLS);
				vm.setValue("secureAuth", true);
			}
		});

		// Connection Security
		SelectItem connSecurity = new SelectItem();
		LinkedHashMap<String, String> opts = new LinkedHashMap<>();
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
		TextItem senderEmail = ItemFactory.newEmailItem(SENDEREMAIL, SENDEREMAIL, false);
		senderEmail.setValue(this.emailSettings.getSenderEmail());
		senderEmail.setWidth(350);
		senderEmail.setWrapTitle(false);

		// Use the user's email as sender
		CheckboxItem userAsSender = new CheckboxItem();
		userAsSender.setName(USERASFROM);
		userAsSender.setTitle(I18N.message(USERASFROM));
		userAsSender.setRedrawOnChange(true);
		userAsSender.setWidth(350);
		userAsSender.setValue(emailSettings.isUserAsFrom());
		userAsSender.setWrapTitle(false);

		// Target folder where outgoing messages are saved
		targetSelector = new FolderSelector("target", null);
		targetSelector.setTitle(I18N.message("targetfolder"));
		targetSelector.setHint(I18N.message("smtptargetfolderhint"));
		targetSelector.setWidth(250);
		targetSelector.setRequired(false);
		if (emailSettings.getTargetFolder() != null)
			targetSelector.setFolder(emailSettings.getTargetFolder());

		SelectItem foldering = ItemFactory.newEmailFolderingSelector();
		foldering.setRequired(false);
		foldering.setValue("" + emailSettings.getFoldering());

		ButtonItem save = prepareSaveButton();

		ButtonItem test = prepareTestButton();

		TextItem clientId = ItemFactory.newTextItem("clientid", emailSettings.getClientId());
		clientId.setWidth(350);
		clientId.setVisibleWhen(new AdvancedCriteria(PROTOCOL, OperatorId.CONTAINS, "365"));

		TextItem clientTenant = ItemFactory.newTextItem("clienttenant", I18N.message("tenantId"),
				emailSettings.getClientTenant());
		clientTenant.setWidth(350);
		clientTenant.setVisibleWhen(new AdvancedCriteria(PROTOCOL, OperatorId.CONTAINS, "365"));

		StaticTextItem authorize = ItemFactory.newStaticTextItem("authorize", I18N.message("clickhereauthgoogle"));
		authorize.setVisibleWhen(new AdvancedCriteria(PROTOCOL, OperatorId.CONTAINS, "google"));
		authorize.addClickHandler(click -> new GoogleApiAuthorization(".smtp").show());

		/*
		 * Two invisible fields to 'mask' the real credentials to the browser
		 * and prevent it to auto-fill the username, password and client secret
		 * we really use.
		 */
		TextItem fakeUsername = ItemFactory.newTextItem("prevent_autofill", this.emailSettings.getUsername());
		fakeUsername.setHidden(true);
		TextItem hiddenPassword = ItemFactory.newTextItem("password_hidden", this.emailSettings.getPwd());
		hiddenPassword.setHidden(true);
		TextItem hiddenClientSecret = ItemFactory.newTextItem("clientsecret_hidden",
				this.emailSettings.getClientSecret());
		hiddenClientSecret.setHidden(true);

		FormItem password = ItemFactory.newSafePasswordItem(PASSWORD, I18N.message(PASSWORD),
				this.emailSettings.getPwd(), hiddenPassword, null);

		FormItem clientSecret = ItemFactory.newSafePasswordItem("clientsecret", I18N.message("clientsecret"),
				this.emailSettings.getClientSecret(), hiddenClientSecret, null);
		clientSecret.setVisibleWhen(new AdvancedCriteria(PROTOCOL, OperatorId.CONTAINS, "365"));

		emailForm.setItems(protocol, server, port, connSecurity, secureAuth, username, password, clientId, clientTenant,
				clientSecret, authorize, senderEmail, userAsSender, targetSelector, foldering, save, test, fakeUsername,
				hiddenPassword, hiddenClientSecret);
		body.setMembers(emailForm);

		tabs.addTab(templates);
	}

	private ButtonItem prepareTestButton() {
		ButtonItem test = new ButtonItem("test", I18N.message("testconnection"));
		test.addClickHandler(click -> {
			if (Boolean.FALSE.equals(vm.validate()))
				return;

			LD.askForValue(I18N.message("email"), I18N.message("email"), vm.getValueAsString(SENDEREMAIL),
					(String value) -> {
						LD.contactingServer();
						SettingService.Instance.get().testEmail(value, new DefaultAsyncCallback<>() {
							@Override
							public void onSuccess(Boolean yes) {
								LD.clearPrompt();
								if (yes.booleanValue())
									SC.say(I18N.message("connectionestablished"));
								else
									SC.warn(I18N.message("connectionfailed"));
							}
						});
					});
		});
		return test;
	}

	private ButtonItem prepareSaveButton() {
		ButtonItem save = new ButtonItem("save", I18N.message("save"));
		save.setStartRow(true);
		save.setEndRow(false);
		save.addClickHandler((com.smartgwt.client.widgets.form.fields.events.ClickEvent event) -> {
			if (Boolean.FALSE.equals(vm.validate()))
				return;

			OutgoingEmailPanel.this.emailSettings.setProtocol(vm.getValueAsString(PROTOCOL));
			OutgoingEmailPanel.this.emailSettings.setServer(vm.getValueAsString("server"));
			if (vm.getValue("port") instanceof Integer intVal)
				OutgoingEmailPanel.this.emailSettings.setPort(intVal);
			else
				OutgoingEmailPanel.this.emailSettings.setPort(Integer.parseInt(vm.getValueAsString("port")));

			OutgoingEmailPanel.this.emailSettings.setUsername(vm.getValueAsString(USERNAME));
			OutgoingEmailPanel.this.emailSettings.setPwd(vm.getValueAsString("password_hidden"));
			OutgoingEmailPanel.this.emailSettings.setConnSecurity(vm.getValueAsString("connSecurity"));
			OutgoingEmailPanel.this.emailSettings.setSecureAuth(Boolean.valueOf(vm.getValueAsString("secureAuth")));
			OutgoingEmailPanel.this.emailSettings.setSenderEmail(vm.getValueAsString(SENDEREMAIL));
			OutgoingEmailPanel.this.emailSettings.setUserAsFrom(Boolean.valueOf(vm.getValueAsString(USERASFROM)));
			OutgoingEmailPanel.this.emailSettings.setFoldering(Integer.parseInt(vm.getValueAsString("foldering")));
			OutgoingEmailPanel.this.emailSettings.setTargetFolder(targetSelector.getFolder());
			OutgoingEmailPanel.this.emailSettings.setClientId(vm.getValueAsString("clientid"));
			OutgoingEmailPanel.this.emailSettings.setClientSecret(vm.getValueAsString("clientsecret_hidden"));
			OutgoingEmailPanel.this.emailSettings.setClientTenant(vm.getValueAsString("clienttenant"));

			SettingService.Instance.get().saveEmailSettings(OutgoingEmailPanel.this.emailSettings,
					new DefaultAsyncCallback<>() {
						@Override
						public void onSuccess(Void ret) {
							Session.get().getInfo().setConfig(Session.get().getTenantName() + ".smtp.userasfrom",
									"" + OutgoingEmailPanel.this.emailSettings.isUserAsFrom());
							GuiLog.info(I18N.message("settingssaved"), null);
						}
					});
		});
		return save;
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