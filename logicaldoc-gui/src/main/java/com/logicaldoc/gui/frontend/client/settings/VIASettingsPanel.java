package com.logicaldoc.gui.frontend.client.settings;

import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIEmailAccount;
import com.logicaldoc.gui.common.client.beans.GUIVIASettings;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.EmailAccountService;
import com.logicaldoc.gui.frontend.client.services.VIAService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Handles the VIA settings.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.8
 */
public class VIASettingsPanel extends AdminPanel {
	private DynamicForm settingsForm = new DynamicForm();

	private DynamicForm emailForm = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

	private VLayout layout = null;

	private GUIVIASettings settings = null;

	public VIASettingsPanel() {
		super("virtualagent");
	}

	@Override
	protected void onDraw() {
		initGUI();
	}

	void initGUI() {
		setWidth100();
		setHeight100();
		setMembersMargin(20);

		VIAService.Instance.get().get(new AsyncCallback<GUIVIASettings>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIVIASettings settings) {
				VIASettingsPanel.this.settings = settings;
				if (settings == null)
					VIASettingsPanel.this.settings = new GUIVIASettings();
				refresh();
			}
		});

	}

	public void refresh() {
		vm.clearValues();
		vm.clearErrors(false);

		if (settingsForm != null)
			settingsForm.destroy();

		if (layout != null) {
			layout.destroy();
		}

		layout = new VLayout();
		layout.setWidth100();

		settingsForm = new DynamicForm();
		settingsForm.setValuesManager(vm);
		settingsForm.setWrapItemTitles(false);
		settingsForm.setTitleOrientation(TitleOrientation.TOP);
		settingsForm.setNumCols(1);
		settingsForm.setWidth(1);

		RadioGroupItem enabled = ItemFactory.newBooleanSelector("eenabled", "enabled");
		enabled.setRequired(true);
		enabled.setValue(settings.isEnabled() ? "yes" : "no");

		SpinnerItem maxAttachments = ItemFactory.newSpinnerItem("maxattachments", "maxattachments",
				settings.getMaxAttachments());
		maxAttachments.setMin(0);
		maxAttachments.setStep(1);
		maxAttachments.setWrapTitle(false);
		maxAttachments.setRequired(true);

		IntegerItem maxAttachmentSize = ItemFactory.newLongItem("maxattachmentsize", "maxattachmentsize",
				settings.getMaxAttachmentSize() / 1024L);
		maxAttachmentSize.setRequired(true);
		maxAttachmentSize.setWrapTitle(false);
		maxAttachmentSize.setHint(I18N.message("KB"));

		settingsForm.setItems(enabled, maxAttachments, maxAttachmentSize);

		emailForm = new DynamicForm();
		emailForm.setValuesManager(vm);
		emailForm.setWrapItemTitles(false);
		emailForm.setTitleOrientation(TitleOrientation.TOP);
		emailForm.setNumCols(2);
		emailForm.setWidth(1);
		emailForm.setIsGroup(true);
		emailForm.setGroupTitle(I18N.message("monitoredemailaccount"));

		GUIEmailAccount account = settings.getEmailAccount();
		if (account == null) {
			account = new GUIEmailAccount();
			account.setEnabled(0);
			settings.setEmailAccount(account);
		}

		TextItem mailaddress = ItemFactory.newEmailItem("mailaddress", "email", false);
		mailaddress.setValue(account.getMailAddress());
		mailaddress.setRequired(false);

		TextItem username = ItemFactory.newTextItemPreventAutocomplete("username", "username", account.getUsername());
		username.setWidth(180);

		TextItem password = ItemFactory.newPasswordItemPreventAutocomplete("password", "password",
				account.getPassword());
		password.setWidth(180);

		TextItem server = ItemFactory.newTextItem("server", "server", account.getHost());
		server.setWidth(180);

		IntegerItem port = ItemFactory.newIntegerItem("port", "port", account.getPort());
		port.setWidth(80);

		RadioGroupItem ssl = ItemFactory.newBooleanSelector("ssl", "ssl");
		ssl.setValue(account.isSsl() ? "yes" : "no");

		SelectItem protocol = ItemFactory.newEmailProtocolSelector("protocol", "protocol");
		protocol.setValue(account.getProvider());

		TextItem folder = ItemFactory.newTextItem("mailfolder", "mailfolder", account.getMailFolder());

		ButtonItem resetCache = prepareResetCacheButton();

		ButtonItem testEmail = prepareTestEmailButton();

		/*
		 * Two invisible fields to 'mask' the real credentials to the browser
		 * and prevent it to auto-fill the username and password we really use.
		 */
		TextItem fakeUsername = ItemFactory.newTextItem("prevent_autofill", "prevent_autofill", account.getUsername());
		fakeUsername.setCellStyle("nodisplay");
		PasswordItem fakePassword = ItemFactory.newPasswordItem("password_fake", "password_fake",
				account.getPassword());
		fakePassword.setCellStyle("nodisplay");

		emailForm.setItems(mailaddress, server, fakeUsername, fakePassword, username, password, protocol, port, ssl,
				folder, resetCache, testEmail);

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(5);
		buttons.setMargin(3);

		IButton save = prepareSaveButton();
		buttons.setMembers(save);

		VLayout separator = new VLayout();
		separator.setHeight(20);
		separator.setWidth100();

		layout.setMembers(settingsForm, separator, emailForm, separator, buttons);

		body.addMember(layout);
	}

	private IButton prepareSaveButton() {
		IButton save = new IButton(I18N.message("save"));
		save.addClickHandler((ClickEvent event) -> {
			onSave();
		});
		return save;
	}

	private ButtonItem prepareResetCacheButton() {
		ButtonItem resetCache = new ButtonItem("resetcache", I18N.message("resetcache"));
		resetCache.addClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {
			@Override
			public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmresetcache"), (Boolean yes) -> {
					if (yes) {
						EmailAccountService.Instance.get().resetCache(settings.getEmailAccount().getId(),
								new AsyncCallback<Void>() {
									@Override
									public void onFailure(Throwable caught) {
										GuiLog.serverError(caught);
									}

									@Override
									public void onSuccess(Void result) {
										GuiLog.info(I18N.message("cachedeleted"), null);
									}
								});
					}
				});
			}
		});
		return resetCache;
	}

	private ButtonItem prepareTestEmailButton() {
		ButtonItem testEmail = new ButtonItem("testconnection", I18N.message("testconnection"));
		testEmail.addClickHandler((com.smartgwt.client.widgets.form.fields.events.ClickEvent event) -> {
			if (validate()) {
				VIAService.Instance.get().save(settings, new AsyncCallback<GUIVIASettings>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIVIASettings settings) {
						GuiLog.info(I18N.message("settingssaved"), null);
						VIASettingsPanel.this.settings = settings;
						EmailAccountService.Instance.get().test(settings.getEmailAccount().getId(),
								new AsyncCallback<Boolean>() {
									@Override
									public void onFailure(Throwable caught) {
										GuiLog.serverError(caught);
									}

									@Override
									public void onSuccess(Boolean result) {
										if (result.booleanValue())
											SC.say(I18N.message("connectionestablished"));
										else
											SC.warn(I18N.message("connectionfailed"));
									}
								});
					}
				});
			}
		});
		return testEmail;
	}

	private void onSave() {
		if (validate()) {
			VIAService.Instance.get().save(settings, new AsyncCallback<GUIVIASettings>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(GUIVIASettings settings) {
					GuiLog.info(I18N.message("settingssaved"), null);
					VIASettingsPanel.this.settings = settings;
					initGUI();
				}
			});
		}
	}

	@SuppressWarnings("unchecked")
	boolean validate() {
		vm.validate();
		if (!vm.hasErrors()) {
			Map<String, Object> values = (Map<String, Object>) vm.getValues();
			settings.setEnabled("yes".equals(values.get("eenabled").toString()));
			settings.setMaxAttachments(Integer.parseInt(values.get("maxattachments").toString()));
			settings.setMaxAttachmentSize(Long.parseLong(values.get("maxattachmentsize").toString()) * 1024L);

			GUIEmailAccount account = settings.getEmailAccount();
			if (account == null) {
				account = new GUIEmailAccount();
				settings.setEmailAccount(account);
			}

			account.setMailAddress((String) values.get("mailaddress"));
			account.setHost((String) values.get("server"));
			account.setUsername((String) values.get("username"));
			account.setPassword((String) values.get("password"));
			account.setProvider((String) values.get("protocol"));
			if (values.get("port") instanceof Integer)
				account.setPort((Integer) values.get("port"));
			else
				account.setPort(Integer.parseInt((String) values.get("port")));
			account.setSsl("yes".equals((String) values.get("ssl")));
			account.setMailFolder((String) values.get("mailfolder"));

			if (account.getMailAddress() == null || account.getMailAddress().trim().isEmpty())
				account.setEnabled(0);
			else
				account.setEnabled(1);
		}
		return !vm.hasErrors();
	}
}