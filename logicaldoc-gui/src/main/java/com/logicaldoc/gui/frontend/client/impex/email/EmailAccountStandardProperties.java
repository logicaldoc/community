package com.logicaldoc.gui.frontend.client.impex.email;

import com.logicaldoc.gui.common.client.beans.GUIEmailAccount;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.logicaldoc.gui.frontend.client.google.GoogleApiAuthorization;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.ToggleItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Shows account's standard properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class EmailAccountStandardProperties extends EmailAccountDetailsTab {

	private static final String SERVER = "server";

	private static final String PROTOCOL = "protocol";

	private static final String USERNAME = "username";

	private DynamicForm form = new DynamicForm();

	private HLayout formsContainer = new HLayout();

	private FolderSelector targetSelector;

	public EmailAccountStandardProperties(GUIEmailAccount account, final ChangedHandler changedHandler) {
		super(account, changedHandler);
		setWidth100();
		setHeight100();
		setMembers(formsContainer);

		targetSelector = new FolderSelector("target", null);
		targetSelector.setTitle(I18N.message("targetfolder"));
		targetSelector.setWidth(250);
		targetSelector.setRequired(true);

		if (account.getTarget() != null)
			targetSelector.setFolder(account.getTarget());
		targetSelector.addFolderChangeListener(folder -> changedHandler.onChanged(null));

		refresh();
	}

	private void refresh() {
		form.clearValues();
		form.clearErrors(false);
		form.destroy();

		if (Boolean.TRUE.equals(formsContainer.contains(form)))
			formsContainer.removeChild(form);

		form = new DynamicForm();
		form.setNumCols(3);
		form.setWidth(1);
		form.setTitleOrientation(TitleOrientation.TOP);

		TextItem mailaddress = ItemFactory.newEmailItem("mailaddress", "email", false);
		mailaddress.setValue(account.getMailAddress());
		mailaddress.addChangedHandler(changedHandler);
		mailaddress.setRequired(true);

		TextItem username = ItemFactory.newTextItemPreventAutocomplete(USERNAME, USERNAME, account.getUsername());
		username.addChangedHandler(changedHandler);
		username.setWidth(200);

		SelectItem language = ItemFactory.newLanguageSelector("language", false, false);
		language.addChangedHandler(changedHandler);
		language.setRequired(true);
		language.setValue(account.getLanguage());

		TextItem server = ItemFactory.newTextItem(SERVER, account.getHost());
		server.setRequired(true);
		server.setWidth(200);
		server.addChangedHandler(changedHandler);

		IntegerItem port = ItemFactory.newIntegerItem("port", "port", account.getPort());
		port.setWidth(80);
		port.setEndRow(true);
		port.addChangedHandler(changedHandler);

		ToggleItem ssl = ItemFactory.newToggleItem("ssl", account.isSsl());
		ssl.addChangedHandler(changedHandler);

		SelectItem protocol = ItemFactory.newEmailProtocolSelector();
		protocol.addChangedHandler(changedHandler);
		protocol.addChangedHandler(changed -> {
			if (changed.getValue().toString().contains("pop3google")) {
				form.setValue(SERVER, "pop.gmail.com");
				form.setValue("port", 995);
				form.setValue("ssl", false);
			} else if (changed.getValue().toString().contains("imapgoogle")) {
				form.setValue(SERVER, "imap.gmail.com");
				form.setValue("port", 993);
				form.setValue("ssl", false);
			} else if (changed.getValue().toString().contains("imapmicrosoft365")) {
				form.setValue(SERVER, "outlook.office365.com");
				form.setValue("port", 993);
				form.setValue("ssl", true);
			} 
		});
		protocol.setRequired(true);
		protocol.setValue(account.getProvider());

		StaticTextItem authorize = ItemFactory.newStaticTextItem("authorize",
				account.getId() == 0L ? I18N.message("saveandclickhereauthgoogle")
						: I18N.message("clickhereauthgoogle"));
		authorize.setVisibleWhen(new AdvancedCriteria(PROTOCOL, OperatorId.CONTAINS, "google"));
		authorize.setEndRow(true);
		if (account.getId() != 0L)
			authorize.addClickHandler(click -> new GoogleApiAuthorization("email-" + account.getId()).show());

		SelectItem foldering = ItemFactory.newEmailFolderingSelector();
		foldering.addChangedHandler(changedHandler);
		foldering.setRequired(true);
		foldering.setValue("" + account.getFoldering());

		TextItem clientId = ItemFactory.newTextItem("clientid", account.getClientId());
		clientId.setWidth(200);
		clientId.addChangedHandler(changedHandler);
		clientId.setVisibleWhen(new AdvancedCriteria(PROTOCOL, OperatorId.CONTAINS, "365"));

		TextItem clientTenant = ItemFactory.newTextItem("clienttenant", I18N.message("tenantId"),
				account.getClientTenant());
		clientTenant.setWidth(200);
		clientTenant.addChangedHandler(changedHandler);
		clientTenant.setVisibleWhen(new AdvancedCriteria(PROTOCOL, OperatorId.CONTAINS, "365"));

		/*
		 * Two invisible fields to 'mask' the real credentials to the browser
		 * and prevent it to auto-fill the username, password and clientSecret
		 * we really use.
		 */
		TextItem fakeUsername = ItemFactory.newTextItem("prevent_autofill", account.getUsername());
		fakeUsername.setHidden(true);
		TextItem fakeUsernameAgain = ItemFactory.newTextItem("prevent_autofill2", account.getUsername());
		fakeUsernameAgain.setHidden(true);
		TextItem hiddenPassword = ItemFactory.newTextItem("password_hidden", account.getPassword());
		hiddenPassword.setHidden(true);
		hiddenPassword.addChangedHandler(changedHandler);
		TextItem hiddenClientSecret = ItemFactory.newTextItem("clientsecret_hidden", account.getClientSecret());
		hiddenClientSecret.setHidden(true);
		hiddenClientSecret.addChangedHandler(changedHandler);

		FormItem password = ItemFactory.newSafePasswordItem("password", I18N.message("password"), account.getPassword(),
				hiddenPassword, changedHandler);
		password.addChangedHandler(changedHandler);
		password.setWidth(200);
		password.setEndRow(true);

		FormItem clientSecret = ItemFactory.newSafePasswordItem("clientsecret", I18N.message("clientsecret"),
				account.getClientSecret(), hiddenClientSecret, changedHandler);
		clientSecret.setWidth(200);
		clientSecret.addChangedHandler(changedHandler);
		clientSecret.setVisibleWhen(new AdvancedCriteria(PROTOCOL, OperatorId.CONTAINS, "365"));

		form.setItems(mailaddress, protocol, ssl, server, port, username, password, authorize, clientId, clientTenant,
				clientSecret, foldering, targetSelector, language, fakeUsername, fakeUsernameAgain, hiddenPassword,
				hiddenClientSecret);

		formsContainer.addMember(form);
	}

	boolean validate() {
		if (form.validate()) {
			account.setMailAddress(form.getValueAsString("mailaddress"));
			account.setHost(form.getValueAsString(SERVER));
			account.setUsername(form.getValueAsString(USERNAME));
			account.setTarget(targetSelector.getFolder());
			account.setLanguage(form.getValueAsString("language"));
			account.setProvider(form.getValueAsString(PROTOCOL));
			account.setPort(Integer.parseInt(form.getValueAsString("port")));
			account.setSsl(Boolean.valueOf(form.getValueAsString("ssl")));
			account.setFoldering(Integer.parseInt(form.getValueAsString("foldering")));
			account.setPassword(form.getValueAsString("password_hidden"));
			account.setClientId(form.getValueAsString("clientid"));
			account.setClientSecret(form.getValueAsString("clientsecret_hidden"));
			account.setClientTenant(form.getValueAsString("clienttenant"));
		}
		return !form.hasErrors();
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