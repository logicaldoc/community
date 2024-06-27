package com.logicaldoc.gui.frontend.client.impex.email;

import java.util.Map;

import com.logicaldoc.gui.common.client.beans.GUIEmailAccount;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Shows account's standard properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class EmailAccountStandardProperties extends EmailAccountDetailsTab {

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
		targetSelector.setStartRow(true);

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

		TextItem server = ItemFactory.newTextItem("server", account.getHost());
		server.setRequired(true);
		server.setWidth(200);
		server.addChangedHandler(changedHandler);

		IntegerItem port = ItemFactory.newIntegerItem("port", "port", account.getPort());
		port.setWidth(80);
		port.setEndRow(true);
		port.addChangedHandler(changedHandler);

		RadioGroupItem ssl = ItemFactory.newBooleanSelector("ssl", "ssl");
		ssl.setValue(account.isSsl() ? "yes" : "no");
		ssl.addChangedHandler(changedHandler);

		SelectItem protocol = ItemFactory.newEmailProtocolSelector();
		protocol.addChangedHandler(changedHandler);
		protocol.setRequired(true);
		protocol.setValue(account.getProvider());

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

		form.setItems(mailaddress, protocol, ssl, server, port, username, password, clientId, clientTenant,
				clientSecret, targetSelector, foldering, language, fakeUsername, fakeUsernameAgain, hiddenPassword,
				hiddenClientSecret);

		formsContainer.addMember(form);
	}

	@SuppressWarnings("unchecked")
	boolean validate() {
		Map<String, Object> values = form.getValues();
		form.validate();
		if (Boolean.FALSE.equals(form.hasErrors())) {
			account.setMailAddress((String) values.get("mailaddress"));
			account.setHost((String) values.get("server"));
			account.setUsername((String) values.get(USERNAME));
			account.setTarget(targetSelector.getFolder());
			account.setLanguage((String) values.get("language"));
			account.setProvider((String) values.get(PROTOCOL));
			if (values.get("port") instanceof Integer)
				account.setPort((Integer) values.get("port"));
			else
				account.setPort(Integer.parseInt((String) values.get("port")));
			account.setSsl("yes".equals(values.get("ssl")));
			account.setFoldering(Integer.parseInt((String) values.get("foldering")));

			account.setPassword((String) values.get("password_hidden"));
			account.setClientId((String) values.get("clientid"));
			account.setClientSecret((String) values.get("clientsecret_hidden"));
			account.setClientTenant((String) values.get("clienttenant"));
		}
		return !form.hasErrors();
	}
}