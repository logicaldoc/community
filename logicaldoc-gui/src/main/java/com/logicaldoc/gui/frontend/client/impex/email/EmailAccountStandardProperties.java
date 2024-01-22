package com.logicaldoc.gui.frontend.client.impex.email;

import java.util.Map;

import com.logicaldoc.gui.common.client.beans.GUIEmailAccount;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
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
	private static final String NODISPLAY = "nodisplay";

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

		if (account.getTarget() != null)
			targetSelector.setFolder(account.getTarget());
		targetSelector.addFolderChangeListener((GUIFolder folder) -> changedHandler.onChanged(null));

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
		username.setWidth(180);

		SelectItem language = ItemFactory.newLanguageSelector("language", false, false);
		language.addChangedHandler(changedHandler);
		language.setRequired(true);
		language.setValue(account.getLanguage());

		TextItem server = ItemFactory.newTextItem("server", account.getHost());
		server.setRequired(true);
		server.setWidth(180);
		server.addChangedHandler(changedHandler);

		IntegerItem port = ItemFactory.newIntegerItem("port", "port", account.getPort());
		port.setWidth(80);
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

		/*
		 * Two invisible fields to 'mask' the real credentials to the browser
		 * and prevent it to auto-fill the username and password we really use.
		 */
		TextItem fakeUsername = ItemFactory.newTextItem("prevent_autofill", account.getUsername());
		fakeUsername.setCellStyle(NODISPLAY);
		TextItem fakeUsernameAgain = ItemFactory.newTextItem("prevent_autofill2", account.getUsername());
		fakeUsernameAgain.setCellStyle(NODISPLAY);
		TextItem hiddenPassword = ItemFactory.newTextItem("password_hidden", account.getPassword());
		hiddenPassword.setCellStyle(NODISPLAY);
		hiddenPassword.addChangedHandler(changedHandler);
		FormItem password = ItemFactory.newSafePasswordItem("password", I18N.message("password"), account.getPassword(),
				hiddenPassword, changedHandler);
		password.addChangedHandler(changedHandler);
		password.setWidth(180);

		form.setItems(mailaddress, server, targetSelector, fakeUsername, fakeUsernameAgain, hiddenPassword, username,
				password, foldering, language, protocol, port, ssl);

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
			account.setProvider((String) values.get("protocol"));
			if (values.get("port") instanceof Integer port) {
				account.setPort(port);
			} else {
				account.setPort(Integer.parseInt((String) values.get("port")));
			}
			account.setSsl("yes".equals(values.get("ssl")));
			account.setFoldering(Integer.parseInt((String) values.get("foldering")));

			account.setPassword((String) values.get("password_hidden"));
		}
		return !form.hasErrors();
	}
}