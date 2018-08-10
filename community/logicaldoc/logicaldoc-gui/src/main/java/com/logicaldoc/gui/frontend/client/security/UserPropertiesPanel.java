package com.logicaldoc.gui.frontend.client.security;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIGroup;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.data.GroupsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Shows document's standard properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class UserPropertiesPanel extends HLayout {
	private DynamicForm form1 = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

	private GUIUser user;

	private ChangedHandler changedHandler;

	private Canvas idLabel;

	private VLayout layout = new VLayout();

	private MultiComboBoxItem groupsItem;

	public UserPropertiesPanel(GUIUser user, ChangedHandler changedHandler) {
		if (user == null) {
			setMembers(UsersPanel.SELECT_USER);
		} else {
			this.user = user;
			this.changedHandler = changedHandler;
			setWidth100();
			setHeight100();
			setMembersMargin(20);

			layout.setWidth(300);

			idLabel = new Label(I18N.message("id") + ": " + Long.toString(user.getId()));
			idLabel.setHeight(15);
			layout.addMember(idLabel, 0);

			prepareGUI();
		}
	}

	public void prepareGUI() {
		boolean readonly = (changedHandler == null);
		vm.clearValues();
		vm.clearErrors(false);

		if (form1 != null)
			form1.destroy();

		if (contains(form1))
			removeChild(form1);

		form1 = new DynamicForm();
		form1.setValuesManager(vm);
		form1.setWrapItemTitles(false);
		form1.setTitleOrientation(TitleOrientation.TOP);
		form1.setNumCols(3);

		layout.addMember(form1, 1);

		CheckboxItem expires = new CheckboxItem("expires", I18N.message("passwordexpires"));
		expires.setValue(user.isPasswordExpires());
		expires.setDisabled(readonly || (Session.get().isDemo() && Session.get().getUser().getId() == 1));
		if (!readonly)
			expires.addChangedHandler(changedHandler);

		CheckboxItem enabled = new CheckboxItem("eenabled", I18N.message("enabled"));
		enabled.setValue(user.isEnabled());
		if (readonly || "admin".equals(user.getUserName())) {
			enabled.setDisabled(true);
		} else {
			enabled.addChangedHandler(changedHandler);
		}

		CheckboxItem changeAtLogin = new CheckboxItem("changeAtLogin", I18N.message("changeatfirstlogin"));
		changeAtLogin.setValue(false);
		changeAtLogin.setVisible(user.getId() == 0);

		CheckboxItem notifyCredentials = new CheckboxItem("notifyCredentials", I18N.message("notifycredentials"));
		notifyCredentials.setValue(true);
		notifyCredentials.setVisible(user.getId() == 0);

		TextItem username = ItemFactory.newTextItem("username", "username", user.getUserName());
		username.setRequired(true);
		username.setSelectOnFocus(true);
		username.setDisabled(readonly || user.getId() != 0);
		if (!readonly)
			username.addChangedHandler(changedHandler);

		TextItem firstname = ItemFactory.newTextItem("firstname", "firstname", user.getFirstName());
		firstname.setRequired(true);
		firstname.setDisabled(readonly);
		if (!readonly)
			firstname.addChangedHandler(changedHandler);

		TextItem name = ItemFactory.newTextItem("name", "lastname", user.getName());
		name.setRequired(true);
		name.setDisabled(readonly);
		if (!readonly)
			name.addChangedHandler(changedHandler);

		TextItem address = ItemFactory.newTextItem("address", "address", user.getAddress());
		address.setDisabled(readonly);
		if (!readonly)
			address.addChangedHandler(changedHandler);

		TextItem postalcode = ItemFactory.newTextItem("postalcode", "postalcode", user.getPostalCode());
		postalcode.setDisabled(readonly);
		if (!readonly)
			postalcode.addChangedHandler(changedHandler);

		TextItem city = ItemFactory.newTextItem("city", "city", user.getCity());
		city.setDisabled(readonly);
		if (!readonly)
			city.addChangedHandler(changedHandler);

		TextItem country = ItemFactory.newTextItem("country", "country", user.getCountry());
		country.setDisabled(readonly);
		if (!readonly)
			country.addChangedHandler(changedHandler);

		SelectItem language = ItemFactory.newLanguageSelector("language", false, true);
		language.setDisabled(readonly || (Session.get().isDemo() && Session.get().getUser().getId() == 1));
		language.setValue(user.getLanguage());
		if (!readonly)
			language.addChangedHandler(changedHandler);

		TextItem state = ItemFactory.newTextItem("state", "state", user.getState());
		state.setDisabled(readonly);
		if (!readonly)
			state.addChangedHandler(changedHandler);

		TextItem phone = ItemFactory.newTextItem("phone", "phone", user.getPhone());
		phone.setDisabled(readonly);
		if (!readonly)
			phone.addChangedHandler(changedHandler);

		TextItem cell = ItemFactory.newTextItem("cell", "cell", user.getCell());
		cell.setDisabled(readonly);
		if (!readonly)
			cell.addChangedHandler(changedHandler);

		TextItem email = ItemFactory.newEmailItem("email", "email", false);
		email.setRequired(true);
		email.setDisabled(readonly);
		email.setValue(user.getEmail());
		if (!readonly)
			email.addChangedHandler(changedHandler);

		TextItem email2 = ItemFactory.newEmailItem("email2", "secondaryemail", false);
		email2.setRequired(false);
		email2.setDisabled(readonly);
		email2.setValue(user.getEmail2());
		if (!readonly)
			email2.addChangedHandler(changedHandler);

		if (user.getId() == 0L)
			form1.setItems(changeAtLogin, notifyCredentials, enabled, expires, username, email, firstname, name,
					email2, language, address, postalcode, city, country, state, phone, cell);
		else
			form1.setItems(username, changeAtLogin, notifyCredentials, enabled, expires, email, firstname, name,
					email2, language, address, postalcode, city, country, state, phone, cell);
		addMember(layout);

		prepareGroupsForm(readonly);
	}

	private void prepareGroupsForm(boolean readOnly) {
		DynamicForm form2 = new DynamicForm();
		form2.setValuesManager(vm);

		List<String> groupIds = new ArrayList<String>();
		GUIGroup[] groups = user.getGroups();
		if (groups != null && groups.length > 0) {
			for (int i = 0; i < groups.length; i++)
				if (groups[i].getType() == 0)
					groupIds.add(Long.toString(groups[i].getId()));
		}

		groupsItem = ItemFactory.newMultiComboBoxItem("groups", "groups", new GroupsDS(),
				groupIds.toArray(new String[0]));
		groupsItem.setDisabled(readOnly || user.isMemberOf("admin"));
		groupsItem.setValueField("id");
		groupsItem.setDisplayField("name");
		groupsItem.addChangedHandler(changedHandler);

		form2.setItems(groupsItem);
		addMember(form2, 2);
	}

	@SuppressWarnings("unchecked")
	boolean validate() {
		Map<String, Object> values = (Map<String, Object>) vm.getValues();
		vm.validate();
		if (!vm.hasErrors()) {
			user.setUserName((String) values.get("username"));
			user.setPasswordExpires(new Boolean(values.get("expires").toString()));
			user.setEnabled(new Boolean(values.get("eenabled").toString()));
			user.setName((String) values.get("name"));
			user.setFirstName((String) values.get("firstname"));
			user.setAddress((String) values.get("address"));
			user.setCity((String) values.get("city"));
			user.setCountry((String) values.get("country"));
			user.setState((String) values.get("state"));
			user.setPostalCode((String) values.get("postalcode"));
			user.setLanguage((String) values.get("language"));
			user.setPhone((String) values.get("phone"));
			user.setCell((String) values.get("cell"));
			user.setEmail((String) values.get("email"));
			user.setEmail2((String) values.get("email2"));

			if (user.getId() == 0L) {
				user.setNotifyCredentials(new Boolean(values.get("notifyCredentials").toString()));
				user.setPasswordExpired(new Boolean(values.get("changeAtLogin").toString()));
			}
		}

		String[] ids = groupsItem.getValues();
		if (ids == null || ids.length == 0) {
			SC.warn(I18N.message("usermustbelongtogroup"));
			Log.warn(I18N.message("usermustbelongtogroup"), I18N.message("usermustbelongtogroup"));
			return false;
		}

		GUIGroup[] groups = new GUIGroup[ids.length];
		for (int i = 0; i < ids.length; i++) {
			GUIGroup group = new GUIGroup();
			group.setId(Long.parseLong(ids[i]));
			groups[i] = group;
		}
		user.setGroups(groups);

		return !vm.hasErrors();
	}
}