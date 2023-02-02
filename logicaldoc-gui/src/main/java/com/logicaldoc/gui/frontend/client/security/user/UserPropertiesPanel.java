package com.logicaldoc.gui.frontend.client.security.user;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIGroup;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.data.GroupsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.Avatar;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Shows users's standard properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class UserPropertiesPanel extends HLayout {
	private DynamicForm form1 = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

	private GUIUser user;

	private UsersPanel usersPanel;

	private ChangedHandler changedHandler;

	private Canvas idLabel;

	private VLayout layout = new VLayout();

	private MultiComboBoxItem groupsItem;

	public UserPropertiesPanel(GUIUser user, ChangedHandler changedHandler, UsersPanel usersPanel) {
		this.usersPanel = usersPanel;
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
		setAlign(Alignment.LEFT);

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

		final CheckboxItem guest = prepareGuestItem(readonly);

		CheckboxItem notifyCredentials = new CheckboxItem("notifyCredentials", I18N.message("notifycredentials"));
		notifyCredentials.setValue(true);
		notifyCredentials.setVisible(user.getId() == 0);

		TextItem username = prepareUsername(readonly);

		TextItem firstname = prepareFirstname(readonly);

		TextItem name = prepareName(readonly);

		TextItem address = prepareAddress(readonly);

		TextItem postalcode = preparePostalcode(readonly);

		TextItem city = prepareCity(readonly);

		TextItem country = prepareCountry(readonly);

		SelectItem language = prepareLanguageSelector(readonly);

		TextItem state = prepareStateItem(readonly);

		TextItem phone = preparePhoneItem(readonly);

		TextItem cell = prepareCellItem(readonly);

		TextItem email = prepareEmailItem(readonly);

		TextItem email2 = prepareEmail2Item(readonly);

		ComboBoxItem timeZone = prepareTimeZoneSelector(readonly);

		if (user.getId() == 0L)
			form1.setItems(notifyCredentials, guest, username, email, firstname, name, email2, language, address,
					postalcode, city, country, state, phone, cell, timeZone);
		else
			form1.setItems(username, notifyCredentials, guest, email, firstname, name, email2, language, address,
					postalcode, city, country, state, phone, cell, timeZone);
		addMember(layout);

		prepareGroupsForm(readonly);

		addAvatar();
	}

	private void addAvatar() {
		if (user.getId() != 0L) {
			Avatar avatar = new Avatar(user.getId(), new AsyncCallback<Void>() {

				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void result) {
					if (usersPanel != null)
						usersPanel.updateRecord(user);
				}
			});
			addMember(avatar);
		}
	}

	private ComboBoxItem prepareTimeZoneSelector(boolean readonly) {
		ComboBoxItem timeZone = ItemFactory.newTimeZoneSelector("timezone", "timezone", user.getTimeZone());
		timeZone.setRequired(false);
		timeZone.setDisabled(readonly);
		if (!readonly)
			timeZone.addChangedHandler(changedHandler);
		return timeZone;
	}

	private TextItem prepareEmail2Item(boolean readonly) {
		TextItem email2 = ItemFactory.newEmailItem("email2", "secondaryemail", false);
		email2.setRequired(false);
		email2.setDisabled(readonly);
		email2.setValue(user.getEmail2());
		if (!readonly)
			email2.addChangedHandler(changedHandler);
		return email2;
	}

	private TextItem prepareEmailItem(boolean readonly) {
		TextItem email = ItemFactory.newEmailItem("email", "email", false);
		email.setRequired(true);
		email.setDisabled(readonly);
		email.setValue(user.getEmail());
		if (!readonly)
			email.addChangedHandler(changedHandler);
		return email;
	}

	private TextItem prepareCellItem(boolean readonly) {
		TextItem cell = ItemFactory.newTextItem("cell", user.getCell());
		cell.setDisabled(readonly);
		if (!readonly)
			cell.addChangedHandler(changedHandler);
		return cell;
	}

	private TextItem preparePhoneItem(boolean readonly) {
		TextItem phone = ItemFactory.newTextItem("phone", user.getPhone());
		phone.setDisabled(readonly);
		if (!readonly)
			phone.addChangedHandler(changedHandler);
		return phone;
	}

	private TextItem prepareStateItem(boolean readonly) {
		TextItem state = ItemFactory.newTextItem("state", user.getState());
		state.setDisabled(readonly);
		if (!readonly)
			state.addChangedHandler(changedHandler);
		return state;
	}

	private SelectItem prepareLanguageSelector(boolean readonly) {
		SelectItem language = ItemFactory.newLanguageSelector("language", false, true);
		language.setDisabled(readonly || (Session.get().isDemo() && Session.get().getUser().getId() == 1));
		language.setValue(user.getLanguage());
		if (!readonly)
			language.addChangedHandler(changedHandler);
		return language;
	}

	private TextItem prepareCountry(boolean readonly) {
		TextItem country = ItemFactory.newTextItem("country", user.getCountry());
		country.setDisabled(readonly);
		if (!readonly)
			country.addChangedHandler(changedHandler);
		return country;
	}

	private TextItem prepareCity(boolean readonly) {
		TextItem city = ItemFactory.newTextItem("city", user.getCity());
		city.setDisabled(readonly);
		if (!readonly)
			city.addChangedHandler(changedHandler);
		return city;
	}

	private TextItem preparePostalcode(boolean readonly) {
		TextItem postalcode = ItemFactory.newTextItem("postalcode", user.getPostalCode());
		postalcode.setDisabled(readonly);
		if (!readonly)
			postalcode.addChangedHandler(changedHandler);
		return postalcode;
	}

	private TextItem prepareAddress(boolean readonly) {
		TextItem address = ItemFactory.newTextItem("address", user.getAddress());
		address.setDisabled(readonly);
		if (!readonly)
			address.addChangedHandler(changedHandler);
		return address;
	}

	private TextItem prepareName(boolean readonly) {
		TextItem name = ItemFactory.newTextItem("name", "lastname", user.getName());
		name.setRequired(true);
		name.setDisabled(readonly);
		if (!readonly)
			name.addChangedHandler(changedHandler);
		return name;
	}

	private TextItem prepareFirstname(boolean readonly) {
		TextItem firstname = ItemFactory.newTextItem("firstname", user.getFirstName());
		firstname.setRequired(true);
		firstname.setDisabled(readonly);
		if (!readonly)
			firstname.addChangedHandler(changedHandler);
		return firstname;
	}

	private TextItem prepareUsername(boolean readonly) {
		TextItem username = ItemFactory.newTextItem("username", user.getUsername());
		username.setRequired(true);
		username.setSelectOnFocus(true);
		username.setDisabled(readonly || user.getId() != 0);
		if (!readonly)
			username.addChangedHandler(changedHandler);
		return username;
	}

	private CheckboxItem prepareGuestItem(boolean readonly) {
		final CheckboxItem guest = new CheckboxItem("readonly", I18N.message("readonly"));
		guest.setValue(user.isReadOnly());
		if (readonly || "admin".equals(user.getUsername())) {
			guest.setDisabled(true);
		} else {
			addGuestChangeHandlers(guest);
		}
		return guest;
	}

	private void addGuestChangeHandlers(final CheckboxItem guest) {
		guest.addChangedHandler(changedHandler);
		guest.addChangedHandler((ChangedEvent event) -> {
			if (guest.getValueAsBoolean()) {
				groupsItem.clearValue();
				Record[] records = groupsItem.getOptionDataSource().getCacheData();
				if (records != null)
					for (Record rec : records) {
						if (rec.getAttributeAsString("name").equals(Constants.GROUP_GUEST)) {
							groupsItem.setValues(rec.getAttributeAsString("id"));
						}
					}
			}
		});
	}

	private void prepareGroupsForm(boolean readOnly) {
		List<String> groupIds = new ArrayList<String>();
		GUIGroup[] groups = user.getGroups();
		if (groups != null && groups.length > 0) {
			for (int i = 0; i < groups.length; i++)
				if (groups[i].getType() == 0)
					groupIds.add(Long.toString(groups[i].getId()));
		}

		groupsItem = ItemFactory.newMultiComboBoxItem("groups", "groups", new GroupsDS(),
				groupIds.toArray(new String[0]));
		groupsItem.setDisabled(readOnly || "admin".equals(user.getUsername())
				|| ("admin" + Session.get().getTenantName()).equals(user.getUsername()));
		groupsItem.setValueField("id");
		groupsItem.setDisplayField("name");
		groupsItem.addChangedHandler(changedHandler);

		DynamicForm form2 = new DynamicForm();
		form2.setValuesManager(vm);
		form2.setItems(groupsItem);
		form2.setWidth(1);
		addMember(form2, 2);
	}

	@SuppressWarnings("unchecked")
	boolean validate() {
		Map<String, Object> values = (Map<String, Object>) vm.getValues();
		vm.validate();
		if (!vm.hasErrors()) {
			user.setUsername((String) values.get("username"));
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
			user.setTimeZone((String) values.get("timezone"));

			if (user.getId() == 0L)
				user.setNotifyCredentials(Boolean.parseBoolean(values.get("notifyCredentials").toString()));
		}

		String[] ids = groupsItem.getValues();
		if (ids == null || ids.length == 0) {
			SC.warn(I18N.message("usermustbelongtogroup"));
			GuiLog.warn(I18N.message("usermustbelongtogroup"), I18N.message("usermustbelongtogroup"));
			return false;
		}

		GUIGroup[] groups = new GUIGroup[ids.length];
		for (int i = 0; i < ids.length; i++) {
			GUIGroup group = new GUIGroup();
			group.setId(Long.parseLong(ids[i]));
			groups[i] = group;
		}
		user.setGroups(groups);

		if (Boolean.parseBoolean(values.get("readonly").toString())) {
			user.setType(GUIUser.TYPE_READONLY);
			user.setGroups(new GUIGroup[0]);
		} else
			user.setType(GUIUser.TYPE_DEFAULT);

		return !vm.hasErrors();
	}
}