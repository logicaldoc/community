package com.logicaldoc.gui.common.client.widgets;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.data.UsersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.PickerIcon;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * This widget allows for the selection of a user with the possibility to search
 * in groups
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.6
 */
public class UserSelector extends SelectItem {

	private static final String USERNAME = "username";

	public UserSelector(String name, String title, String groupIdOrName, boolean allowNull, boolean skipDisabled) {
		this(name, title, groupIdOrName, allowNull, skipDisabled, null);
	}

	public UserSelector(String name, String title, String groupIdOrName, boolean allowNull, boolean skipDisabled,
			List<FormItemIcon> additionalIcons) {
		setName(name);
		setTitle(I18N.message(title));
		setWrapTitle(false);

		ListGridField id = new ListGridField("id", I18N.message("id"));
		id.setHidden(true);
		ListGridField username = new ListGridField(USERNAME, I18N.message(USERNAME));
		ListGridField label = new ListGridField("label", I18N.message("name"));
		UserListGridField avatar = new UserListGridField();

		setValueField("id");
		setDisplayField(USERNAME);
		setSortField("label");
		setPickListWidth(300);
		setPickListFields(id, avatar, username, label);

		setPickListProperties(new UserPickListProperties());
		setOptionDataSource(new UsersDS(groupIdOrName, allowNull, skipDisabled));
		setHintStyle("hint");

		PickerIcon clear = new PickerIcon(PickerIcon.CLEAR, event -> {
			clearValue();
			setValue((String) null);
			fireEvent(new ChangedEvent(getJsObj()));
		});
		clear.setWidth(12);
		clear.setHeight(12);

		PickerIcon search = new PickerIcon(PickerIcon.SEARCH, event -> {
			UserSearchDialog dialog = new UserSearchDialog(UserSelector.this);
			dialog.show();
		});
		search.setWidth(12);
		search.setHeight(12);

		List<FormItemIcon> icons = new ArrayList<>();
		icons.add(clear);
		if (groupIdOrName == null || "".equals(groupIdOrName))
			icons.add(search);
		if (additionalIcons != null && !additionalIcons.isEmpty())
			icons.addAll(additionalIcons);
		setIcons(icons.toArray(new FormItemIcon[0]));
	}

	public GUIUser getUser() {
		GUIUser user = null;
		ListGridRecord selection = getSelectedRecord();
		if (selection != null) {
			user = new GUIUser();
			user.setId(selection.getAttributeAsLong("id"));
			user.setUsername(selection.getAttributeAsString(USERNAME));
		}
		return user;
	}
}