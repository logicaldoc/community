package com.logicaldoc.gui.common.client.widgets;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.data.UsersDS;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.UserListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
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
		this(name, title, groupIdOrName, allowNull, skipDisabled, true, null);
	}

	public UserSelector(String name, String title, String groupIdOrName, boolean allowNull, boolean skipDisabled,
			boolean withClear) {
		this(name, title, groupIdOrName, allowNull, skipDisabled, withClear, null);
	}

	public UserSelector(String name, String title, String groupIdOrName, boolean allowNull, boolean skipDisabled,
			boolean withClear, List<FormItemIcon> additionalIcons) {
		setName(name);
		setTitle(I18N.message(title));
		setWrapTitle(false);

		ListGridField id = new IdListGridField();
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

		FormItemIcon clear = new FormItemIcon();
		clear.setPrompt(I18N.message("clear"));
		clear.setSrc("[SKIN]/icons/trash.png");
		clear.setWidth(12);
		clear.setHeight(12);
		clear.addFormItemClickHandler(click -> {
			clearValue();
			setValue((String) null);
			fireUserChanged();
		});

		FormItemIcon search = new FormItemIcon();
		search.setPrompt(I18N.message("search"));
		search.setSrc("[SKIN]/icons/magnifying-glass.png");
		search.setWidth(12);
		search.setHeight(12);
		search.addFormItemClickHandler(click -> new UserSearchDialog(UserSelector.this).show());

		List<FormItemIcon> icons = new ArrayList<>();
		if (withClear)
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

	void fireUserChanged() {
		fireEvent(new ChangedEvent(getJsObj()));
	}
}