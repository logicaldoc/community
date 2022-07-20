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
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
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
		ListGridField username = new ListGridField("username", I18N.message("username"));
		ListGridField label = new ListGridField("label", I18N.message("name"));
		UserListGridField avatar = new UserListGridField();

		setValueField("id");
		setDisplayField("username");
		setSortField("label");
		setPickListWidth(300);
		setPickListFields(id, avatar, username, label);

		setPickListProperties(new UserPickListProperties());
		setOptionDataSource(new UsersDS(groupIdOrName, allowNull, skipDisabled));
		setHintStyle("hint");

		PickerIcon clear = new PickerIcon(PickerIcon.CLEAR, new FormItemClickHandler() {
			@Override
			public void onFormItemClick(FormItemIconClickEvent event) {
				clearValue();
				setValue((String) null);
				fireEvent(new ChangedEvent(getJsObj()));
			}
		});
		clear.setWidth(12);
		clear.setHeight(12);

		PickerIcon search = new PickerIcon(PickerIcon.SEARCH, new FormItemClickHandler() {
			@Override
			public void onFormItemClick(FormItemIconClickEvent event) {
				UserSearchDialog dialog = new UserSearchDialog(UserSelector.this);
				dialog.show();
			}
		});
		search.setWidth(12);
		search.setHeight(12);

		List<FormItemIcon> icons = new ArrayList<FormItemIcon>();
		icons.add(clear);
		if (groupIdOrName == null || "".equals(groupIdOrName))
			icons.add(search);
		if (additionalIcons != null && !additionalIcons.isEmpty())
			icons.addAll(additionalIcons);
		setIcons(icons.toArray(new FormItemIcon[0]));

		// If we use a formatter here, then the user is not able to use the
		// keyboard to search among the elements of the picklist
//		setValueFormatter(new AvatarFormItemValueFormatter());
	}

	public GUIUser getUser() {
		GUIUser user = null;
		ListGridRecord selection = getSelectedRecord();
		if (selection != null) {
			user = new GUIUser();
			user.setId(selection.getAttributeAsLong("id"));
			user.setUsername(selection.getAttributeAsString("username"));
		}
		return user;
	}

//	private class AvatarFormItemValueFormatter implements FormItemValueFormatter {
//		@Override
//		public String formatValue(Object value, Record record, DynamicForm form, FormItem item) {
//			if (value == null)
//				return "";
//			else
//				return Util.avatarWithText(value.toString(), value.toString());
//		}
//	}
}