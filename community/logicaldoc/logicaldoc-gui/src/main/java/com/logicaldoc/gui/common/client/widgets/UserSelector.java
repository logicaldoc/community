package com.logicaldoc.gui.common.client.widgets;

import com.logicaldoc.gui.common.client.data.UsersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.form.fields.PickerIcon;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
import com.smartgwt.client.widgets.grid.ListGridField;

/**
 * This widget allows for the selection of a user with the possibility to search
 * in groups
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.6
 */
public class UserSelector extends SelectItem {
	public UserSelector(String name, String title, String groupIdOrName, boolean allowNull) {
		setName(name);
		setTitle(I18N.message(title));
		setWrapTitle(false);
		ListGridField username = new ListGridField("username", I18N.message("username"));
		ListGridField label = new ListGridField("label", I18N.message("name"));
		setValueField("id");
		setDisplayField("username");
		setPickListWidth(300);
		setPickListFields(username, label);
		setOptionDataSource(new UsersDS(groupIdOrName, allowNull));
		setHintStyle("hint");

		PickerIcon clear = new PickerIcon(PickerIcon.CLEAR, new FormItemClickHandler() {
			@Override
			public void onFormItemClick(FormItemIconClickEvent event) {
				clearValue();
				setValue((String) null);
				fireEvent(new ChangedEvent(getJsObj()));
			}
		});

		PickerIcon search = new PickerIcon(PickerIcon.SEARCH, new FormItemClickHandler() {
			@Override
			public void onFormItemClick(FormItemIconClickEvent event) {
				UserSearchDialog dialog = new UserSearchDialog(UserSelector.this);
				dialog.show();
			}
		});

		if (groupIdOrName != null && !"".equals(groupIdOrName))
			setIcons(clear);
		else
			setIcons(clear, search);
	}
}