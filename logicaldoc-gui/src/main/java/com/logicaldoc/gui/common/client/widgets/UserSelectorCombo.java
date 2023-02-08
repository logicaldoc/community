package com.logicaldoc.gui.common.client.widgets;

import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.data.UsersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.FormItemValueFormatter;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * This widget allows for the selection of a set of users
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class UserSelectorCombo extends MultiComboBoxItem {

	public UserSelectorCombo(String name, String title, String groupIdOrName, boolean allowNull, boolean skipDisabled) {
		setName(name);
		setTitle(I18N.message(title));
		setWrapTitle(false);

		setValueField("id");
		setDisplayField("username");
		setOptionDataSource(new UsersDS(groupIdOrName, allowNull, skipDisabled));
		setHintStyle("hint");

		setEmptyDisplayValue(I18N.message("selectuser"));
		
		setWidth("*");

		setValueFormatter(new AvatarFormItemValueFormatter());
	}

	public long[] getUserIds() {
		long[] ids = new long[getValues().length];
		for (int i = 0; i < ids.length; i++)
			ids[i] = Long.parseLong(getValues()[i]);
		return ids;
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

	private class AvatarFormItemValueFormatter implements FormItemValueFormatter {
		@Override
		public String formatValue(Object value, Record rec, DynamicForm form, FormItem item) {
			if (value == null)
				return "";
			else
				return Util.avatarWithText(value.toString(), value.toString());
		}
	}
}