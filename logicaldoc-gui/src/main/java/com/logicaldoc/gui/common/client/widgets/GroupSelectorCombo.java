package com.logicaldoc.gui.common.client.widgets;

import com.logicaldoc.gui.common.client.data.GroupsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;

/**
 * This widget allows for the selection of a set of groups
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.1
 */
public class GroupSelectorCombo extends MultiComboBoxItem {

	public GroupSelectorCombo(String name, String title) {
		setName(name);
		setTitle(I18N.message(title));
		setWrapTitle(false);

		setValueField("id");
		setDisplayField("name");
		setOptionDataSource(new GroupsDS());
		setHintStyle("hint");

		setWidth("*");

		setValueFormatter((value, rec, form, item) -> {
			return AwesomeFactory.getIconHtml("user-friends") + "&nbsp;" + value;
		});
	}

	public long[] getGroupIds() {
		long[] ids = new long[getValues().length];
		for (int i = 0; i < ids.length; i++)
			ids[i] = Long.parseLong(getValues()[i]);
		return ids;
	}
}