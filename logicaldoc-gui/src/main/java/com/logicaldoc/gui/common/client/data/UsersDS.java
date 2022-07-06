package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceDateField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve all users. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class UsersDS extends DataSource {
	public UsersDS(String groupIdOrName, boolean required, boolean skipDisabled) {
		setTitleField("label");
		setRecordXPath("/list/user");

		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);

		DataSourceTextField username = new DataSourceTextField("username");
		DataSourceTextField label = new DataSourceTextField("label");
		DataSourceBooleanField guest = new DataSourceBooleanField("guest");
		guest.setHidden(true);
		DataSourceImageField eenabled = new DataSourceImageField("enabledIcon");
		DataSourceBooleanField enabled = new DataSourceBooleanField("eenabled");
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField firstName = new DataSourceTextField("firstName");
		DataSourceTextField email = new DataSourceTextField("email");
		DataSourceTextField timeZone = new DataSourceTextField("timeZone");
		DataSourceTextField phone = new DataSourceTextField("phone");
		DataSourceTextField cell = new DataSourceTextField("cell");
		DataSourceTextField usergroup = new DataSourceTextField("usergroup");
		DataSourceTextField groups = new DataSourceTextField("groups");
		DataSourceImageField avatar = new DataSourceImageField("avatar", I18N.message("avatar"), 16);
		DataSourceDateField expire = new DataSourceDateField("expire");

		setFields(id, username, label, eenabled, enabled, name, firstName, email, phone, cell, groups, usergroup, guest,
				expire, avatar, timeZone);
		setDataURL("data/users.xml?1=1" + (groupIdOrName != null ? "&groupId=" + groupIdOrName : "") + "&required="
				+ required + "&skipdisabled=" + skipDisabled);
		setClientOnly(true);
	}

	public static UsersDS get(long groupId) {
		return new UsersDS(Long.toString(groupId), true, false);
	}
}