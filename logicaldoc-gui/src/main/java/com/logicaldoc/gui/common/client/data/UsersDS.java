package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceDateField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
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
		DataSourceTextField city = new DataSourceTextField("city");
		DataSourceTextField department = new DataSourceTextField("department");
		DataSourceTextField building = new DataSourceTextField("building");
		DataSourceTextField company = new DataSourceTextField("company");
		DataSourceTextField organizationalUnit = new DataSourceTextField("organizationalUnit");
		DataSourceTextField usergroup = new DataSourceTextField("usergroup");
		DataSourceTextField groups = new DataSourceTextField("groups");
		DataSourceImageField avatar = new DataSourceImageField("avatar", I18N.message("avatar"), 16);
		DataSourceDateField expire = new DataSourceDateField("expire");
		DataSourceDateField lastlogin = new DataSourceDateField("lastLogin");
		DataSourceDateField creation = new DataSourceDateField("creation");
		DataSourceIntegerField source = new DataSourceIntegerField("source");

		setFields(id, username, label, eenabled, enabled, name, firstName, email, phone, cell, city, company,
				department, organizationalUnit, building, groups, usergroup, guest, lastlogin, expire, creation, avatar,
				timeZone, source);
		setDataURL("data/users.xml?1=1" + (groupIdOrName != null ? "&groupId=" + groupIdOrName : "") + "&required="
				+ required + "&skipdisabled=" + skipDisabled);
		setClientOnly(true);
	}

	public static UsersDS get(long groupId) {
		return new UsersDS(Long.toString(groupId), true, false);
	}
}