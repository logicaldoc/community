package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve all users assigned to a stamp. It is based on Xml
 * parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class StampUsersDS extends DataSource {
	public StampUsersDS(long stampId) {
		setTitleField("label");
		setRecordXPath("/list/user");

		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);

		DataSourceTextField username = new DataSourceTextField("username");
		DataSourceTextField label = new DataSourceTextField("label");
		DataSourceImageField enabled = new DataSourceImageField("eenabled");
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField firstName = new DataSourceTextField("firstName");
		DataSourceTextField email = new DataSourceTextField("email");
		DataSourceTextField phone = new DataSourceTextField("phone");
		DataSourceTextField cell = new DataSourceTextField("cell");

		setFields(id, username, label, enabled, name, firstName, email, phone, cell);
		setDataURL("data/stampusers.xml?stampId=" + stampId);
		setClientOnly(true);
	}
}