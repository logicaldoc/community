package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve a list of currently logged users. It is based on Xml
 * parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.0.1
 */
public class OnlineUsersDS extends DataSource {

	public OnlineUsersDS() {
		setTitleField("sid");
		setRecordXPath("/list/user");

		DataSourceTextField username = new DataSourceTextField("username");
		username.setPrimaryKey(true);

		setFields(username);
		setDataURL("data/onlineusers.xml");
		setClientOnly(true);
	}
}