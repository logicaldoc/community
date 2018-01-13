package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve all import folders. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class EmailAccountsDS extends DataSource {
	public EmailAccountsDS(boolean withEmpty) {
		setTitleField("email");
		setRecordXPath("/list/account");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		DataSourceTextField email = new DataSourceTextField("email");
		DataSourceImageField enabled = new DataSourceImageField("eenabled");

		setFields(id, email, enabled);
		setDataURL("data/emailaccounts.xml");
		setClientOnly(true);
	}
}