package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve all import folders. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class EmailAccountsDS extends DataSource {
	public EmailAccountsDS(String type) {
		setTitleField("email");
		setRecordXPath("/list/account");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		DataSourceTextField email = new DataSourceTextField("email");
		DataSourceBooleanField enabled = new DataSourceBooleanField("eenabled");

		setFields(id, email, enabled);
		setDataURL("data/emailaccounts.xml" + (type != null ? "?type=" + type : ""));
		setClientOnly(true);
	}
}