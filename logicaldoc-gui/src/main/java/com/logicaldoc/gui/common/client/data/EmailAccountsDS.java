package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve all import folders. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class EmailAccountsDS extends DataSource {
	public EmailAccountsDS(boolean withEmpty, String type) {
		setTitleField("email");
		setRecordXPath("/list/account");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		DataSourceTextField email = new DataSourceTextField("email");
		DataSourceImageField enabled = new DataSourceImageField("eenabled");
		DataSourceIntegerField emails = new DataSourceIntegerField("emails");

		setFields(id, email, enabled);
		setDataURL("data/emailaccounts.xml" + (type != null ? "?type=" + type : ""));
		setClientOnly(true);
	}
}