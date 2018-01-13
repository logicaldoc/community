package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Data source to retrieve all retention policies. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 7.2
 */
public class RetentionPoliciesDS extends DataSource {

	public RetentionPoliciesDS() {
		setTitleField("name");
		setRecordXPath("/list/policy");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField days = new DataSourceTextField("days");
		DataSourceTextField dateOption = new DataSourceTextField("dateOption");
		DataSourceTextField template = new DataSourceTextField("template");
		DataSourceTextField action = new DataSourceTextField("action");
		DataSourceImageField enabled = new DataSourceImageField("eenabled");

		setFields(id, name, days, dateOption, template, action, enabled);
		setDataURL("data/retentionpolicies.xml");
		setClientOnly(true);
	}
}