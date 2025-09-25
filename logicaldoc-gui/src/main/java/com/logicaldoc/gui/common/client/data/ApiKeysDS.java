package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Data Source to handle API Keys grid lists. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.4
 */
public class ApiKeysDS extends DataSource {

	public ApiKeysDS() {
		setTitleField("filename");
		setRecordXPath("/list/apikey");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField key = new DataSourceTextField("key");
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceDateTimeField creation = new DataSourceDateTimeField("creation");
		DataSourceDateTimeField lastUsed = new DataSourceDateTimeField("lastUsed");

		setFields(id, name, key, creation, lastUsed);

		setDataURL("data/apikeys.xml");
	}
}