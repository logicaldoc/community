package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve all system properties. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public class PropertiesDS extends DataSource {
	public PropertiesDS() {
		setTitleField("label");
		setRecordXPath("/list/property");

		DataSourceTextField id = new DataSourceTextField("name");
		id.setPrimaryKey(true);

		DataSourceTextField username = new DataSourceTextField("value");
		DataSourceTextField scope = new DataSourceTextField("scope");

		setFields(id, username, scope);
		setDataURL("data/properties.xml");
		setClientOnly(true);
	}
}