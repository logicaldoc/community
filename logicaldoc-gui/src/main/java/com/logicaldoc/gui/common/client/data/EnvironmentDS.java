package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource get the environment variables
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.4
 */
public class EnvironmentDS extends DataSource {
	public EnvironmentDS() {
		setRecordXPath("/list/entry");
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField scope = new DataSourceTextField("scope");
		DataSourceTextField value = new DataSourceTextField("value");

		setFields(name, value, scope);
		setClientOnly(true);
		setDataURL("data/environment.xml");
	}
}