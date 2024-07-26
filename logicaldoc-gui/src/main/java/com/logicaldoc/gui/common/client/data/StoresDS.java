package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve all the stores. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.3
 */
public class StoresDS extends DataSource {

	public StoresDS(boolean withEmpty, boolean parameters) {
		setTitleField("name");
		setRecordXPath("/list/store");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField path = new DataSourceTextField("path");
		DataSourceImageField write = new DataSourceImageField("write");
		setFields(write, id, name, path);
		setDataURL("data/stores.xml?empty=" + withEmpty + "&parameters=" + parameters);
		setClientOnly(true);
	}
}