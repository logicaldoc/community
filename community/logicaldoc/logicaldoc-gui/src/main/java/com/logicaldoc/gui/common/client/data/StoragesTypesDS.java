package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve all the types of storage. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class StoragesTypesDS extends DataSource {

	public StoragesTypesDS() {
		setTitleField("name");
		setRecordXPath("/list/storage");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField type = new DataSourceTextField("type");
		setFields(id, name, type);
		setDataURL("data/storages.xml?types=true");
		setClientOnly(true);
	}
}