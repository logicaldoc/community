package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve all syndications. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.2
 */
public class SyndicationsDS extends DataSource {

	public SyndicationsDS() {
		setTitleField("src");
		setRecordXPath("/list/syndication");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField url = new DataSourceTextField("url");
		DataSourceTextField targetPath = new DataSourceTextField("targetPath");
		DataSourceBooleanField enabled = new DataSourceBooleanField("eenabled");

		setFields(id, name, url, targetPath, enabled);
		setDataURL("data/syndications.xml");
		setClientOnly(true);
	}
}