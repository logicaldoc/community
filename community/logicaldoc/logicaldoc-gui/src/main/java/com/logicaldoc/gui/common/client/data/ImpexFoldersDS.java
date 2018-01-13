package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve all folders inside the impex/in folder. It is based on
 * Xml parsing.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class ImpexFoldersDS extends DataSource {

	public ImpexFoldersDS() {
		setTitleField("name");
		setRecordXPath("/list/folder");
		DataSourceTextField name = new DataSourceTextField("name");
		name.setPrimaryKey(true);
		DataSourceDateTimeField date = new DataSourceDateTimeField("date");

		setFields(name, date);
		setDataURL("data/impexfolders.xml");
		setClientOnly(true);
	}
}