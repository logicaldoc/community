package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve all the parsers.
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.0
 */
public class ParsersDS extends DataSource {

	private static ParsersDS instance;

	public static ParsersDS get() {
		if (instance == null)
			instance = new ParsersDS();
		return instance;
	}

	private ParsersDS() {
		setTitleField("name");
		setRecordXPath("/list/parser");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);
		DataSourceImageField icon = new DataSourceImageField("icon");
		DataSourceTextField extension = new DataSourceTextField("extension");
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField aliases = new DataSourceTextField("aliases");

		setFields(id, icon, extension, name, aliases);
		setClientOnly(true);
		setDataURL("data/parsers.xml");
	}
}