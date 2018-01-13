package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Data source to retrieve all forms. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class FormsDS extends DataSource {

	public FormsDS() {
		setTitleField("name");
		setRecordXPath("/list/form");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField templateId = new DataSourceTextField("templateId");

		setFields(id, name, templateId);
		setDataURL("data/forms.xml");
		setClientOnly(true);
	}
}