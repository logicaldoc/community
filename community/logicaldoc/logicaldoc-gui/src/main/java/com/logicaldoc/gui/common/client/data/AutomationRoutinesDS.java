package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Data source to retrieve all forms. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class AutomationRoutinesDS extends DataSource {

	public AutomationRoutinesDS(boolean showEmpty) {
		setTitleField("name");
		setRecordXPath("/list/routine");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField description = new DataSourceTextField("description");
		DataSourceTextField body = new DataSourceTextField("automation");

		setFields(id, name, description, body);
		setDataURL("data/automationroutines.xml" + (showEmpty ? "?showEmpty=true" : ""));
		setClientOnly(true);
	}
}