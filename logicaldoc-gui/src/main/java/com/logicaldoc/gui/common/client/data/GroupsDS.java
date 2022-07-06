package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve all groups. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GroupsDS extends DataSource {
	public GroupsDS() {
		setTitleField("name");
		setRecordXPath("/list/group");
		DataSourceTextField id = new DataSourceTextField("id", I18N.message("id"));
		id.setPrimaryKey(true);

		DataSourceTextField name = new DataSourceTextField("name", I18N.message("name"));
		DataSourceTextField description = new DataSourceTextField("description");
		DataSourceTextField label = new DataSourceTextField("label");

		setFields(id, name, description, label);
		setDataURL("data/groups.xml?locale=" + I18N.getLocale());

		setClientOnly(true);
	}
}