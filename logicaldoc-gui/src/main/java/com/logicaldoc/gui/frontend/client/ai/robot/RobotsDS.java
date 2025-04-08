package com.logicaldoc.gui.frontend.client.ai.robot;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle grids or fobots. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class RobotsDS extends DataSource {

	public RobotsDS() {
		setRecordXPath("/list/robot");

		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField label = new DataSourceTextField("label");
		DataSourceTextField avatar = new DataSourceTextField("avatar");
		avatar.setHidden(true);
		DataSourceTextField description = new DataSourceTextField("description");
		DataSourceBooleanField enabled = new DataSourceBooleanField("eenabled");

		setFields(id, enabled, name, label, description);
		setClientOnly(true);

		setDataURL("data/robots.xml?object=robot");
	}
}