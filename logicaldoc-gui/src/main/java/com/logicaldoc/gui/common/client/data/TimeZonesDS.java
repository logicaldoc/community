package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource retrieve the available Skins
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.1
 */
public class TimeZonesDS extends DataSource {
	public TimeZonesDS() {
		setTitleField("label");
		setRecordXPath("/list/timezone");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setRequired(true);

		setFields(id);
		setClientOnly(true);
		setDataURL("data/timezones.xml");
	}
}