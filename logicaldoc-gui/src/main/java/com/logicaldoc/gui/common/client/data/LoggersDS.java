package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Data source retrieve informations about the appenders
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.2
 */
public class LoggersDS extends DataSource {
	public LoggersDS() {
		setTitleField("label");
		setRecordXPath("/list/logger");
		DataSourceTextField name = new DataSourceTextField("name");
		name.setPrimaryKey(true);
		name.setRequired(true);
		name.setHidden(false);

		DataSourceTextField level = new DataSourceTextField("level");

		DataSourceBooleanField additivity = new DataSourceBooleanField("additivity");

		DataSourceBooleanField reserved = new DataSourceBooleanField("reserved");
		reserved.setHidden(true);

		setFields(name, level, additivity, reserved);
		setClientOnly(true);
		setDataURL("data/log.xml?loggers=true");
	}
}