package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Data source retrieve informations about the log files
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.2
 */
public class LogAppendersDS extends DataSource {
	public LogAppendersDS() {
		setTitleField("label");
		setRecordXPath("/list/appender");
		DataSourceTextField name = new DataSourceTextField("name");
		name.setPrimaryKey(true);
		name.setRequired(true);
		name.setHidden(true);

		DataSourceTextField label = new DataSourceTextField("label");

		setFields(name, label);
		setClientOnly(true);
		setDataURL("data/log.xml");
	}
}