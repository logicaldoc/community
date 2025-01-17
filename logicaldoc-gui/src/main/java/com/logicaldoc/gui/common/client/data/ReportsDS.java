package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Data source to retrieve all custom reports. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ReportsDS extends DataSource {

	public ReportsDS() {
		setTitleField("name");
		setRecordXPath("/list/report");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField outputFolderId = new DataSourceTextField("outputFolderId");
		DataSourceTextField outputDocId = new DataSourceTextField("outputDocId");

		DataSourceBooleanField running = new DataSourceBooleanField("running");
		DataSourceIntegerField status = new DataSourceIntegerField("status");
		DataSourceBooleanField enabled = new DataSourceBooleanField("eenabled");

		DataSourceDateTimeField lastRun = new DataSourceDateTimeField("lastRun");
		DataSourceDateTimeField lastModified = new DataSourceDateTimeField("lastModified");

		setFields(id, name, enabled, running, status, lastRun, outputFolderId, outputDocId,
				lastModified);
		setDataURL("data/reports.xml");
		setClientOnly(true);
	}
}