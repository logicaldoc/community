package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle automation triggers. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
public class AutomationTriggersDS extends DataSource {
	public AutomationTriggersDS(Long foldId, String event) {
		setRecordXPath("/list/trigger");

		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField events = new DataSourceTextField("events");
		DataSourceTextField folderId = new DataSourceTextField("folderId");
		DataSourceTextField folder = new DataSourceTextField("folder");
		DataSourceTextField routine = new DataSourceTextField("routine");
		DataSourceTextField routineId = new DataSourceTextField("routineId");
		DataSourceTextField automation = new DataSourceTextField("automation");
		DataSourceTextField cron = new DataSourceTextField("cron");
		DataSourceDateField date = new DataSourceDateField("date");

		setFields(id, events, folderId, folder, routineId, routine, automation, date, cron);
		setClientOnly(true);
		setDataURL("data/automationtriggers.xml?locale=" + I18N.getLocale()
				+ (foldId != null ? "&folderId=" + foldId : "") + (event != null ? "&event=" + event : ""));
	}
}