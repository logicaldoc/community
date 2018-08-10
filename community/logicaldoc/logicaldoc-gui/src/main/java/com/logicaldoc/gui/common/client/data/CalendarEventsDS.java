package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceSequenceField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Data source to retrieve calendar events.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.7
 */
public class CalendarEventsDS extends DataSource {
	public CalendarEventsDS(Long docId) {
		setRecordXPath("/list/event");
		DataSourceSequenceField id = new DataSourceSequenceField("eventId");
		id.setPrimaryKey(true);

		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField description = new DataSourceTextField("description");
		DataSourceDateTimeField start = new DataSourceDateTimeField("startDate");
		DataSourceDateTimeField end = new DataSourceDateTimeField("endDate");
		DataSourceDateTimeField eventWindowStyle = new DataSourceDateTimeField("eventWindowStyle");
		DataSourceTextField parentId = new DataSourceTextField("parentId");

		setFields(id, name, description, start, end, eventWindowStyle, parentId);
		setClientOnly(true);
		setDataURL("data/calendarevents.xml" + (docId != null ? "?docId=" + docId : ""));
	}
}