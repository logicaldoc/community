package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle ticket grid lists.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4
 */
public class TicketsDS extends DataSource {

	public TicketsDS(Integer max) {
		setTitleField("ticketId");
		setRecordXPath("/list/ticket");

		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);
		DataSourceTextField ticketId = new DataSourceTextField("ticketId");
		DataSourceTextField docId = new DataSourceTextField("docId");
		DataSourceTextField folderId = new DataSourceTextField("folderId");
		DataSourceDateTimeField creation = new DataSourceDateTimeField("creation");
		DataSourceDateTimeField expired = new DataSourceDateTimeField("expired");
		DataSourceIntegerField count = new DataSourceIntegerField("count");
		DataSourceIntegerField views = new DataSourceIntegerField("views");
		DataSourceIntegerField maxCount = new DataSourceIntegerField("maxCount");
		DataSourceIntegerField maxViews = new DataSourceIntegerField("maxViews");
		DataSourceTextField suffix = new DataSourceTextField("suffix");
		DataSourceBooleanField valid = new DataSourceBooleanField("valid");
		DataSourceTextField fileName = new DataSourceTextField("filename");
		DataSourceIntegerField type = new DataSourceIntegerField("type");
		DataSourceImageField icon = new DataSourceImageField("icon");
		icon.setHidden(true);
		DataSourceBooleanField enabled = new DataSourceBooleanField("eenabled");

		setFields(id, enabled, ticketId, type, docId, creation, expired, count, maxCount, views, maxViews, suffix,
				valid, icon, fileName, folderId);
		setClientOnly(true);

		String url = "data/tickets.xml?1=1";
		if (max != null)
			url += "&max=" + max;
		setDataURL(url);
	}
}