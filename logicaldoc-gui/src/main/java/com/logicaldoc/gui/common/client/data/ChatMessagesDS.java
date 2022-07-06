package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle user history grid lists. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ChatMessagesDS extends DataSource {
	public ChatMessagesDS() {
		setRecordXPath("/list/message");

		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField username = new DataSourceTextField("username");
		DataSourceTextField message = new DataSourceTextField("message");
		DataSourceDateTimeField date = new DataSourceDateTimeField("date");
		setFields(id, date, username, message);
		setClientOnly(true);
		setDataURL("data/chatmessages.xml");
	}
}