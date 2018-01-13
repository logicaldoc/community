package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve the system messages for the current user.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class MessagesDS extends DataSource {

	public MessagesDS() {
		setTitleField("subject");
		setRecordXPath("/list/message");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setRequired(true);

		DataSourceTextField subject = new DataSourceTextField("subject");
		DataSourceTextField text = new DataSourceTextField("text");
		DataSourceImageField priority = new DataSourceImageField("priority");
		DataSourceTextField from = new DataSourceTextField("from");
		DataSourceDateTimeField sent = new DataSourceDateTimeField("sent");
		DataSourceTextField read = new DataSourceTextField("read");

		setFields(id, subject, text, priority, from, sent, read);
		setClientOnly(true);
		setDataURL("data/messages.xml");
	}
}