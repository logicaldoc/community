package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve the feed messages.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class FeedMessagesDS extends DataSource {

	public FeedMessagesDS() {
		setTitleField("title");
		setRecordXPath("/list/feedmessage");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setRequired(true);

		DataSourceTextField guid = new DataSourceTextField("guid");
		DataSourceTextField text = new DataSourceTextField("text");
		DataSourceTextField description = new DataSourceTextField("description");
		DataSourceTextField link = new DataSourceTextField("link");
		DataSourceDateTimeField pubDate = new DataSourceDateTimeField("pubDate");
		DataSourceIntegerField read = new DataSourceIntegerField("read");

		setFields(id, guid, text, description, link, pubDate, read);
		setClientOnly(true);
		setDataURL("data/feedmessage.xml");
	}
}
