package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle user history grid lists. It is based on Xml parsing
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class UserHistoryDS extends DataSource {
	public UserHistoryDS(long userId) {
		setRecordXPath("/list/history");
		DataSourceTextField user = new DataSourceTextField("user");

		DataSourceTextField folderId = new DataSourceTextField("folderId");

		DataSourceDateTimeField date = new DataSourceDateTimeField("date");
		DataSourceTextField event = new DataSourceTextField("event");
		DataSourceTextField comment = new DataSourceTextField("comment");
		DataSourceTextField sid = new DataSourceTextField("sid");
		DataSourceTextField ip = new DataSourceTextField("id");

		setFields(user, date, event, ip, comment, sid, folderId);
		setClientOnly(true);
		setDataURL("data/userhistory.xml?id=" + userId + "&locale=" + I18N.getLocale());
	}
}