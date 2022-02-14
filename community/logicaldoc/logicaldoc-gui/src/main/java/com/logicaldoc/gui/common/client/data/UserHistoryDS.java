package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle user history grid lists. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class UserHistoryDS extends DataSource {

	public static Integer getDefaultMaxHistories() {
		try {
			return Session.get().getConfigAsInt("gui.maxhistories");
		} catch (Throwable t) {
			return 100;
		}
	}

	public UserHistoryDS(long userId, Integer max) {
		this(userId, null, max);
	}

	public UserHistoryDS(long userId) {
		this(userId, null, null);
	}

	public UserHistoryDS(long userId, String event, Integer max) {
		setRecordXPath("/list/history");
		DataSourceTextField user = new DataSourceTextField("user");

		DataSourceTextField folderId = new DataSourceTextField("folderId");

		DataSourceDateTimeField date = new DataSourceDateTimeField("date");
		DataSourceTextField evnt = new DataSourceTextField("event");
		DataSourceTextField comment = new DataSourceTextField("comment");
		DataSourceTextField reason = new DataSourceTextField("reason");
		DataSourceTextField sid = new DataSourceTextField("sid");
		DataSourceTextField ip = new DataSourceTextField("id");
		DataSourceTextField device = new DataSourceTextField("device");
		DataSourceTextField geolocation = new DataSourceTextField("geolocation");

		setFields(user, date, evnt, ip, device, geolocation, comment, reason, sid, folderId);
		setClientOnly(true);

		String url = "data/userhistory.xml?id=" + userId + "&locale=" + I18N.getLocale();
		if (event != null)
			url += "&event=" + event;
		url += "&max=" + (max != null ? max : getDefaultMaxHistories());
		setDataURL(url);
	}
}