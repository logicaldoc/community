package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle versions grid lists. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class FolderHistoryDS extends DataSource {
	private static Integer getDefaultMaxHistories() {
		try {
			return Session.get().getConfigAsInt("gui.maxhistories");
		} catch (Exception t) {
			return 100;
		}
	}

	public FolderHistoryDS(long folderId, Integer max) {
		setRecordXPath("/list/history");
		DataSourceTextField user = new DataSourceTextField("user");
		DataSourceTextField userId = new DataSourceTextField("userId");
		DataSourceDateTimeField date = new DataSourceDateTimeField("date");
		DataSourceTextField event = new DataSourceTextField("event");
		DataSourceTextField comment = new DataSourceTextField("comment");
		DataSourceTextField reason = new DataSourceTextField("reason");
		DataSourceTextField fileName = new DataSourceTextField("filename");
		DataSourceTextField path = new DataSourceTextField("path");
		DataSourceTextField sid = new DataSourceTextField("sid");
		DataSourceTextField key = new DataSourceTextField("key");
		DataSourceTextField ip = new DataSourceTextField("ip");
		DataSourceTextField device = new DataSourceTextField("device");
		DataSourceTextField geolocation = new DataSourceTextField("geolocation");

		setFields(userId, user, date, event, comment, reason, fileName, path, sid, key, ip, device, geolocation);
		setClientOnly(true);

		setDataURL("data/folderhistory.xml?id=" + folderId + "&locale=" + I18N.getLocale() + "&max="
				+ (max != null ? max : getDefaultMaxHistories()));
	}
}