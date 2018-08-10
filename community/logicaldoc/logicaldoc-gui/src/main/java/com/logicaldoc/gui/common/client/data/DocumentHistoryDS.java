package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle versions grid lists. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentHistoryDS extends DataSource {
	private static final int MAX = 100;

	public DocumentHistoryDS(long docId, Integer max) {
		init("data/documenthistory.xml?docId=" + docId + "&locale=" + I18N.getLocale() + "&max="
				+ (max != null ? max : MAX));
	}

	public DocumentHistoryDS(Long userId, Long docId, String event, Integer max) {
		String url = "data/documenthistory.xml?locale=" + I18N.getLocale();
		if (userId != null)
			url += "&userId=" + userId;
		if (event != null)
			url += "&event=" + event;
		if (docId != null)
			url += "&docId=" + docId;
		init(url + "&max=" + (max != null ? max : MAX));
	}

	private void init(String url) {
		setRecordXPath("/list/history");
		DataSourceTextField user = new DataSourceTextField("user");
		DataSourceDateTimeField date = new DataSourceDateTimeField("date");
		DataSourceTextField event = new DataSourceTextField("event");
		DataSourceTextField comment = new DataSourceTextField("comment");
		DataSourceTextField filename = new DataSourceTextField("filename");
		DataSourceTextField version = new DataSourceTextField("version");
		DataSourceImageField icon = new DataSourceImageField("icon");
		DataSourceBooleanField _new = new DataSourceBooleanField("new");
		DataSourceTextField documentId = new DataSourceTextField("docId");
		DataSourceTextField folderId = new DataSourceTextField("folderId");
		DataSourceTextField userId = new DataSourceTextField("userId");
		DataSourceTextField path = new DataSourceTextField("path");
		DataSourceTextField sid = new DataSourceTextField("sid");

		setFields(user, filename, date, event, comment, version, icon, _new, documentId, folderId, userId, path, sid);
		setClientOnly(true);

		setDataURL(url);
	}
}