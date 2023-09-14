package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to display the reading requests
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
public class ReadingRequestsDS extends DataSource {

	private static final String MAX = "&max=";

	private static Integer getDefaultMaxHistories() {
		try {
			return Session.get().getConfigAsInt("gui.maxhistories");
		} catch (Exception t) {
			return 100;
		}
	}

	public ReadingRequestsDS(boolean received, Integer max) {
		init("data/readingrequests.xml?received=" + received + "&locale=" + I18N.getLocale() + MAX
				+ (max != null ? max : getDefaultMaxHistories()));
	}

	public ReadingRequestsDS(String url) {
		init(url);
	}

	private void init(String url) {
		setRecordXPath("/list/readingrequest");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);
		
		DataSourceTextField userId = new DataSourceTextField("userId");
		DataSourceTextField user = new DataSourceTextField("user");
		DataSourceDateTimeField date = new DataSourceDateTimeField("date");
		DataSourceDateTimeField confirmed = new DataSourceDateTimeField("confirmed");
		DataSourceTextField filename = new DataSourceTextField("filename");
		DataSourceTextField fileVersion = new DataSourceTextField("fileVersion");
		DataSourceImageField icon = new DataSourceImageField("icon");
		icon.setHidden(true);
		DataSourceTextField documentId = new DataSourceTextField("docId");
		DataSourceTextField folderId = new DataSourceTextField("folderId");
		DataSourceTextField requestor = new DataSourceTextField("requestor");
		DataSourceTextField requestorId = new DataSourceTextField("requestorId");
		DataSourceTextField message = new DataSourceTextField("message");

		setFields(id, user, filename, date, fileVersion, icon, documentId, folderId, userId, requestor,
				requestorId, message, confirmed);
		setClientOnly(true);

		setDataURL(url);
	}
}