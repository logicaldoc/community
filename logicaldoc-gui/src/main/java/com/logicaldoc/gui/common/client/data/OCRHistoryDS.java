package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle the grids of OCR histories. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.1
 */
public class OCRHistoryDS extends DataSource {

	private static Integer getDefaultMaxHistories() {
		try {
			return Session.get().getConfigAsInt("gui.maxhistories");
		} catch (Exception t) {
			return 100;
		}
	}

	public OCRHistoryDS(Integer max) {
		String url = "data/ocrhistories.xml?locale=" + I18N.getLocale() + "&max="
				+ (max != null ? max : getDefaultMaxHistories());

		setRecordXPath("/list/history");
		DataSourceDateTimeField date = new DataSourceDateTimeField("date");
		DataSourceTextField event = new DataSourceTextField("event");
		DataSourceTextField comment = new DataSourceTextField("comment");
		DataSourceTextField filename = new DataSourceTextField("filename");
		DataSourceImageField icon = new DataSourceImageField("icon");
		icon.setHidden(true);
		DataSourceTextField documentId = new DataSourceTextField("docId");
		DataSourceTextField folderId = new DataSourceTextField("folderId");
		DataSourceTextField path = new DataSourceTextField("path");

		setFields(filename, date, event, comment, icon, documentId, folderId, path);
		setClientOnly(true);

		setDataURL(url);
	}
}