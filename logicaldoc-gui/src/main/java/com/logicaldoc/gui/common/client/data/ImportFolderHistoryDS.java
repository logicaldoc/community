package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle the grids of import folder histories. It is based on Xml
 * parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.1
 */
public class ImportFolderHistoryDS extends DataSource {
	
	private static Integer getDefaultMaxHistories() {
		try {
			return Session.get().getConfigAsInt("gui.maxhistories");
		} catch (Throwable t) {
			return 100;
		}
	}

	public ImportFolderHistoryDS(long importFolderId, Integer max) {
		String url = "data/importfolderhistories.xml?locale=" + I18N.getLocale() + "&importFolderId=" + importFolderId
				+ "&max=" + (max != null ? max : getDefaultMaxHistories());

		setRecordXPath("/list/history");
		DataSourceDateTimeField date = new DataSourceDateTimeField("date");
		DataSourceTextField event = new DataSourceTextField("event");
		DataSourceTextField comment = new DataSourceTextField("comment");
		DataSourceTextField filename = new DataSourceTextField("filename");
		DataSourceImageField icon = new DataSourceImageField("icon");
		icon.setHidden(true);
		DataSourceBooleanField _new = new DataSourceBooleanField("new");
		DataSourceTextField documentId = new DataSourceTextField("docId");
		DataSourceTextField folderId = new DataSourceTextField("folderId");
		DataSourceTextField path = new DataSourceTextField("path");
		DataSourceTextField source = new DataSourceTextField("source");

		setFields(filename, date, event, comment, icon, _new, documentId, folderId, path, source);
		setClientOnly(true);

		setDataURL(url);
	}
}