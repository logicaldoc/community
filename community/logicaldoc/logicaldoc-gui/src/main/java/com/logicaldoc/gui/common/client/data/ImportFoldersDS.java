package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve all import folders. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ImportFoldersDS extends DataSource {

	public ImportFoldersDS(boolean withEmpty) {
		setTitleField("src");
		setRecordXPath("/list/folder");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		DataSourceTextField src = new DataSourceTextField("src");
		DataSourceTextField type = new DataSourceTextField("type");
		DataSourceTextField provider = new DataSourceTextField("provider");
		DataSourceImageField enabled = new DataSourceImageField("eenabled");

		setFields(id, src, type, provider, enabled);
		setDataURL("data/importfolders.xml?locale=" + I18N.getLocale());
		setClientOnly(true);
	}
}