package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle imcremental export archives grid lists. It is based on
 * Xml parsing
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class IncrementalArchivesDS extends DataSource {
	public IncrementalArchivesDS(int archivesType) {
		setTitleField("name");
		setRecordXPath("/list/archive");

		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField prefix = new DataSourceTextField("prefix");
		DataSourceTextField type = new DataSourceTextField("type");
		DataSourceTextField typelabel = new DataSourceTextField("typelabel");
		DataSourceIntegerField frequency = new DataSourceIntegerField("frequency");

		setFields(id, prefix, type, typelabel, frequency);
		setClientOnly(true);
		setDataURL("data/incrementalarchives.xml?locale=" + I18N.getLocale() + "&type=" + archivesType);
	}
}