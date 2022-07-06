package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Data Source to handle dashelts grid lists. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
public class DashletsDS extends DataSource {
	public DashletsDS() {
		setTitleField("title");
		setRecordXPath("/list/dashlet");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField title = new DataSourceTextField("title");
		DataSourceTextField type = new DataSourceTextField("type");
		DataSourceTextField content = new DataSourceTextField("content");
		DataSourceTextField query = new DataSourceTextField("query");

		setFields(id, name, title, type, content, query);
		setClientOnly(true);
		setDataURL("data/dashlets.xml?locale="+I18N.getLocale());
	}
}