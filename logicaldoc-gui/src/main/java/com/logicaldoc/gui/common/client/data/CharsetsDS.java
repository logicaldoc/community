package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve the installed languages. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.3
 */
public class CharsetsDS extends DataSource {
	public CharsetsDS() {
		setTitleField("name");
		setRecordXPath("/list/charset");

		DataSourceTextField code = new DataSourceTextField("code");
		code.setPrimaryKey(true);
		code.setRequired(true);

		DataSourceTextField name = new DataSourceTextField("name");

		setFields(code, name);
		setDataURL("data/charsets.xml?locale=" + I18N.getLocale());
		setClientOnly(true);
	}
}