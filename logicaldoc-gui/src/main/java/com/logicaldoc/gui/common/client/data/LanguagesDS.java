package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.Session;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve the installed languages. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class LanguagesDS extends DataSource {
	public LanguagesDS(boolean gui) {
		setTitleField("label");
		setRecordXPath("/list/lang");

		DataSourceTextField code = new DataSourceTextField("code");
		code.setPrimaryKey(true);

		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceImageField enabled = new DataSourceImageField("eenabled");

		setFields(code, name, enabled);
		setDataURL("data/languages.xml?locale=" + Session.get().getUser().getLanguage() + "&gui="
				+ Boolean.toString(gui));
		setClientOnly(true);
	}
}