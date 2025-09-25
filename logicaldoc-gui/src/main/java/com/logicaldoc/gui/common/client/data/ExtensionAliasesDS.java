package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve the aliases for the input extensions.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class ExtensionAliasesDS extends DataSource {

	public ExtensionAliasesDS() {
		setTitleField("name");
		setRecordXPath("/list/alias");

		DataSourceTextField extension = new DataSourceTextField("extension");
		extension.setPrimaryKey(true);
		extension.setRequired(true);
		
		DataSourceTextField aliases = new DataSourceTextField("aliases");

		setFields(extension, aliases);
		setClientOnly(true);
		setDataURL("data/extensionaliases.xml");
	}
}