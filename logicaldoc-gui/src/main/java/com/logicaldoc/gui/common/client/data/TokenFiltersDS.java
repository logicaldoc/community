package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve the token filters.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.3
 */
public class TokenFiltersDS extends DataSource {
	public TokenFiltersDS(String filter) {
		setTitleField("label");
		setRecordXPath("/list/filter");

		DataSourceTextField name = new DataSourceTextField("name");
		name.setPrimaryKey(true);

		DataSourceImageField enabled = new DataSourceImageField("eenabled");

		DataSourceTextField value = new DataSourceTextField("value");

		setFields(name, enabled, value);
		setDataURL("data/tokenfilters.xml" + (filter != null ? "?filter=" + filter : ""));
		setClientOnly(true);
	}
}