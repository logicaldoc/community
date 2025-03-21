package com.logicaldoc.gui.frontend.client.ai.model;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle type of models drop-down lists. It is based on Xml
 * parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class ModelTypeDS extends DataSource {

	public ModelTypeDS() {
		init("data/ai.xml?object=modeltype");
	}

	private void init(String url) {
		setRecordXPath("/list/type");

		DataSourceTextField value = new DataSourceTextField("value");
		value.setPrimaryKey(true);
		value.setRequired(true);

		setFields(value);
		setClientOnly(true);

		setDataURL(url);
	}
}