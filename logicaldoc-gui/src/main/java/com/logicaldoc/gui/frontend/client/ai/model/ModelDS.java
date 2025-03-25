package com.logicaldoc.gui.frontend.client.ai.model;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle type of samplers drop-down lists. It is based on Xml
 * parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class ModelDS extends DataSource {

	public ModelDS() {
		init("data/ai.xml?object=model");
	}

	private void init(String url) {
		setRecordXPath("/list/model");

		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField label = new DataSourceTextField("label");
		DataSourceTextField description = new DataSourceTextField("description");
		DataSourceTextField typeField = new DataSourceTextField("type");
		DataSourceDateTimeField trained = new DataSourceDateTimeField("trained");

		setFields(id, name, label, trained, description, typeField);
		setClientOnly(true);

		setDataURL("data/ai.xml?object=model");
	}
}