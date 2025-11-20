package com.logicaldoc.gui.frontend.client.ai.embedding;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle grids of embedding schemes. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.2
 */
public class EmbeddingSchemesDS extends DataSource {

	public EmbeddingSchemesDS() {
		this(null);
	}

	public EmbeddingSchemesDS(String type) {
		setRecordXPath("/list/scheme");

		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField label = new DataSourceTextField("label");
		DataSourceTextField typeField = new DataSourceTextField("type");
		DataSourceDateTimeField enabled = new DataSourceDateTimeField("eenabled");
		DataSourceTextField model = new DataSourceTextField("model");
		DataSourceTextField modelSpec = new DataSourceTextField("modelspec");
		DataSourceTextField modelId = new DataSourceTextField("modelid");

		setFields(id, name, label, enabled, typeField, model, modelSpec, modelId);
		setClientOnly(true);

		setDataURL("data/ai.xml?object=embeddingscheme" + (type != null ? "&type=" + type : ""));
	}
}