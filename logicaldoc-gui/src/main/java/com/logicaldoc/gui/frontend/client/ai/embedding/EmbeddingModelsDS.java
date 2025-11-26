package com.logicaldoc.gui.frontend.client.ai.embedding;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle grids of embedding models. It is based on Xml parsing
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.2
 *
 */
public class EmbeddingModelsDS extends DataSource {
	public EmbeddingModelsDS() {
		setRecordXPath("/list/model");

		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField label = new DataSourceTextField("label");

		setFields(id, name, label);
		setClientOnly(true);

		setDataURL("data/ai.xml?object=embeddingmodel");
	}
}