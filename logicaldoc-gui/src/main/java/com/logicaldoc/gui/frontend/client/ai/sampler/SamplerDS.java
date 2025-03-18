package com.logicaldoc.gui.frontend.client.ai.sampler;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle samplers grid lists. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class SamplerDS extends DataSource {

	public SamplerDS(String type) {
		setRecordXPath("/list/sampler");

		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField label = new DataSourceTextField("label");
		DataSourceTextField typeField = new DataSourceTextField("type");

		setFields(id, name, label, typeField);
		setClientOnly(true);

		String url = "data/ai.xml?object=sampler";
		if (type != null)
			url += "&type=" + type;

		setDataURL(url);
	}
}