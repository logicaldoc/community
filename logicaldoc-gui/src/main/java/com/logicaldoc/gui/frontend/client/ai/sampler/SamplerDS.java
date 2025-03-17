package com.logicaldoc.gui.frontend.client.ai.sampler;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle samplers grid lists. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class SamplerDS extends DataSource {

	public SamplerDS() {
		init("data/ai.xml?object=sampler");
	}

	private void init(String url) {
		setRecordXPath("/list/sampler");

		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField label = new DataSourceTextField("label");
		DataSourceDateTimeField creation = new DataSourceDateTimeField("creation");
		DataSourceTextField type = new DataSourceTextField("type");

		setFields(id, name, label, type, creation);
		setClientOnly(true);

		setDataURL(url);
	}
}