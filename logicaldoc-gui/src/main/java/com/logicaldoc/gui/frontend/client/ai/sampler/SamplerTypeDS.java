package com.logicaldoc.gui.frontend.client.ai.sampler;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle type of samplers drop-down lists. It is based on Xml
 * parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class SamplerTypeDS extends DataSource {

	public SamplerTypeDS() {
		init("data/ai.xml?object=samplertype");
	}

	private void init(String url) {
		setRecordXPath("/list/type");

		DataSourceTextField value = new DataSourceTextField("value");
		value.setPrimaryKey(true);
		value.setRequired(true);

		DataSourceTextField label = new DataSourceTextField("label");
		
		setFields(value, label);
		setClientOnly(true);

		setDataURL(url);
	}
}