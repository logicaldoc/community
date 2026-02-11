package com.logicaldoc.gui.frontend.client.ai.filler;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

public class FillersDS extends DataSource {

	public FillersDS(String type) {
		setRecordXPath("/list/filler");

		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField label = new DataSourceTextField("label");
		DataSourceTextField description = new DataSourceTextField("description");
		DataSourceTextField typeField = new DataSourceTextField("type");

		setFields(id, name, label, description, typeField);
		setClientOnly(true);

		String url = "data/ai.xml?object=sampler";
		if (type != null)
			url += "&type=" + type;

		setDataURL(url);
	}
}
