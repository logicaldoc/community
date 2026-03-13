package com.logicaldoc.gui.frontend.client.ai.autofill;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * A datasource to retrieve all the filler types
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.3
 */
public class FillerTypesDS extends DataSource {

	public FillerTypesDS() {
		this(null);
	}

	public FillerTypesDS(String type) {
		setRecordXPath("/list/type");

		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField label = new DataSourceTextField("label");
		
		setFields(id, label);
		setClientOnly(true);

		String url = "data/fillers.xml?type=all";

		setDataURL(url);
	}
}