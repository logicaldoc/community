package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to show the aspects and their status in the different runlevels.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class AspectsDS extends DataSource {
	public AspectsDS() {
		setTitleField("entity");
		setRecordXPath("/list/aspect");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		
		DataSourceBooleanField _default = new DataSourceBooleanField("default");
		DataSourceBooleanField bulkload = new DataSourceBooleanField("bulkload");
		DataSourceBooleanField devel = new DataSourceBooleanField("devel");
		DataSourceBooleanField demo = new DataSourceBooleanField("demo");

		setFields(id, _default, bulkload, devel, demo);
		setClientOnly(true);
		setDataURL("data/aspects.xml");
	}
}