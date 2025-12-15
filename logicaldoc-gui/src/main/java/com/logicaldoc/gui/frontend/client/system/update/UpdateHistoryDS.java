package com.logicaldoc.gui.frontend.client.system.update;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Data Source to display installed updates and patches
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.2
 */
public class UpdateHistoryDS extends DataSource {

	public UpdateHistoryDS() {
		setTitleField("name");
		setRecordXPath("/list/update");
		setDataURL("data/updatehistory.xml");
		setClientOnly(true);

		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceDateTimeField date = new DataSourceDateTimeField("date");
		DataSourceTextField type = new DataSourceTextField("type");
		DataSourceIntegerField rating = new DataSourceIntegerField("rating");
		setFields(name, date, type, rating);
	}
}