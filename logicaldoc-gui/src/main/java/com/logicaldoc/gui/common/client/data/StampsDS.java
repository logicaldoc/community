package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Data source to retrieve all stamps. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class StampsDS extends DataSource {

	public StampsDS(Long userId, boolean enabledOnly) {
		setTitleField("name");
		setRecordXPath("/list/stamp");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceBooleanField enabled = new DataSourceBooleanField("eenabled");
		DataSourceTextField description = new DataSourceTextField("description");
		DataSourceTextField text = new DataSourceTextField("text");
		DataSourceImageField image = new DataSourceImageField("image");

		setFields(id, name, description, text, image, enabled);
		setDataURL("data/stamps.xml?enabledOnly=" + enabledOnly + (userId != null ? "&userId=" + userId : ""));
		setClientOnly(true);
	}
}