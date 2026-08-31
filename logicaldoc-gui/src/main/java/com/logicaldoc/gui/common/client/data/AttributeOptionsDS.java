package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Data source to handle the options for the extended attributes.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1
 */
public class AttributeOptionsDS extends DataSource {
	public AttributeOptionsDS(long setId, String attribute, String category, boolean withEmpty) {
		setRecordXPath("/list/option");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);
		DataSourceTextField attributeField = new DataSourceTextField("attribute");
		attributeField.setHidden(true);

		DataSourceTextField value = new DataSourceTextField("value");
		
		DataSourceTextField cat = new DataSourceTextField("category");

		DataSourceIntegerField position = new DataSourceIntegerField("position");
		position.setHidden(true);

		DataSourceTextField templateIdField = new DataSourceTextField("templateId");
		templateIdField.setHidden(true);

		setFields(id, attributeField, value, cat, position, templateIdField);
		setClientOnly(true);

		setDataURL("data/attributeoptions.xml?setId=" + setId + "&" + "attribute=" + attribute + "&withempty="
				+ withEmpty+(category!=null ? "&category="+category : ""));
	}
}