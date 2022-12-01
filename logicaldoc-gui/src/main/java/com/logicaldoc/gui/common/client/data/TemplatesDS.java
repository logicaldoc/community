package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve all templates. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class TemplatesDS extends DataSource {
	public TemplatesDS(boolean withEmpty, Long templateId, Integer type) {
		setTitleField("template");
		setRecordXPath("/list/template");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField description = new DataSourceTextField("description");
		DataSourceTextField readonly = new DataSourceTextField("readonly");
		DataSourceIntegerField documents = new DataSourceIntegerField("documents");
		DataSourceIntegerField ttype = new DataSourceIntegerField("type");
		setFields(id, name, description, documents, readonly, ttype);
		setDataURL("data/templates.xml?withempty=" + withEmpty + (templateId != null ? "&templateId=" + templateId : "")
				+ (type != null ? "&type=" + type : ""));
		setClientOnly(true);
	}
}