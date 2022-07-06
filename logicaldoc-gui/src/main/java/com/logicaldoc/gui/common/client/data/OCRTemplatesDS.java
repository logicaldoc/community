package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve all OCR templates. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class OCRTemplatesDS extends DataSource {

	/**
	 * The constructor
	 * 
	 * @param withEmpty if the result must contain an empty element also
	 * @param templateId optional identifier of the document template
	 */
	public OCRTemplatesDS(boolean withEmpty, Long templateId) {
		setTitleField("template");
		setRecordXPath("/list/template");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField description = new DataSourceTextField("description");
		setFields(id, name, description);
		setDataURL("data/ocrtemplates.xml?withempty=" + withEmpty
				+ (templateId != null ? "&templateId=" + templateId : ""));
		setClientOnly(true);
	}
}