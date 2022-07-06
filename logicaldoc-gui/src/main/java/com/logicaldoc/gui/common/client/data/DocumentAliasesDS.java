package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource retrieve the aliases of a document
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class DocumentAliasesDS extends DataSource {
	public DocumentAliasesDS(long docId) {
		setTitleField("filename");
		setRecordXPath("/list/alias");
		DataSourceImageField icon = new DataSourceImageField("icon");
		DataSourceTextField filename = new DataSourceTextField("filename");
		DataSourceTextField path = new DataSourceTextField("path");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField folderId = new DataSourceTextField("folderId");
		folderId.setHidden(true);
		
		setFields(id, icon, filename, path, folderId);
		setClientOnly(true);
		setDataURL("data/documentaliases.xml?docId=" + docId);
	}
}