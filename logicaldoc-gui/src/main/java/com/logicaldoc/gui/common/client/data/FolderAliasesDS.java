package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource retrieve the aliases of a folder
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class FolderAliasesDS extends DataSource {
	public FolderAliasesDS(long folderId) {
		setTitleField("name");
		setRecordXPath("/list/alias");
		DataSourceImageField icon = new DataSourceImageField("icon");
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField path = new DataSourceTextField("path");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		setFields(id, icon, name, path);
		setClientOnly(true);
		setDataURL("data/folderaliases.xml?folderId=" + folderId);
	}
}