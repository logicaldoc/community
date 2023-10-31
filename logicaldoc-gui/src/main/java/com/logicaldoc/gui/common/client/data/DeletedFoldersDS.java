package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle deleted folders grid lists.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 */
public class DeletedFoldersDS extends DataSource {

	public DeletedFoldersDS(Long userId, Long parentId, Integer max) {
		setTitleField("name");
		setRecordXPath("/list/folder");

		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);
		DataSourceDateTimeField lastModified = new DataSourceDateTimeField("lastModified");
		DataSourceTextField parentIdField = new DataSourceTextField("parentId");
		DataSourceImageField deleteUserId = new DataSourceImageField("deleteUserId");
		DataSourceTextField deleteUser = new DataSourceTextField("deleteUser");
		DataSourceTextField type = new DataSourceTextField("type");
		DataSourceTextField color = new DataSourceTextField("color");
		
		setFields(id, name, lastModified, parentIdField, deleteUser, deleteUserId, type, color);
		setClientOnly(true);

		String url = "data/deletedfolders.xml?1=1";
		if (userId != null)
			url += "&userId=" + userId;
		if (parentId != null)
			url += "&parentId=" + parentId;
		if (max != null)
			url += "&max=" + max;
		setDataURL(url);
	}
}