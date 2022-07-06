package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve the bookmarks of the current user.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class BookmarksDS extends DataSource {
	public BookmarksDS() {
		setTitleField("name");
		setRecordXPath("/list/bookmark");
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);
		DataSourceImageField icon = new DataSourceImageField("icon");
		DataSourceTextField description = new DataSourceTextField("description");
		DataSourceIntegerField position = new DataSourceIntegerField("position");
		DataSourceTextField userId = new DataSourceTextField("userId");
		DataSourceTextField targetId = new DataSourceTextField("targetId");
		DataSourceTextField folderId = new DataSourceTextField("folderId");
		DataSourceIntegerField type = new DataSourceIntegerField("type");
		type.setHidden(true);

		setFields(id, name, description, icon, userId, targetId, position, folderId, type);
		setClientOnly(true);
		setDataURL("data/bookmarks.xml");
	}
}