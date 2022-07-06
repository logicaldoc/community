package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle the garbage of the current user.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GarbageDS extends DataSource {
	public GarbageDS() {
		setTitleField("filename");
		setRecordXPath("/list/entry");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);
		DataSourceImageField icon = new DataSourceImageField("icon");
		DataSourceTextField customId = new DataSourceTextField("customId");
		DataSourceDateTimeField lastModified = new DataSourceDateTimeField("lastModified");
		DataSourceTextField folderId = new DataSourceTextField("folderId");
		DataSourceTextField type = new DataSourceTextField("type");
		DataSourceIntegerField folderType = new DataSourceIntegerField("folderType");
		DataSourceTextField fileName = new DataSourceTextField("filename");
		DataSourceTextField color = new DataSourceTextField("color");

		setFields(id, fileName, customId, icon, lastModified, folderId, type, folderType, color);
		setClientOnly(true);
		setDataURL("data/garbage.xml");
	}
}