package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceFloatField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle locked documents grid lists.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1.2
 */
public class LockedDocsDS extends DataSource {

	public LockedDocsDS(Long userId) {
		setTitleField("filename");
		setRecordXPath("/list/document");

		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);
		DataSourceImageField icon = new DataSourceImageField("icon");
		DataSourceTextField customId = new DataSourceTextField("customId");
		DataSourceTextField version = new DataSourceTextField("version");
		DataSourceTextField fileVersion = new DataSourceTextField("fileVersion");
		DataSourceTextField userIdItem = new DataSourceTextField("userId");
		DataSourceTextField username = new DataSourceTextField("username");
		DataSourceFloatField size = new DataSourceFloatField("size");
		DataSourceDateTimeField lastModified = new DataSourceDateTimeField("lastModified");
		DataSourceTextField fileName = new DataSourceTextField("filename");
		DataSourceTextField digest = new DataSourceTextField("digest");
		DataSourceImageField immutable = new DataSourceImageField("immutable");
		DataSourceTextField folderId = new DataSourceTextField("folderId");
		DataSourceTextField type = new DataSourceTextField("type");
		DataSourceImageField locked = new DataSourceImageField("locked");

		setFields(id, userIdItem, username, size, version, fileVersion, lastModified, customId, icon, fileName, digest,
				immutable, folderId, type, locked);
		setClientOnly(true);

		String url = "data/lockeddocs.xml";
		if (userId != null)
			url += "?userId=" + userId;
		setDataURL(url);
	}
}