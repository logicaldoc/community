package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceFloatField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle deleted documents grid lists.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 7.2.1
 */
public class DeletedDocsDS extends DataSource {

	public DeletedDocsDS(Long userId, Long folderId) {
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
		DataSourceTextField filename = new DataSourceTextField("filename");
		DataSourceTextField digest = new DataSourceTextField("digest");
		DataSourceImageField immutable = new DataSourceImageField("immutable");
		DataSourceTextField fid = new DataSourceTextField("folderId");
		DataSourceTextField type = new DataSourceTextField("type");
		DataSourceImageField locked = new DataSourceImageField("locked");
		DataSourceImageField deletedUserId = new DataSourceImageField("deletedUserId");

		setFields(id, userIdItem, username, size, version, fileVersion, lastModified, customId, icon, filename,
				digest, immutable, fid, deletedUserId, type, locked);
		setClientOnly(true);

		String url = "data/deleteddocs.xml?1=1";
		if (userId != null)
			url += "&userId=" + userId;
		if (folderId != null)
			url += "&folderId=" + folderId;
		setDataURL(url);
	}
}