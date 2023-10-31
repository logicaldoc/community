package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceFloatField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle deleted documents grid lists.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.2.1
 */
public class DeletedDocsDS extends DataSource {

	public DeletedDocsDS(Long userId, Long folderId, Integer max) {
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
		DataSourceIntegerField immutable = new DataSourceIntegerField("immutable");
		DataSourceTextField fid = new DataSourceTextField("folderId");
		DataSourceTextField type = new DataSourceTextField("type");
		DataSourceImageField locked = new DataSourceImageField("locked");
		DataSourceImageField deleteUserId = new DataSourceImageField("deleteUserId");
		DataSourceTextField deleteUser = new DataSourceTextField("deleteUser");
		DataSourceTextField avatar = new DataSourceTextField("avatar", I18N.message("avatar"), 24);

		setFields(id, userIdItem, username, size, version, fileVersion, lastModified, customId, icon, filename, digest,
				immutable, fid, deleteUserId, avatar, deleteUser, type, locked);
		setClientOnly(true);

		String url = "data/deleteddocs.xml?1=1";
		if (userId != null)
			url += "&userId=" + userId;
		if (folderId != null)
			url += "&folderId=" + folderId;
		if (max != null)
			url += "&max=" + max;
		setDataURL(url);
	}
}