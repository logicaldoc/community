package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceFloatField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle locked documents grid lists.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1.2
 */
public class LockedDocsDS extends DataSource {

	public LockedDocsDS(Long userId, Integer max) {
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
		DataSourceIntegerField immutable = new DataSourceIntegerField("immutable");
		DataSourceTextField folderId = new DataSourceTextField("folderId");
		DataSourceTextField type = new DataSourceTextField("type");
		DataSourceIntegerField status = new DataSourceIntegerField("status");

		setFields(id, userIdItem, username, size, version, fileVersion, lastModified, customId, icon, fileName, digest,
				immutable, folderId, type, status);
		setClientOnly(true);

		String url = "data/lockeddocs.xml?1=1";
		if (userId != null)
			url += "&userId=" + userId;
		if (max != null)
			url += "&max=" + max;
		setDataURL(url);
	}
}