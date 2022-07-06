package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceFloatField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle achived documents grid lists.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.2
 */
public class ArchivedDocsDS extends DataSource {

	public ArchivedDocsDS(Long folderId, Integer max) {
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
		DataSourceFloatField size = new DataSourceFloatField("size");
		DataSourceDateTimeField lastModified = new DataSourceDateTimeField("lastModified");
		DataSourceDateTimeField created = new DataSourceDateTimeField("created");
		DataSourceTextField filename = new DataSourceTextField("filename");
		DataSourceTextField fid = new DataSourceTextField("folderId");
		DataSourceTextField folder = new DataSourceTextField("folder");
		DataSourceTextField type = new DataSourceTextField("type");

		setFields(id, size, version, fileVersion, created, lastModified, customId, icon, filename, fid, folder, type);
		setClientOnly(true);

		String url = "data/archiveddocs.xml?1=1";
		if (folderId != null)
			url += "&folderId=" + folderId;
		if (max != null)
			url += "&max=" + max;
		setDataURL(url);
	}
}