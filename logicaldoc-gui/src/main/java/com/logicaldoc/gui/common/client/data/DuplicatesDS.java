package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceFloatField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle duplicates documents grid lists. It is based on Xml
 * parsing.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class DuplicatesDS extends DataSource {

	/**
	 * Constructor
	 * 
	 * @param folderId identifier of the folder
	 * @param max maximum number of entries
	 */
	public DuplicatesDS(Long folderId, int max) {
		setTitleField("filename");
		setRecordXPath("/list/duplicate");

		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);
		DataSourceImageField icon = new DataSourceImageField("icon");
		DataSourceTextField customId = new DataSourceTextField("customId");
		DataSourceTextField version = new DataSourceTextField("version");
		DataSourceTextField publisher = new DataSourceTextField("publisher");
		DataSourceFloatField size = new DataSourceFloatField("size");
		DataSourceDateTimeField lastModified = new DataSourceDateTimeField("lastModified");
		DataSourceTextField filename = new DataSourceTextField("filename");
		DataSourceTextField digest = new DataSourceTextField("digest");
		DataSourceImageField immutable = new DataSourceImageField("immutable");
		DataSourceTextField foldId = new DataSourceTextField("folderId");
		DataSourceTextField type = new DataSourceTextField("type");
		DataSourceImageField locked = new DataSourceImageField("locked");
		DataSourceTextField folderName = new DataSourceTextField("foldername");

		setFields(id, size, publisher, version, lastModified, customId, icon, filename, digest, immutable, foldId,
				folderName, type, locked);
		setClientOnly(true);
		setDataURL("data/duplicates.xml?max=" + max + (folderId != null ? "&folderId=" + folderId : ""));
	}
}