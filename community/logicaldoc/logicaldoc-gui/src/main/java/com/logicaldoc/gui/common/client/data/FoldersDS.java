package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceFloatField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle folders and sub-folders structures. It is based on Xml
 * parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class FoldersDS extends DataSource {

	private static FoldersDS instance;

	public static FoldersDS get() {
		if (instance == null)
			instance = new FoldersDS(null);
		return instance;
	}

	public FoldersDS(String id) {
		String dsId = id;
		if (dsId == null)
			dsId = "FoldersDS";
		setID(dsId);
		setTitleField("name");
		setRecordXPath("/list/folder");
		DataSourceTextField name = new DataSourceTextField("name", I18N.message("name"), 255);

		DataSourceTextField color = new DataSourceTextField("color", I18N.message("color"), 255);

		DataSourceTextField recordId = new DataSourceTextField("id", I18N.message("id"));
		recordId.setPrimaryKey(true);
		recordId.setRequired(true);

		DataSourceTextField folderId = new DataSourceTextField("folderId", I18N.message("id"));
		folderId.setHidden(true);

		DataSourceTextField foldRef = new DataSourceTextField("foldRef");
		foldRef.setHidden(true);

		DataSourceIntegerField type = new DataSourceIntegerField("type", I18N.message("type"));
		type.setRequired(true);

		DataSourceFloatField size = new DataSourceFloatField("size", I18N.message("size"));

		DataSourceTextField parent = new DataSourceTextField("parent", "Parent ID");
		parent.setRequired(true);
		parent.setForeignKey(dsId + ".id");
		parent.setRootValue("/");

		setFields(recordId, name, folderId, foldRef, parent, type, size, color);

		setDataURL("data/folders.xml?");
		setClientOnly(false);
	}
}