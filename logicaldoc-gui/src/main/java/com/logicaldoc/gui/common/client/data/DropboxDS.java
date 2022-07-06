package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle folders and sub-folders in Dropbox. It is based on Xml
 * parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.0
 */
public class DropboxDS extends DataSource {

	public DropboxDS(String id, boolean folders) {
		String dsId = id;
		if (dsId == null)
			dsId = "DropboxDS";
		setID(dsId);
		setTitleField("name");
		setRecordXPath("/list/entry");

		DataSourceTextField name = new DataSourceTextField("name", I18N.message("name"), 255);

		DataSourceTextField path = new DataSourceTextField("path", I18N.message("path"));
		path.setPrimaryKey(true);
		path.setRequired(true);

		DataSourceTextField type = new DataSourceTextField("type", I18N.message("type"));
		type.setRequired(true);

		DataSourceTextField icon = new DataSourceTextField("iicon", I18N.message("icon"));

		DataSourceTextField parent = new DataSourceTextField("parent", "Parent");
		parent.setRequired(true);
		parent.setForeignKey(dsId + ".path");
		parent.setRootValue("#parent#");

		setFields(name, path, type, parent, icon);

		setDataURL("data/dropbox.xml?folders=" + folders);
		setClientOnly(false);
	}
}