package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle folders and sub-folders in Zoho. It is based on Xml
 * parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.2
 */
public class ZohoDS extends DataSource {

	public ZohoDS(String id, boolean folders) {
		String dsId = id;
		if (dsId == null)
			dsId = "ZohoDS";
		setID(dsId);
		setTitleField("name");
		setRecordXPath("/list/entry");

		DataSourceTextField name = new DataSourceTextField("name", I18N.message("name"), 255);

		DataSourceTextField entryId = new DataSourceTextField("id", I18N.message("id"));
		entryId.setPrimaryKey(true);
		entryId.setRequired(true);

		DataSourceTextField type = new DataSourceTextField("type", I18N.message("type"));
		type.setRequired(true);

		DataSourceTextField icon = new DataSourceTextField("iicon", I18N.message("icon"));

		DataSourceTextField parent = new DataSourceTextField("parent", "Parent");
		parent.setRequired(true);
		parent.setForeignKey(dsId + ".id");
		parent.setRootValue("#parent#");

		setFields(name, entryId, type, parent, icon);

		setDataURL("data/zoho.xml?folders=" + folders);
		setClientOnly(false);
	}
}