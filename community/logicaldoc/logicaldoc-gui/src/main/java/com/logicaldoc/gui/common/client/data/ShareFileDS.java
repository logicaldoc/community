package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle folders and sub-folders in ShareFile. It is based on Xml
 * parsing
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 7.2.1
 */
public class ShareFileDS extends DataSource {

	public ShareFileDS(boolean folders) {
		setTitleField("name");
		setRecordXPath("/list/entry");

		DataSourceTextField name = new DataSourceTextField("name", I18N.message("name"), 255);

		DataSourceTextField id = new DataSourceTextField("iid", I18N.message("id"));
		id.setPrimaryKey(true);
		id.setRequired(true);

		DataSourceTextField type = new DataSourceTextField("type", I18N.message("type"));
		type.setRequired(true);

		DataSourceTextField icon = new DataSourceTextField("iicon", I18N.message("icon"));

		DataSourceTextField parent = new DataSourceTextField("parent", I18N.message("parent"));
		parent.setRequired(true);
		parent.setForeignKey(getID() + ".iid");
		parent.setRootValue("#parent#");

		setFields(name, id, type, parent, icon);

		setDataURL("data/sharefile.xml?folders=" + folders);
		setClientOnly(false);
	}
}