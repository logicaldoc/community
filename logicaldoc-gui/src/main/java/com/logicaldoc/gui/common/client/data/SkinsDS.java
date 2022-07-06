package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource retrieve the available Skins
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.1
 */
public class SkinsDS extends DataSource {
	public SkinsDS() {
		setTitleField("label");
		setRecordXPath("/list/skin");
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField label = new DataSourceTextField("label");
		name.setPrimaryKey(true);
		name.setHidden(true);
		name.setRequired(true);

		setFields(name, label);
		setClientOnly(true);
		setDataURL("data/skins.xml");
	}
}