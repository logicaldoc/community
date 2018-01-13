package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to show cluster channels. It is based on Xml parsing
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.5
 */
public class ChannelsDS extends DataSource {

	public ChannelsDS() {
		setTitleField("name");
		setRecordXPath("/list/channel");
		DataSourceTextField name = new DataSourceTextField("name", I18N.message("name"), 255);
		name.setPrimaryKey(true);
		name.setRequired(true);

		DataSourceTextField members = new DataSourceTextField("members", I18N.message("members"));

		setFields(name, members);

		setDataURL("data/channels.xml");
		setClientOnly(false);
	}
}