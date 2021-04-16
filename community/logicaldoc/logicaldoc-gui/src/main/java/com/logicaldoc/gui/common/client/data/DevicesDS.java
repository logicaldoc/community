package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Data Source to show the user's devices. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
public class DevicesDS extends DataSource {

	public DevicesDS() {
		setTitleField("id");
		setRecordXPath("/list/device");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);
		DataSourceTextField deviceId = new DataSourceTextField("deviceid");
		id.setHidden(true);
		DataSourceTextField browser = new DataSourceTextField("browser");
		DataSourceTextField os = new DataSourceTextField("os");
		DataSourceTextField type = new DataSourceTextField("type");
		DataSourceDateField creation = new DataSourceDateField("creation");
		DataSourceDateField lastlogin = new DataSourceDateField("lastlogin");

		setFields(id, deviceId, browser, os, type, lastlogin, creation);
		setDataURL("data/devices.xml?trustedonly=true");
		setClientOnly(true);
	}
}