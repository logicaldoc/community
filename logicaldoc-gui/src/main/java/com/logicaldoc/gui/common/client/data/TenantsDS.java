package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve all tenants. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.9
 */
public class TenantsDS extends DataSource {
	public TenantsDS() {
		this(false);
	}

	public TenantsDS(boolean appendSystemTenant) {
		setTitleField("label");
		setRecordXPath("/list/tenant");

		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);

		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField displayName = new DataSourceTextField("displayName");
		DataSourceBooleanField enabled = new DataSourceBooleanField("eenabled");
		DataSourceTextField email = new DataSourceTextField("email");
		DataSourceTextField city = new DataSourceTextField("city");
		DataSourceTextField country = new DataSourceTextField("country");
		DataSourceTextField telephone = new DataSourceTextField("telephone");
		DataSourceTextField postalCode = new DataSourceTextField("postalCode");
		DataSourceTextField state = new DataSourceTextField("state");
		DataSourceTextField address = new DataSourceTextField("address");
		DataSourceDateTimeField expire = new DataSourceDateTimeField("expire");

		setFields(id, name, enabled, expire, displayName, email, city, country, telephone, postalCode, state, address);
		setDataURL("data/tenants.xml" + (appendSystemTenant ? "?appendSystemTenant=true" : ""));
		setClientOnly(true);
	}
}