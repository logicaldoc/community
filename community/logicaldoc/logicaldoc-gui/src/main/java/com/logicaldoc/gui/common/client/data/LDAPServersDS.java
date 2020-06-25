package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve all LDAP servers. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.2
 */
public class LDAPServersDS extends DataSource {

	public LDAPServersDS() {
		setTitleField("src");
		setRecordXPath("/list/server");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		DataSourceTextField src = new DataSourceTextField("url");
		DataSourceImageField enabled = new DataSourceImageField("eenabled");

		setFields(id, src, enabled);
		setDataURL("data/ldapservers.xml");
		setClientOnly(true);
	}
}