package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve a list of sessions. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class SessionsDS extends DataSource {
	public SessionsDS() {
		this(null, null);
	}

	public SessionsDS(String node) {
		this(null, node);
	}

	public SessionsDS(Integer status, String node) {
		setTitleField("sid");
		setRecordXPath("/list/session");

		DataSourceTextField sid = new DataSourceTextField("sid");
		sid.setPrimaryKey(true);

		DataSourceTextField stat = new DataSourceTextField("status");
		DataSourceTextField statusLabel = new DataSourceTextField("statusLabel");
		DataSourceTextField username = new DataSourceTextField("username");
		DataSourceTextField tenant = new DataSourceTextField("tenant");
		DataSourceDateField created = new DataSourceDateField("created");
		DataSourceDateField finished = new DataSourceDateField("finished");
		DataSourceTextField duration = new DataSourceTextField("duration");
		DataSourceDateField renew = new DataSourceDateField("renew");
		DataSourceTextField client = new DataSourceTextField("client");
		DataSourceTextField nodeField = new DataSourceTextField("node");

		setFields(sid, stat, statusLabel, username, tenant, created, renew, finished, duration, client, nodeField);
		String url = "data/sessions.xml?locale=" + I18N.getLocale() + (status != null ? "&status=" + status : "")
				+ (node != null ? "&node=" + node : "");
		setDataURL(url);
		setClientOnly(true);
	}
}