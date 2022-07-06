package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.Session;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle jobs grid lists. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 8.7.4
 */
public class JobsDS extends DataSource {

	public JobsDS() {
		init("data/jobs.xml?groupsonly=true");
	}

	public JobsDS(Integer max, String group) {
		init("data/jobs.xml?tenantId=" + Session.get().getTenantId() + "&max=" + (max != null ? max : 100)
				+ (group != null ? "&group=" + group : ""));
	}

	private void init(String url) {
		setRecordXPath("/list/job");
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField group = new DataSourceTextField("group");
		DataSourceTextField trigger = new DataSourceTextField("trigger");
		DataSourceTextField tenantId = new DataSourceTextField("tenantId");
		DataSourceTextField tenant = new DataSourceTextField("tenant");
		DataSourceTextField description = new DataSourceTextField("description");

		DataSourceDateTimeField previousFire = new DataSourceDateTimeField("previousFire");
		DataSourceDateTimeField nextFire = new DataSourceDateTimeField("nextFire");

		setFields(name, group, trigger, tenant, tenantId, description, previousFire, nextFire);
		setClientOnly(true);

		setDataURL(url);
	}
}