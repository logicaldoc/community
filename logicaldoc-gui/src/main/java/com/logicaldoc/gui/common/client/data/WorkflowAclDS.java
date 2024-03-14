package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to get the ACL of a workflow. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class WorkflowAclDS extends DataSource {
	public WorkflowAclDS(long id) {
		setTitleField("entity");
		setRecordXPath("/list/ace");
		DataSourceTextField entity = new DataSourceTextField("entity");
		DataSourceTextField entityId = new DataSourceTextField("entityId");
		entityId.setPrimaryKey(true);
		DataSourceBooleanField read = new DataSourceBooleanField("read");
		DataSourceBooleanField write = new DataSourceBooleanField("write");
		DataSourceTextField type = new DataSourceTextField("type");

		setFields(entityId, entity, read, write, type);
		setClientOnly(true);

		setDataURL("data/workflowacl.xml?workflowId=" + id + "&locale=" + I18N.getLocale());
	}
}