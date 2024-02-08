package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle the ACL of an automation routine. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class AutomationRoutineAclDS extends DataSource {
	public AutomationRoutineAclDS(long routineId) {
		setTitleField("entity");
		setRecordXPath("/list/ace");
		DataSourceTextField entity = new DataSourceTextField("entity");
		DataSourceTextField entityId = new DataSourceTextField("entityId");
		entityId.setPrimaryKey(true);
		DataSourceBooleanField read = new DataSourceBooleanField("read");
		DataSourceBooleanField write = new DataSourceBooleanField("write");

		setFields(entityId, entity, read, write);
		setClientOnly(true);
		setDataURL("data/automationroutineacl.xml?routineId=" + routineId + "&locale=" + I18N.getLocale());
	}
}