package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle workflow trigger lists.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class WorkflowTriggersDS extends DataSource {
	private static final String WORKFLOW = "workflow";

	public WorkflowTriggersDS(String folderId) {
		setTitleField(WORKFLOW);
		setRecordXPath("/list/workflowtrigger");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);
		DataSourceTextField workflowId = new DataSourceTextField("workflowId");
		workflowId.setHidden(true);
		DataSourceTextField templateId = new DataSourceTextField("templateId");
		templateId.setHidden(true);
		DataSourceTextField workflow = new DataSourceTextField(WORKFLOW, I18N.message(WORKFLOW));
		DataSourceTextField template = new DataSourceTextField("template", I18N.message("template"));
		DataSourceTextField events = new DataSourceTextField("events", I18N.message("triggeron"));
		setFields(id, workflowId, templateId, workflow, template, events);
		setDataURL("data/workflowtriggers.xml?folderId=" + folderId);
		setClientOnly(false);
	}
}