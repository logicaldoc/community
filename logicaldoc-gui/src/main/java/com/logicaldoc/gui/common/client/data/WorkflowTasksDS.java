package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateField;
import com.smartgwt.client.data.fields.DataSourceTextField;

public class WorkflowTasksDS extends DataSource {
	public WorkflowTasksDS(Integer type, String taskId, Integer max) {
		setTitleField("name");
		setRecordXPath("/list/workflowtask");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setRequired(true);
		DataSourceTextField processId = new DataSourceTextField("processId");
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField workflow = new DataSourceTextField("workflow");
		DataSourceTextField workflowDisplay = new DataSourceTextField("workflowDisplay");
		DataSourceTextField pooledassignees = new DataSourceTextField("pooledassignees");
		DataSourceTextField documents = new DataSourceTextField("documents");
		DataSourceTextField documentIds = new DataSourceTextField("documentIds");
		DataSourceDateField startdate = new DataSourceDateField("startdate");
		DataSourceDateField duedate = new DataSourceDateField("duedate");
		DataSourceDateField enddate = new DataSourceDateField("enddate");
		DataSourceTextField lastnote = new DataSourceTextField("lastnote");
		DataSourceTextField templateVersion = new DataSourceTextField("templateVersion");
		DataSourceTextField display = new DataSourceTextField("display");
		display.setHidden(true);

		setFields(id, processId, name, workflowDisplay, startdate, duedate, enddate, workflow, documents, lastnote, documentIds,
				pooledassignees, templateVersion, display);
		setDataURL("data/workflowtasks.xml?1=1" + (type != null ? "&type=" + type : "")
				+ (taskId != null ? "&taskId=" + taskId : "") + (max != null ? ("&max=" + max) : ""));
		setClientOnly(true);
	}
}
