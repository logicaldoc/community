package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;

public class WorkflowsDS extends DataSource {
	public WorkflowsDS(boolean retrieveDefinitions, boolean deployedOnly, Long userId) {
		this(null, retrieveDefinitions, deployedOnly, false, userId);
	}

	public WorkflowsDS(String workflowName, boolean retrieveDefinitions, boolean deployedOnly,
			boolean checkDeployExists, Long userId) {
		setTitleField("name");
		setRecordXPath("/list/workflow");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setRequired(true);
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField label = new DataSourceTextField("label");
		DataSourceIntegerField version = new DataSourceIntegerField("version");
		DataSourceDateTimeField date = new DataSourceDateTimeField("date");
		DataSourceTextField description = new DataSourceTextField("description");
		DataSourceBooleanField deployed = new DataSourceBooleanField("deployed");
		DataSourceTextField color = new DataSourceTextField("color");
		color.setHidden(true);
		setFields(id, name, label, version, date, deployed, description, color);
		setDataURL("data/workflows.xml?locale=" + I18N.getLocale()
				+ (retrieveDefinitions ? "&retrievedefinitions=true" : "") + (deployedOnly ? "&deployedOnly=true" : "")
				+ (workflowName != null ? "&name=" + workflowName : "") + (userId != null ? "&userId=" + userId : "")
				+ (checkDeployExists ? "&checkDeployExists=true" : "") + (checkDeployExists ? "&checkDeployExists=true" : ""));
		setClientOnly(true);
	}

	public WorkflowsDS(Long userId) {
		this(null, false, false, true, userId);
	}
}