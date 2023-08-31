package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;

public class WorkflowHistoriesDS extends DataSource {
	public WorkflowHistoriesDS(Long instanceId, Long workflowTemplateId, String eventFilter, String tagFilter,
			Integer max) {
		setRecordXPath("/list/workflowhistory");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setRequired(true);

		DataSourceTextField tag = new DataSourceTextField("tag");
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField display = new DataSourceTextField("display");
		display.setHidden(true);
		DataSourceTextField wfDisplay = new DataSourceTextField("wfDisplay");
		wfDisplay.setHidden(true);
		DataSourceTextField taskId = new DataSourceTextField("taskId");
		DataSourceDateTimeField startDate = new DataSourceDateTimeField("startdate");
		DataSourceDateTimeField endDate = new DataSourceDateTimeField("enddate");
		DataSourceTextField documents = new DataSourceTextField("documents");
		DataSourceTextField initiator = new DataSourceTextField("initiator");
		DataSourceTextField initiatorId = new DataSourceTextField("initiatorId");

		DataSourceTextField event = new DataSourceTextField("event");
		DataSourceDateTimeField date = new DataSourceDateTimeField("date");
		DataSourceTextField user = new DataSourceTextField("user");
		DataSourceTextField userId = new DataSourceTextField("userId");
		DataSourceTextField comment = new DataSourceTextField("comment");
		DataSourceTextField filename = new DataSourceTextField("filename");
		DataSourceTextField documentId = new DataSourceTextField("documentId");
		DataSourceTextField sessionId = new DataSourceTextField("sessionid");
		DataSourceTextField transition = new DataSourceTextField("transition");
		DataSourceImageField icon = new DataSourceImageField("icon");
		icon.setHidden(true);

		DataSourceIntegerField templateVersion = new DataSourceIntegerField("templateVersion");
		DataSourceIntegerField templateId = new DataSourceIntegerField("templateId");

		setFields(id, taskId, name, tag, startDate, endDate, documents, initiator, initiatorId, event, date, user,
				userId, comment, icon, filename, transition, documentId, sessionId, templateId, templateVersion,
				display, wfDisplay);
		setDataURL("data/workflowhistories.xml?locale=" + I18N.getLocale()
				+ (instanceId != null ? "&instanceId=" + instanceId : "")
				+ (workflowTemplateId != null ? "&workflowTemplateId=" + workflowTemplateId : "")
				+ (eventFilter != null ? "&event=" + eventFilter : "") + (tagFilter != null ? "&tag=" + tagFilter : "")
				+ (max != null ? "&max=" + max : ""));
		setClientOnly(true);
	}
}
