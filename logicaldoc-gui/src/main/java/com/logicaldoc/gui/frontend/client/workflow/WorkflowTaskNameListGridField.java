package com.logicaldoc.gui.frontend.client.workflow;

import com.logicaldoc.gui.common.client.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;

/**
 * A field to display the name of a workflow task with corresponding color
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class WorkflowTaskNameListGridField extends ColoredListGridField {

	public WorkflowTaskNameListGridField() {
		this("workflowStatus", "workflowStatusDisplay", I18N.message("workflowstatus"), 150);
	}

	public WorkflowTaskNameListGridField(String name) {
		this(name, "workflowStatusDisplay");
	}

	public WorkflowTaskNameListGridField(String name, String colorFieldName, String title, int width) {
		super(name, colorFieldName, title, width);
	}

	public WorkflowTaskNameListGridField(String name, String colorFieldName, String title) {
		this(name, colorFieldName, I18N.message(title), 150);
	}

	public WorkflowTaskNameListGridField(String name, String colorFieldName) {
		this(name, colorFieldName, null);
	}
}