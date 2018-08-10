package com.logicaldoc.gui.frontend.client.workflow;

import com.logicaldoc.gui.common.client.beans.GUIWorkflow;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Window;

/**
 * Show the completion diagram
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.5
 */
public class WorkflowCompletionWindow extends Window {

	public WorkflowCompletionWindow(GUIWorkflow workflow) {
		super();
		setOverflow(Overflow.SCROLL);
		setWidth100();
		setHeight100();
		setTitle(I18N.message("completiondiagram"));

		DrawingPanel drawingPanel = new DrawingPanel(workflow);
		addItem(drawingPanel);
	}
}
