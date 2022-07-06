package com.logicaldoc.gui.frontend.client.workflow;

import com.logicaldoc.gui.common.client.beans.GUIWorkflow;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.PrintUtil;
import com.logicaldoc.gui.frontend.client.workflow.designer.WorkflowDrawingPanel;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.PanelPlacement;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;

/**
 * Show the completion diagram
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.5
 */
public class WorkflowPreview extends Window {

	public WorkflowPreview(String title, GUIWorkflow workflow) {
		super();
		setPlacement(PanelPlacement.FILLSCREEN);
		setOverflow(Overflow.SCROLL);
		setWidth100();
		setHeight100();
		setTitle(title);

		final WorkflowDrawingPanel drawingPanel = new WorkflowDrawingPanel(workflow);

		HeaderControl print = new HeaderControl(HeaderControl.PRINT, new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				PrintUtil.printScreenShot(drawingPanel.getID(), I18N.message("workflow") + " - " + workflow.getName());
			}
		});
		setHeaderControls(HeaderControls.HEADER_LABEL, print, HeaderControls.CLOSE_BUTTON);

		addItem(drawingPanel);
	}

	public WorkflowPreview(GUIWorkflow workflow) {
		this(I18N.message("completiondiagram"), workflow);
	}
}