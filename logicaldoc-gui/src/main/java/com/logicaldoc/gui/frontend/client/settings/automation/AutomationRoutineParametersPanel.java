package com.logicaldoc.gui.frontend.client.settings.automation;

import com.logicaldoc.gui.common.client.beans.GUIAutomationRoutine;
import com.logicaldoc.gui.common.client.widgets.ExtendedPropertiesPanel;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;

/**
 * Shows routine's input parameters that are collected from a Template
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.1
 */
public class AutomationRoutineParametersPanel extends AutomationRoutineDetailsTab {
	private ExtendedPropertiesPanel propertiesPanel;

	public AutomationRoutineParametersPanel(GUIAutomationRoutine routine, ChangedHandler changedHandler) {
		super(routine, changedHandler);
		setWidth100();
		setHeight100();
		setMembersMargin(1);

		propertiesPanel = new ExtendedPropertiesPanel(routine, changedHandler, true, false, true, true);
		setMembers(propertiesPanel);
	}

	public boolean validate() {
		return propertiesPanel.validate();
	}
}