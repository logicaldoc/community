package com.logicaldoc.gui.frontend.client.settings.automation;

import com.logicaldoc.gui.common.client.beans.GUIAutomationRoutine;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Superclass for all tab panels in the automation routine details area
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public abstract class AutomationRoutineDetailsTab extends HLayout {

	protected GUIAutomationRoutine routine;

	protected ChangedHandler changedHandler;

	protected AutomationRoutineDetailsTab(GUIAutomationRoutine routine, ChangedHandler changedHandler) {
		super();
		this.routine = routine;
		this.changedHandler = changedHandler;
	}

	public GUIAutomationRoutine getRoutine() {
		return routine;
	}

	public ChangedHandler getChangedHandler() {
		return changedHandler;
	}
}