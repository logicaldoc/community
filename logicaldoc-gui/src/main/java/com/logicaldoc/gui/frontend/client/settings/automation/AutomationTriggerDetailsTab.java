package com.logicaldoc.gui.frontend.client.settings.automation;

import com.logicaldoc.gui.common.client.beans.GUIAutomationTrigger;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Superclass for all tab panels in the import folders details area
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public abstract class AutomationTriggerDetailsTab extends HLayout {

	protected GUIAutomationTrigger trigger;

	protected ChangedHandler changedHandler;

	public AutomationTriggerDetailsTab(GUIAutomationTrigger trigger, ChangedHandler changedHandler) {
		super();
		this.trigger = trigger;
		this.changedHandler = changedHandler;
	}

	public GUIAutomationTrigger getTrigger() {
		return trigger;
	}

	public ChangedHandler getChangedHandler() {
		return changedHandler;
	}
}