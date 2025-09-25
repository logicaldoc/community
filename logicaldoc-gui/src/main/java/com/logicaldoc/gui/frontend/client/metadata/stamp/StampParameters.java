package com.logicaldoc.gui.frontend.client.metadata.stamp;

import com.logicaldoc.gui.common.client.beans.GUIStamp;
import com.logicaldoc.gui.common.client.widgets.ExtendedPropertiesPanel;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;

/**
 * Shows stamp's input parameters that are collected from a Template
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.3
 */
public class StampParameters extends StampDetailsTab {
	private ExtendedPropertiesPanel propertiesPanel;

	public StampParameters(GUIStamp stamp, ChangedHandler changedHandler) {
		super(stamp, changedHandler);
		setWidth100();
		setHeight100();
		setMembersMargin(1);

		propertiesPanel = new ExtendedPropertiesPanel(stamp, changedHandler, true, false, true);
		setMembers(propertiesPanel);
	}

	public boolean validate() {
		return propertiesPanel.validate();
	}
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}