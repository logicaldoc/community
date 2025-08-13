package com.logicaldoc.gui.frontend.client.document;

import com.logicaldoc.gui.common.client.ServerValidationError;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.widgets.ExtendedPropertiesPanel;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;

/**
 * Shows document's extended properties.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentExtendedPropertiesPanel extends DocumentDetailTab {

	private ExtendedPropertiesPanel extPropertiesPanel;

	public DocumentExtendedPropertiesPanel(GUIDocument document, ChangedHandler changedHandler,
			ChangedHandler templateChangedHandler) {
		super(document, changedHandler);
		setWidth100();
		setHeight100();
		setMembersMargin(1);

		extPropertiesPanel = new ExtendedPropertiesPanel(document, changedHandler, templateChangedHandler,
				updateEnabled, !document.isBulkUpdate(), true);
		setMembers(extPropertiesPanel);
	}

	@Override
	public boolean validate() {
		return extPropertiesPanel.validate();
	}

	@Override
	public void handleErrors(ServerValidationError[] errors) {
		extPropertiesPanel.onErrors(errors);
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