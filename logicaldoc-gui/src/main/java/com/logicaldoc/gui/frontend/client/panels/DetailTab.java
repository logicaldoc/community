package com.logicaldoc.gui.frontend.client.panels;

import com.logicaldoc.gui.common.client.ServerValidationError;
import com.logicaldoc.gui.common.client.ServerValidationException;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * A generic panel to display details of an object
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.3
 */
public abstract class DetailTab extends HLayout {

	protected ChangedHandler changedHandler;

	protected DetailTab() {
		super();
	}

	protected DetailTab(ChangedHandler changedHandler) {
		this.changedHandler = changedHandler;
	}

	protected DetailTab(int membersMargin) {
		super(membersMargin);
	}

	public ChangedHandler getChangedHandler() {
		return changedHandler;
	}

	public boolean validate() {
		return true;
	}

	public void handleErrors(ServerValidationException errorException) {
		GuiLog.serverError(errorException);
		handleErrors(errorException.getErrors());
	}

	public void handleErrors(ServerValidationError[] errors) {
		// Nothing to do
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