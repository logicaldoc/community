package com.logicaldoc.gui.frontend.client.folder;

import com.logicaldoc.gui.common.client.ServerValidationError;
import com.logicaldoc.gui.common.client.ServerValidationException;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Superclass for all tab panels in the folder details area
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public abstract class FolderDetailTab extends HLayout {

	protected GUIFolder folder;

	protected ChangedHandler changedHandler;

	/**
	 * Constructor
	 * 
	 * @param folder The folder panel refers to
	 * @param changedHandler The handler to be invoked in case of changes in the
	 *        folder
	 */
	public FolderDetailTab(GUIFolder folder, ChangedHandler changedHandler) {
		super();
		this.folder = folder;
		this.changedHandler = changedHandler;
	}

	public GUIFolder getFolder() {
		return folder;
	}

	public ChangedHandler getChangedHandler() {
		return changedHandler;
	}

	public void handleErrors(ServerValidationException errorException) {
		GuiLog.serverError(errorException);
		handleErrors(errorException.getErrors());
	}

	public void handleErrors(ServerValidationError[] errors) {

	}
}