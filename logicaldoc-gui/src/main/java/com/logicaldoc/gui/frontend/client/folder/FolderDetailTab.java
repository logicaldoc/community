package com.logicaldoc.gui.frontend.client.folder;

import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.frontend.client.panels.DetailTab;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;

/**
 * Superclass for all tab panels in the folder details area
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public abstract class FolderDetailTab extends DetailTab {

	protected GUIFolder folder;

	/**
	 * Constructor
	 * 
	 * @param folder The folder panel refers to
	 * @param changedHandler The handler to be invoked in case of changes in the
	 *        folder
	 */
	protected FolderDetailTab(GUIFolder folder, ChangedHandler changedHandler) {
		super(changedHandler);
		this.folder = folder;
	}

	public GUIFolder getFolder() {
		return folder;
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