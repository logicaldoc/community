package com.logicaldoc.gui.frontend.client.impex.folders;

import com.logicaldoc.gui.common.client.beans.GUIImportFolder;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Superclass for all tab panels in the import folders details area
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public abstract class ImportFolderDetailsTab extends HLayout {
	protected GUIImportFolder importFolder;

	protected ChangedHandler changedHandler;

	/**
	 * 
	 * @param importFolder The importFolder this instance refers to
	 * @param changedHandler The handler to be invoked in case of changes in the
	 *        importFolder
	 */
	public ImportFolderDetailsTab(GUIImportFolder importFolder, ChangedHandler changedHandler) {
		super();
		this.importFolder = importFolder;
		this.changedHandler = changedHandler;
	}

	public GUIImportFolder getImportFolder() {
		return importFolder;
	}

	public ChangedHandler getChangedHandler() {
		return changedHandler;
	}
}
