package com.logicaldoc.gui.frontend.client.search;

import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.frontend.client.folder.FolderSearchForm;

/**
 * Shows a folders search form
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
public class FoldersForm extends FolderSearchForm {
	private static FoldersForm instance;

	public static FoldersForm get() {
		if (instance == null)
			instance = new FoldersForm();
		return instance;
	}
	
	private FoldersForm() {
		super();
	}

	
	@Override
	protected void search(GUISearchOptions options) {
		if (options != null) {
			Search.get().setOptions(options);
			Search.get().search();
		}
	}
}