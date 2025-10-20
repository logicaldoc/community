package com.logicaldoc.gui.frontend.client.search;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUICriterion;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.frontend.client.folder.FolderSearchForm;
import com.logicaldoc.gui.frontend.client.services.TemplateService;

/**
 * Shows a folders search form
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
public class FoldersForm extends FolderSearchForm implements SearchObserver {

	private static FoldersForm instance;

	public static FoldersForm get() {
		if (instance == null) {
			instance = new FoldersForm();
			Search.get().addObserver(instance);
		}
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

	@Override
	public void onSearchArrived() {
		// Nothing to do
	}

	@Override
	public void onOptionsChanged(GUISearchOptions options) {
		if (options.getType() == GUISearchOptions.TYPE_FOLDERS) {
			defaultOptions = options;
			SearchMenu.get().openFoldersSection();
			if (isDrawn())
				applyOptions(options);
		}
	}

	@Override
	protected GUISearchOptions prepareOptions() {
		GUISearchOptions options = super.prepareOptions();
		options.setSource(this);
		return options;
	}

	@Override
	protected void applyOptions(GUISearchOptions options) {
		if (options == null)
			return;

		folderSelector.setFolder(options.getFolder(), options.getFolderName());

		vm.setValue("subfolders", options.isSearchInSubPath());
		vm.setValue("aliases", options.getRetrieveAliases() == 1);

		vm.setValue(CASESENSITIVE, options.getCaseSensitive() == 1);
		vm.setValue("match", options.getTopOperator());

		conditionsLayout.removeMembers(conditionsLayout.getMembers());

		if (options.getTemplate() != null) {
			vm.setValue("template", Long.toString(options.getTemplate()));
			TemplateService.Instance.get().getTemplate(options.getTemplate(), new DefaultAsyncCallback<>() {
				@Override
				public void handleSuccess(GUITemplate result) {
					selectedTemplate = result;
					applyCriteria(options);
				}
			});
		} else {
			selectedTemplate = null;
			vm.setValue("template", (String) null);
			applyCriteria(options);
		}
	}

	private void applyCriteria(GUISearchOptions options) {
		for (GUICriterion criterion : options.getCriteria()) {
			ParameterConditionRow row = appendCondition();
			row.setCriterion(criterion);
		}
	}
}