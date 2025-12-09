package com.logicaldoc.gui.frontend.client.search;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.SearchService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * This is the form used to save and update the current search
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class SaveSearchDialog extends Window {

	private static final String DESCRIPTION = "description";

	private ValuesManager vm = new ValuesManager();

	public SaveSearchDialog() {
		super();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("savesearch"));
		setWidth(350);
		setHeight(100);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(5);
		setAutoSize(true);

		final DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setWidth(350);
		form.setMargin(5);

		TextItem name = ItemFactory.newSimpleTextItemWithHyphen("name", Search.get().getOptions().getName());
		name.setRequired(true);
		name.setWidth(200);

		TextItem description = ItemFactory.newTextItem(DESCRIPTION, null);
		description.setBrowserSpellCheck(true);
		description.setWidth(300);

		ButtonItem save = new ButtonItem();
		save.setTitle(I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(event -> onSave());

		form.setFields(name, description, save);
		addItem(form);
	}

	private void onSave() {
		if (Boolean.FALSE.equals(vm.validate()))
			return;

		GUISearchOptions options = Search.get().getOptions();
		options.setName(vm.getValueAsString("name"));
		options.setDescription(vm.getValueAsString(DESCRIPTION));
		SearchService.Instance.get().save(options, new DefaultAsyncCallback<>() {
			@Override
			public void handleSuccess(Void b) {
				destroy();
				SavedSearchesPanel.get().refresh();
			}
		});
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