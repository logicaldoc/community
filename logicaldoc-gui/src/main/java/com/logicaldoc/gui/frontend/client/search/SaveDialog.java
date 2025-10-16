package com.logicaldoc.gui.frontend.client.search;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.SearchService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
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
public class SaveDialog extends Window {

	private static final String DESCRIPTION = "description";

	private ValuesManager vm = new ValuesManager();

	public SaveDialog() {
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

		TextItem name = ItemFactory.newSimpleTextItemWithHyphen("name", null);
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
		vm.validate();
		if (Boolean.TRUE.equals(vm.hasErrors()))
			return;

		final GUISearchOptions options = Search.get().getOptions();
		options.setName(vm.getValueAsString("name"));
		options.setDescription(vm.getValueAsString(DESCRIPTION));
		SearchService.Instance.get().save(Search.get().getOptions(), new DefaultAsyncCallback<>() {
			@Override
			public void handleSuccess(Boolean b) {
				if (Boolean.FALSE.equals(b))
					SC.warn(I18N.message("duplicateelement"));
				else {
					try {
						if (SavedSearchesPanel.get() != null)
							SavedSearchesPanel.get().addEntry(vm.getValueAsString("name"),
									vm.getValueAsString(DESCRIPTION),
									options.getType() == GUISearchOptions.TYPE_FULLTEXT ? I18N.message("fulltext")
											: I18N.message("parametric"));
					} catch (Exception t) {
						// Nothing to do
					}
					destroy();
				}
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