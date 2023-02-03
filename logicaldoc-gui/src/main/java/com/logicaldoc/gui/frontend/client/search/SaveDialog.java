package com.logicaldoc.gui.frontend.client.search;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.validators.SimpleTextValidator;
import com.logicaldoc.gui.frontend.client.services.SearchService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;

/**
 * This is the form used to save and update the current search
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class SaveDialog extends Window {

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

		TextItem name = ItemFactory.newTextItem("name", null);
		name.setRequired(true);
		name.setValidators(new SimpleTextValidator());
		name.setWidth(200);

		TextItem description = ItemFactory.newTextItem("description", null);
		description.setBrowserSpellCheck(true);
		description.setWidth(300);

		ButtonItem save = new ButtonItem();
		save.setTitle(I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler((ClickEvent event) -> {
			onSave();
		});

		form.setFields(name, description, save);
		addItem(form);
	}

	private void onSave() {
		vm.validate();
		if (Boolean.TRUE.equals(vm.hasErrors()))
			return;

		final GUISearchOptions options = Search.get().getOptions();
		options.setName(vm.getValueAsString("name"));
		options.setDescription(vm.getValueAsString("description"));
		SearchService.Instance.get().save(Search.get().getOptions(), new AsyncCallback<Boolean>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Boolean b) {
				if (!b)
					SC.warn(I18N.message("duplicateelement"));
				else {
					try {
						if (SavedSearchesPanel.get() != null)
							SavedSearchesPanel.get().addEntry(vm.getValueAsString("name"),
									vm.getValueAsString("description"),
									options.getType() == GUISearchOptions.TYPE_FULLTEXT ? I18N.message("fulltext")
											: I18N.message("parametric"));
					} catch (Throwable t) {
						// Nothing to do
					}
					destroy();
				}
			}
		});
	}
}
