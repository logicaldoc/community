package com.logicaldoc.gui.frontend.client.impex.archives;

import com.logicaldoc.gui.common.client.beans.GUIArchive;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.ToggleItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows the settings form for an import archive.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ImportSettingsPanel extends VLayout {

	private static final String IMPORTTEMPLATES = "importtemplates";

	private GUIArchive archive;

	private ValuesManager vm = new ValuesManager();

	private DynamicForm form = new DynamicForm();

	public ImportSettingsPanel(GUIArchive archive, ChangedHandler changedHandler) {
		this.archive = archive;

		form.setValuesManager(vm);
		form.setTitleOrientation(TitleOrientation.TOP);

		TextItem description = ItemFactory.newTextItem("description", archive.getDescription());
		description.addChangedHandler(changedHandler);
		description.setDisabled(archive.getStatus() != GUIArchive.STATUS_OPEN);

		ToggleItem importTemplates = ItemFactory.newToggleItem(IMPORTTEMPLATES, archive.getImportTemplate() == 1);
		importTemplates.addChangedHandler(changedHandler);
		importTemplates.setDisabled(archive.getStatus() != GUIArchive.STATUS_OPEN);

		SelectItem options = ItemFactory.newImportCustomIds();
		options.setWidth(200);
		options.setValue(Integer.toString(archive.getImportCustomId()));
		options.addChangedHandler(changedHandler);
		options.setDisabled(archive.getStatus() != GUIArchive.STATUS_OPEN);

		form.setFields(description, importTemplates, options);

		addMember(form);
	}

	public boolean validate() {
		vm.getValues();
		vm.validate();
		if (Boolean.FALSE.equals(vm.hasErrors())) {
			archive.setDescription(vm.getValueAsString("description"));
			archive.setImportCustomId(Integer.parseInt(vm.getValueAsString("importcids")));
			archive.setImportTemplate(Boolean.parseBoolean(vm.getValueAsString(IMPORTTEMPLATES)) ? 1 : 0);
			return true;
		} else
			return false;
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