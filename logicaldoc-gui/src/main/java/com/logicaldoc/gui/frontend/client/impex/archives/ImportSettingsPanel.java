package com.logicaldoc.gui.frontend.client.impex.archives;

import com.logicaldoc.gui.common.client.beans.GUIArchive;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows the settings form for an import archive.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ImportSettingsPanel extends VLayout {
	private GUIArchive archive;

	private ValuesManager vm = new ValuesManager();

	private DynamicForm form = new DynamicForm();

	public ImportSettingsPanel(GUIArchive archive, ChangedHandler changedHandler) {
		this.archive = archive;

		form.setValuesManager(vm);
		form.setTitleOrientation(TitleOrientation.TOP);

		TextItem description = ItemFactory.newTextItem("description", archive.getDescription());
		description.addChangedHandler(changedHandler);
		description.setDisabled(archive.getStatus() != GUIArchive.STATUS_OPENED);

		RadioGroupItem importTemplates = ItemFactory.newBooleanSelector("importtemplates", "importtemplates");
		importTemplates.setValue(archive.getImportTemplate() == 1 ? "yes" : "no");
		importTemplates.addChangedHandler(changedHandler);
		importTemplates.setDisabled(archive.getStatus() != GUIArchive.STATUS_OPENED);

		SelectItem options = ItemFactory.newImportCustomIds();
		options.setWidth(200);
		options.setValue(Integer.toString(archive.getImportCustomId()));
		options.addChangedHandler(changedHandler);
		options.setDisabled(archive.getStatus() != GUIArchive.STATUS_OPENED);

		form.setFields(description, importTemplates, options);

		addMember(form);
	}

	public boolean validate() {
		vm.getValues();
		vm.validate();
		if (!vm.hasErrors()) {
			archive.setDescription(vm.getValueAsString("description").toString());
			archive.setImportCustomId(Integer.parseInt(vm.getValueAsString("importcids")));
			archive.setImportTemplate("yes".equals(vm.getValueAsString("importtemplates")) ? 1 : 0);
			return true;
		} else
			return false;
	}
}