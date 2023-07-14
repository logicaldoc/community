package com.logicaldoc.gui.frontend.client.impex.folders;

import java.util.Map;

import com.logicaldoc.gui.common.client.beans.GUIImportFolder;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Displays the automation routine associated to the ImportFolder
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.5
 */
public class ImportFolderAutomationPanel extends ImportFolderDetailsTab {

	private DynamicForm form = new DynamicForm();

	private HLayout container = new HLayout();

	public ImportFolderAutomationPanel(GUIImportFolder importFolder, ChangedHandler changedHandler) {
		super(importFolder, changedHandler);
		setWidth100();
		setHeight100();
		setMembers(container);
	}

	@Override
	public void onDraw() {
		form.clearValues();
		form.clearErrors(false);
		form.destroy();

		if (Boolean.TRUE.equals(container.contains(form)))
			container.removeChild(form);

		form = new DynamicForm();
		form.setWidth100();
		form.setHeight100();
		form.setNumCols(1);
		form.setTitleOrientation(TitleOrientation.TOP);

		TextAreaItem automationBefore = ItemFactory.newTextAreaItemForAutomation("automationBefore",
				"whendocumentprocessing", importFolder.getAutomation(), changedHandler, false);
		automationBefore.setRequired(false);
		automationBefore.setWidth("*");
		automationBefore.setHeight("*");
		automationBefore.addChangedHandler(changedHandler);

		TextAreaItem automationAfter = ItemFactory.newTextAreaItemForAutomation("automationAfter",
				"afterdocumentprocessed", importFolder.getAutomationAfter(), changedHandler, false);
		automationAfter.setRequired(false);
		automationAfter.setWidth("*");
		automationAfter.setHeight("*");
		automationAfter.addChangedHandler(changedHandler);

		form.setItems(automationBefore, automationAfter);

		container.addMember(form);
	}

	@SuppressWarnings("unchecked")
	boolean validate() {
		Map<String, Object> values = (Map<String, Object>) form.getValues();
		form.validate();
		if (Boolean.FALSE.equals(form.hasErrors())) {
			importFolder.setAutomation((String) values.get("automationBefore"));
			importFolder.setAutomationAfter((String) values.get("automationAfter"));
		}
		return !form.hasErrors();
	}
}