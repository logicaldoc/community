package com.logicaldoc.gui.frontend.client.settings.automation;

import com.logicaldoc.gui.common.client.beans.GUIAutomationRoutine;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Shows routine's standard properties
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
public class AutomationRoutineProperties extends AutomationRoutineDetailsTab {

	private HLayout formsContainer = new HLayout();

	private ValuesManager vm = new ValuesManager();

	public AutomationRoutineProperties(GUIAutomationRoutine routine, final ChangedHandler changedHandler) {
		super(routine, changedHandler);
		setWidth100();
		setHeight100();

		setMembers(formsContainer);
		refresh();
	}

	private void refresh() {
		vm.clearValues();
		vm.clearErrors(false);

		if (formsContainer.getMembers() != null)
			formsContainer.removeMembers(formsContainer.getMembers());

		DynamicForm form2 = new DynamicForm();
		form2.setWidth100();
		form2.setTitleOrientation(TitleOrientation.TOP);
		form2.setValuesManager(vm);
		form2.setNumCols(1);

		final TextAreaItem automation = ItemFactory.newTextAreaItemForAutomation("automation", routine.getAutomation(),
				changedHandler, false);
		automation.setStartRow(false);
		automation.setRequired(true);
		automation.setWidth("*");

		form2.setItems(automation);

		DynamicForm form1 = new DynamicForm();
		form1.setNumCols(1);
		form1.setTitleOrientation(TitleOrientation.TOP);
		form1.setValuesManager(vm);

		TextItem name = ItemFactory.newSimpleTextItem("name", routine.getName());
		name.addChangedHandler(changedHandler);
		name.setDisabled(routine.getId() != 0L);

		TextItem description = ItemFactory.newTextItem("description", routine.getDescription());
		description.addChangedHandler(changedHandler);
		description.setWidth(200);

		form1.setItems(name, description);

		formsContainer.setMembers(form1, form2);
	}

	@Override
	public boolean validate() {
		vm.validate();
		if (Boolean.FALSE.equals(vm.hasErrors())) {
			routine.setName(vm.getValueAsString("name"));
			routine.setDescription(vm.getValueAsString("description"));
			routine.setAutomation(vm.getValueAsString("automation"));
		}
		return !vm.hasErrors();
	}
}