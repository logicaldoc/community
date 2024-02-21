package com.logicaldoc.gui.frontend.client.workflow.designer;

import java.util.stream.Collectors;

import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.beans.GUIWorkflow;
import com.logicaldoc.gui.common.client.data.UsersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.ColorPickerItem;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * This window contains a form with the main settings of the workflow.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 7.0
 */
public class WorkflowSettings extends Window {

	private static final String COLOR = "color";

	private static final String WORKFLOWDESCR = "workflowdescr";

	private static final String WORKFLOW_LABEL = "workflowLabel";

	private GUIWorkflow workflow = null;

	private ValuesManager vm = new ValuesManager();

	private DynamicForm form = null;

	private TextItem workflowName = null;

	public WorkflowSettings(GUIWorkflow workflow) {
		this.workflow = workflow;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		setTitle(I18N.message("workflowsettings"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setAutoSize(true);
		centerInPage();

		form = new DynamicForm();
		form.setMargin(1);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(2);
		form.setMinWidth(300);
		form.setValuesManager(vm);
		workflowName = ItemFactory.newSimpleTextItem("workflowName", "workflowname", null);
		workflowName.setRequired(true);
		if (this.workflow != null) {
			workflowName.setValue(this.workflow.getName());
			workflowName.setDisabled(!(this.workflow.getName() == null || this.workflow.getName().trim().isEmpty()));
		}

		TextItem workflowLabel = ItemFactory.newTextItem(WORKFLOW_LABEL, "label", workflow.getLabel());

		StaticTextItem id = ItemFactory.newStaticTextItem("id", this.workflow != null ? this.workflow.getId() : "");
		StaticTextItem version = ItemFactory.newStaticTextItem("version",
				this.workflow != null ? "" + this.workflow.getVersion() : "");

		TextAreaItem workflowDescr = ItemFactory.newTextAreaItem(WORKFLOWDESCR, workflow.getDescription());
		workflowDescr.setWrapTitle(false);
		workflowDescr.setHeight(80);
		workflowDescr.setWidth("*");
		workflowDescr.setColSpan(2);

		ColorPickerItem color = ItemFactory.newColorPickerItem(workflow.getColor(), true, null);
		color.setWrapTitle(false);
		color.setEndRow(true);

		MultiComboBoxItem supervisors = ItemFactory.newMultiComboBoxItem("supervisors", "supervisors",
				new UsersDS(null, false, false), workflow.getSupervisors().stream().map(GUIUser::getId)
						.collect(Collectors.toList()).toArray(new Long[0]));
		supervisors.setWidth("*");
		supervisors.setValueField("id");
		supervisors.setDisplayField("username");
		supervisors.setColSpan(2);

		ButtonItem save = prepareSaveButton(workflow, supervisors);

		form.setItems(workflowName, workflowLabel, id, version, workflowDescr, color, supervisors, save);
		setMembers(form);

		addItem(form);
	}

	private ButtonItem prepareSaveButton(GUIWorkflow workflow, MultiComboBoxItem supervisors) {
		ButtonItem save = new ButtonItem("save", I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(event -> {
			if (Boolean.TRUE.equals(vm.validate()))
				onSave(workflow, supervisors);
		});
		save.setDisabled(!workflow.isLatestVersion());
		return save;
	}

	private void onSave(GUIWorkflow workflow, MultiComboBoxItem supervisors) {
		WorkflowSettings.this.workflow.setDescription(vm.getValueAsString(WORKFLOWDESCR));
		WorkflowSettings.this.workflow.setLabel(vm.getValueAsString(WORKFLOW_LABEL));
		WorkflowSettings.this.workflow.setColor(vm.getValueAsString(COLOR));

		String[] supervisorIds = supervisors.getValues();
		workflow.getSupervisors().clear();
		if (supervisorIds != null && supervisorIds.length > 0)
			for (int i = 0; i < supervisorIds.length; i++) {
				GUIUser user = new GUIUser();
				user.setId(Long.parseLong(supervisorIds[i]));
				workflow.getSupervisors().add(user);
			}

		destroy();
	}
}