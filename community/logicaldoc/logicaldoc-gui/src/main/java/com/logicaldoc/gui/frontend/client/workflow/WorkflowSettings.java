package com.logicaldoc.gui.frontend.client.workflow;

import java.util.Map;

import com.logicaldoc.gui.common.client.beans.GUIWorkflow;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.PickerIcon;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;

/**
 * This window contains a form with the main settings of the workflow.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 7.0
 */
public class WorkflowSettings extends Window {

	private GUIWorkflow workflow = null;

	private ValuesManager vm = new ValuesManager();

	private DynamicForm form = null;

	private SelectItem supervisor = null;

	private TextItem workflowName = null;

	public WorkflowSettings(GUIWorkflow workflow) {
		this.workflow = workflow;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		setTitle(I18N.message("workflowsettings"));
		setWidth(400);
		setHeight(220);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		form = new DynamicForm();
		form.setMargin(1);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(1);
		form.setValuesManager(vm);
		workflowName = ItemFactory.newSimpleTextItem("workflowName", "workflowname", null);
		workflowName.setRequired(true);
		if (this.workflow != null) {
			workflowName.setValue(this.workflow.getName());
			workflowName.setDisabled(!(this.workflow.getName() == null || this.workflow.getName().trim().isEmpty()));
		}

		TextAreaItem workflowDescr = ItemFactory.newTextAreaItem("workflowDescr", "workflowdescr", null);
		workflowDescr.setWrapTitle(false);
		workflowDescr.setValue(this.workflow.getDescription());

		supervisor = ItemFactory.newUserSelector("supervisor", "supervisor", null, true);
		supervisor.setValue(workflow.getSupervisor());
		supervisor.setWrapTitle(false);
		supervisor.setDisplayField("username");

		PickerIcon icon = new PickerIcon(PickerIcon.CLEAR, new FormItemClickHandler() {
			public void onFormItemClick(FormItemIconClickEvent event) {
				supervisor.setValue("");
			}
		});
		supervisor.setIcons(icon);

		ButtonItem save = new ButtonItem("save", I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(new ClickHandler() {
			@SuppressWarnings("unchecked")
			public void onClick(ClickEvent event) {
				Map<String, Object> values = (Map<String, Object>) vm.getValues();

				if (vm.validate()) {
					if (values.get("workflowDescr") != null)
						WorkflowSettings.this.workflow.setDescription(values.get("workflowDescr").toString());
					if (supervisor.getValueAsString() != null)
						WorkflowSettings.this.workflow.setSupervisor(supervisor.getValueAsString());
					destroy();
				}
			}
		});

		form.setItems(workflowName, workflowDescr, supervisor, save);
		setMembers(form);

		addItem(form);
	}
}