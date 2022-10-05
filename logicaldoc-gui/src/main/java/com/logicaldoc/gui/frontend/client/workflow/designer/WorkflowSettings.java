package com.logicaldoc.gui.frontend.client.workflow.designer;

import java.util.Map;

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
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;

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

		TextItem workflowLabel = ItemFactory.newTextItem("workflowLabel", "label", workflow.getLabel());

		StaticTextItem id = ItemFactory.newStaticTextItem("id", "id",
				this.workflow != null ? this.workflow.getId() : "");
		StaticTextItem version = ItemFactory.newStaticTextItem("version", "version",
				this.workflow != null ? "" + this.workflow.getVersion() : "");

		TextAreaItem workflowDescr = ItemFactory.newTextAreaItem("workflowDescr", "workflowdescr",
				workflow.getDescription());
		workflowDescr.setWrapTitle(false);
		workflowDescr.setHeight(80);
		workflowDescr.setWidth("*");
		workflowDescr.setColSpan(2);

		Long[] ids = null;
		if (workflow.getSupervisors() != null && workflow.getSupervisors().length > 0) {
			ids = new Long[workflow.getSupervisors().length];
			for (int i = 0; i < ids.length; i++)
				ids[i] = workflow.getSupervisors()[i].getId();
		}

		MultiComboBoxItem supervisors = ItemFactory.newMultiComboBoxItem("supervisors", "supervisors",
				new UsersDS(null, false, false), ids);
		supervisors.setWidth("*");
		supervisors.setValueField("id");
		supervisors.setDisplayField("username");
		supervisors.setColSpan(2);

		ButtonItem save = new ButtonItem("save", I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(new ClickHandler() {
			@SuppressWarnings("unchecked")
			public void onClick(ClickEvent event) {
				Map<String, Object> values = (Map<String, Object>) vm.getValues();

				if (vm.validate()) {
					if (values.get("workflowDescr") != null)
						WorkflowSettings.this.workflow.setDescription(values.get("workflowDescr").toString());
					else
						WorkflowSettings.this.workflow.setDescription(null);

					if (values.get("workflowLabel") != null)
						WorkflowSettings.this.workflow.setLabel(values.get("workflowLabel").toString());
					else
						WorkflowSettings.this.workflow.setLabel(null);

					String[] ids = supervisors.getValues();
					GUIUser[] users = new GUIUser[ids != null ? ids.length : 0];
					if (ids != null && ids.length > 0)
						for (int i = 0; i < ids.length; i++) {
							GUIUser user = new GUIUser();
							user.setId(Long.parseLong(ids[i]));
							users[i] = user;
						}
					workflow.setSupervisors(users);

					destroy();
				}
			}
		});
		save.setDisabled(!workflow.isLatestVersion());

		form.setItems(workflowName, workflowLabel, id, version, workflowDescr, supervisors, save);
		setMembers(form);

		addItem(form);
	}
}