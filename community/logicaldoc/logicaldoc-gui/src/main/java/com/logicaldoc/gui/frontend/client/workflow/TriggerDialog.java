package com.logicaldoc.gui.frontend.client.workflow;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.WorkflowService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.MultipleAppearance;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SubmitItem;
import com.smartgwt.client.widgets.layout.VStack;

/**
 * This is the form used for editing a workflow trigger
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5
 */
public class TriggerDialog extends Window {

	public TriggerDialog(WorkflowTriggersPanel panel) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		setTitle(I18N.message("workflowtriggertext"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		String[] events = new String[] { "event.stored" };

		if (panel.getSelectedRecord() != null) {
			String buf = panel.getSelectedRecord().getAttributeAsString("events");
			if (buf == null || buf.isEmpty())
				events = null;
			else if (!buf.contains(","))
				events = new String[] { buf.trim() };
			else {
				events = buf.split(",");
			}
		}

		VStack layout = new VStack(5);

		// Workflows list
		DynamicForm workflowForm = new DynamicForm();
		workflowForm.setAlign(Alignment.LEFT);
		workflowForm.setTitleOrientation(TitleOrientation.LEFT);
		workflowForm.setNumCols(2);
		workflowForm.setColWidths(110, "*");

		SelectItem workflow = ItemFactory.newWorkflowSelector();
		workflow.setColSpan(2);
		workflow.setEndRow(true);
		workflow.setRequired(true);
		workflowForm.setItems(workflow);
		if (panel.getSelectedRecord() != null)
			workflow.setValue(panel.getSelectedRecord().getAttributeAsLong("workflowId"));

		DynamicForm templateForm = new DynamicForm();
		templateForm.setAlign(Alignment.LEFT);
		templateForm.setTitleOrientation(TitleOrientation.LEFT);
		templateForm.setNumCols(2);
		templateForm.setColWidths(110, "*");
		
		// Templates list
		SelectItem template = ItemFactory.newTemplateSelector(true, null);
		template.setWrapTitle(false);
		template.setColSpan(2);
		template.setEndRow(true);
		template.setWidth(200);
		template.setMultipleAppearance(MultipleAppearance.GRID);
		templateForm.setItems(template);
		if (panel.getSelectedRecord() != null)
			template.setValue(panel.getSelectedRecord().getAttributeAsLong("templateId"));

		SelectItem eventsSelector = ItemFactory.newEventsSelector("events", "triggeron", null, false, false, false);
		eventsSelector.setValue(events);

		DynamicForm form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.LEFT);

		SubmitItem saveButton = new SubmitItem("save", I18N.message("save"));
		saveButton.setAlign(Alignment.LEFT);
		saveButton.addClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {
			@Override
			public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
				String workflowSelectedId = "";
				if (workflow.getValue() != null) {
					workflowSelectedId = workflow.getValue().toString();

					String templateSelectedId = "";
					if (template.getValue() != null)
						templateSelectedId = template.getValueAsString();

					WorkflowService.Instance.get().saveTrigger(Long.toString(panel.getFolder().getId()),
							workflowSelectedId, templateSelectedId, eventsSelector.getValueAsString(),
							new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void result) {
									panel.refresh();
									destroy();
								}
							});
				} else {
					SC.warn(I18N.message("workflowselection"));
				}
			}
		});

		form.setFields(eventsSelector, saveButton);

		layout.addMember(workflowForm);
		layout.addMember(templateForm);
		layout.addMember(form);

		addItem(layout);
	}
}