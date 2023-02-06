package com.logicaldoc.gui.frontend.client.folder;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAutomationRoutine;
import com.logicaldoc.gui.common.client.beans.GUIAutomationTrigger;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.AutomationService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This is the form used for editing an Automation Trigger.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
public class AutomationTriggerDialog extends Window {

	private static final String EVENTS = "events";

	private GUIAutomationTrigger trigger;

	private FolderAutomationPanel automationPanel;

	private ValuesManager vm = new ValuesManager();

	private HLayout forms = new HLayout();

	private SelectItem routine;

	public AutomationTriggerDialog(GUIAutomationTrigger trigger, FolderAutomationPanel automationPanel) {
		this.trigger = trigger;
		this.automationPanel = automationPanel;

		HeaderControl closeIcon = new HeaderControl(HeaderControl.CLOSE, new ClickHandler() {
			public void onClick(ClickEvent event) {
				destroy();
			}
		});

		setHeaderControls(HeaderControls.HEADER_LABEL, closeIcon);
		setTitle(I18N.message("automationtrigger"));

		setWidth("60%");
		setHeight(460);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				onSave();
			}
		});

		ToolStripButton close = new ToolStripButton();
		close.setTitle(I18N.message("close"));
		close.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				destroy();
			}
		});

		DynamicForm form2 = new DynamicForm();
		form2.setWidth100();
		form2.setHeight100();
		form2.setTitleOrientation(TitleOrientation.TOP);
		form2.setNumCols(1);
		form2.setValuesManager(vm);

		final TextAreaItem automation = ItemFactory.newTextAreaItemForAutomation("automation", trigger.getAutomation(),
				null, false);
		automation.setShowTitle(false);
		automation.setStartRow(false);
		automation.setWidth("*");
		automation.setHeight("*");
		automation.setDisabled(trigger.getRoutine() != null);
		form2.setItems(automation);

		ChangedHandler changeHandler = new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				if (event == null) {
					automation.setDisabled(false);
				} else {
					automation.setDisabled(event.getValue() != null && !event.getValue().toString().isEmpty());
				}
			}
		};
		routine = ItemFactory.newAutomationRoutineSelector("routine",
				trigger.getRoutine() != null ? trigger.getRoutine().getId() : null, changeHandler, true);

		toolStrip.addFormItem(routine);
		toolStrip.addButton(save);
		toolStrip.addSeparator();
		toolStrip.addButton(close);

		DynamicForm form1 = new DynamicForm();
		form1.setTitleOrientation(TitleOrientation.TOP);
		form1.setHeight100();
		form1.setNumCols(1);
		form1.setValuesManager(vm);

		SelectItem events = ItemFactory.newEventsSelector(EVENTS, I18N.message("triggeron"), null, true, true, true,
				true);
		events.setHeight(210);
		events.setHeight(250);
		events.setEndRow(true);
		events.setValues(trigger.getEventsArray());

		form1.setItems(events);

		forms.setMembers(form1, form2);
		forms.setWidth100();
		forms.setHeight100();

		addItem(toolStrip);
		addItem(forms);
	}

	private void onSave() {
		vm.validate();
		if (Boolean.FALSE.equals(vm.hasErrors())) {
			Long routineId = null;
			if (routine.getValue() != null && !routine.getValueAsString().isEmpty())
				routineId = Long.parseLong(routine.getValueAsString());

			if (routineId != null) {
				GUIAutomationRoutine aRoutine = new GUIAutomationRoutine(Long.parseLong(routine.getValue().toString()));
				routine.setName(routine.getSelectedRecord().getAttributeAsString("name"));
				trigger.setRoutine(aRoutine);
			} else
				trigger.setRoutine(null);

			trigger.setAutomation(vm.getValueAsString("automation"));

			String eventsStr = null;
			if (vm.getValueAsString(EVENTS) != null) {
				String buf = vm.getValueAsString(EVENTS).toString().trim().toLowerCase();
				buf = buf.replace('[', ' ');
				buf = buf.replace(']', ' ');
				eventsStr = buf.replace(" ", "");
			}

			trigger.setEvents(eventsStr);

			AutomationService.Instance.get().saveTrigger(trigger, new AsyncCallback<GUIAutomationTrigger>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(GUIAutomationTrigger trg) {
					automationPanel.updateRecord(trg);
					destroy();
				}
			});
		}
	}
}