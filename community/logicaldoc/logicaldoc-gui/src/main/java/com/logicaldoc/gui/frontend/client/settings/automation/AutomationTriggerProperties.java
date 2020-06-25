package com.logicaldoc.gui.frontend.client.settings.automation;

import java.util.Map;

import com.logicaldoc.gui.common.client.beans.GUIAutomationRoutine;
import com.logicaldoc.gui.common.client.beans.GUIAutomationTrigger;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.FolderChangeListener;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpacerItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Shows trigger's standard properties
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
public class AutomationTriggerProperties extends AutomationTriggerDetailsTab {

	private HLayout formsContainer = new HLayout();

	private FolderSelector folderSelector;

	private ValuesManager vm = new ValuesManager();

	public AutomationTriggerProperties(GUIAutomationTrigger trigger, final ChangedHandler changedHandler) {
		super(trigger, changedHandler);
		setWidth100();
		setHeight100();

		setMembers(formsContainer);
		folderSelector = new FolderSelector("folder", true);
		folderSelector.setWidth(200);
		folderSelector.setEndRow(true);
		folderSelector.setTitle(I18N.message("folder"));
		if (trigger.getFolder() != null)
			folderSelector.setFolder(trigger.getFolder());
		folderSelector.addFolderChangeListener(new FolderChangeListener() {
			@Override
			public void onChanged(GUIFolder folder) {
				changedHandler.onChanged(null);
			}
		});

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

		final TextAreaItem automation = ItemFactory.newTextAreaItemForAutomation("automation", "automation",
				trigger.getAutomation(), changedHandler, false);
		automation.setStartRow(false);
		automation.setWidth("*");
		automation.setDisabled(trigger.getRoutine()!=null);
		
		form2.setItems(automation);
		
		DynamicForm form1 = new DynamicForm();
		form1.setNumCols(2);
		form1.setTitleOrientation(TitleOrientation.TOP);
		form1.setValuesManager(vm);
		form1.setHeight100();

		SelectItem events = ItemFactory.newEventsSelector("events", I18N.message("triggeron"), changedHandler, true, true,
				true);
		events.setRowSpan(3);
		events.setValues(trigger.getEventsArray());

		SpacerItem spacer = new SpacerItem();
		spacer.setHeight(100);

		SelectItem routine = ItemFactory.newAutomationRoutineSelector("routine",
				trigger.getRoutine() != null ? trigger.getRoutine().getId() : null, changedHandler, true);
		routine.setEndRow(true);
		routine.addChangedHandler(changedHandler);
		ChangedHandler cngHandler = new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				if (event == null) {
					automation.setDisabled(false);
				} else {
					automation.setDisabled(event.getValue() != null && !event.getValue().toString().isEmpty());
				}
			}
		};
		routine.addChangedHandler(cngHandler);
		
		form1.setItems(events, folderSelector, routine, spacer);

		formsContainer.setMembers(form1, form2);
	}

	@SuppressWarnings("unchecked")
	boolean validate() {
		Map<String, Object> values = (Map<String, Object>) vm.getValues();
		vm.validate();
		if (!vm.hasErrors()) {
			try {
				if (values.get("routine") != null) {
					SelectItem item = (SelectItem) vm.getItem("routine");
					GUIAutomationRoutine routine = new GUIAutomationRoutine(
							Long.parseLong(values.get("routine").toString()));
					routine.setName(item.getSelectedRecord().getAttributeAsString("name"));
					trigger.setRoutine(routine);
				} else
					trigger.setRoutine(null);

				if (folderSelector.getFolderId() != null)
					trigger.setFolder(folderSelector.getFolder());
				else
					trigger.setFolder(null);

				trigger.setAutomation((String) values.get("automation"));

				String eventsStr = null;
				if (vm.getValueAsString("events") != null) {
					String buf = vm.getValueAsString("events").toString().trim().toLowerCase();
					buf = buf.replace('[', ' ');
					buf = buf.replace(']', ' ');
					eventsStr = buf.replace(" ", "");
				}

				trigger.setEvents(eventsStr);
			} catch (Throwable t) {
				SC.warn(t.getMessage());
			}
		}
		return !vm.hasErrors();
	}
}