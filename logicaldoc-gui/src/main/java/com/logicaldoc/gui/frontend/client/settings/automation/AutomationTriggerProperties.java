package com.logicaldoc.gui.frontend.client.settings.automation;

import java.util.Date;
import java.util.Map;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.logicaldoc.gui.common.client.beans.GUIAutomationRoutine;
import com.logicaldoc.gui.common.client.beans.GUIAutomationTrigger;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.FolderChangeListener;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpacerItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.TimeItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.validator.CustomValidator;
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
		automation.setDisabled(trigger.getRoutine() != null);

		DynamicForm form1 = new DynamicForm();
		form1.setNumCols(5);
		form1.setTitleOrientation(TitleOrientation.TOP);
		form1.setValuesManager(vm);
		form1.setHeight100();

		SelectItem events = ItemFactory.newEventsSelector("events", I18N.message("triggeronevents"),
				new ChangedHandler() {

					@Override
					public void onChanged(ChangedEvent event) {
						SelectItem item = (SelectItem) vm.getItem("events");
						if (item.getValues() != null && item.getValues().length > 0) {
							vm.getItem("date").setDisabled(true);
							vm.getItem("date").clearValue();
							vm.getItem("time").setDisabled(true);
							vm.getItem("time").clearValue();
							vm.getItem("cron").setDisabled(true);
							vm.getItem("cron").clearValue();
						} else {
							vm.getItem("folder").setDisabled(false);
							vm.getItem("date").setDisabled(false);
							vm.getItem("time").setDisabled(false);
							vm.getItem("cron").setDisabled(false);
						}

						if (changedHandler != null)
							changedHandler.onChanged(event);
					}
				}, true, true, true, true);
		events.setRowSpan(2);
		events.setColSpan(4);
		events.setValues(trigger.getEventsArray());
		events.setDisabled(trigger.getDate() != null || (trigger.getCron() != null && !trigger.getCron().isEmpty()));

		folderSelector.setDisabled(events.isDisabled());
		
		DateItem date = ItemFactory.newDateItem("date", I18N.message("triggerondate"));
		date.setValue(trigger.getDate());
		date.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				FormItem time = (FormItem) vm.getItem("time");
				FormItem date = (FormItem) vm.getItem("date");
				if (time.getValue() != null || date.getValue() != null) {
					vm.getItem("events").setDisabled(true);
					vm.getItem("events").clearValue();
					vm.getItem("folder").setDisabled(true);
					vm.getItem("folder").clearValue();
					vm.getItem("cron").setDisabled(true);
					vm.getItem("cron").clearValue();
				} else {
					vm.getItem("events").setDisabled(false);
					vm.getItem("folder").setDisabled(false);
					vm.getItem("cron").setDisabled(false);
				}

				if (changedHandler != null)
					changedHandler.onChanged(event);
			}
		});
		date.setDisabled((trigger.getEvents() != null && !trigger.getEvents().isEmpty())
				|| (trigger.getCron() != null && !trigger.getCron().isEmpty()));

		TimeItem time = ItemFactory.newTimeItem("time", "time");
		time.setColSpan(4);
		if (trigger.getDate() != null) {
			DateTimeFormat df = DateTimeFormat.getFormat("HH:mm");
			time.setValue(df.format(trigger.getDate()));
		}
		time.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				FormItem time = (FormItem) vm.getItem("time");
				FormItem date = (FormItem) vm.getItem("date");
				if (time.getValue() != null || date.getValue() != null) {
					vm.getItem("events").setDisabled(true);
					vm.getItem("events").clearValue();
					vm.getItem("folder").setDisabled(true);
					vm.getItem("folder").clearValue();
					vm.getItem("cron").setDisabled(true);
					vm.getItem("cron").clearValue();
				} else {
					vm.getItem("events").setDisabled(false);
					vm.getItem("folder").setDisabled(true);
					vm.getItem("cron").setDisabled(false);
				}

				if (changedHandler != null)
					changedHandler.onChanged(event);
			}
		});
		time.setDisabled((trigger.getEvents() != null && !trigger.getEvents().isEmpty())
				|| (trigger.getCron() != null && !trigger.getCron().isEmpty()));

		TextItem cron = ItemFactory.newCronExpressionItem("cron", I18N.message("triggeroncron"), trigger.getCron(),
				new ChangedHandler() {

					@Override
					public void onChanged(ChangedEvent event) {
						FormItem cron = (FormItem) vm.getItem("cron");
						if (cron.getValue() != null) {
							vm.getItem("events").setDisabled(true);
							vm.getItem("events").clearValue();
							vm.getItem("folder").setDisabled(true);
							vm.getItem("folder").clearValue();
							vm.getItem("date").setDisabled(true);
							vm.getItem("date").clearValue();
							vm.getItem("time").setDisabled(true);
							vm.getItem("time").clearValue();
						} else {
							vm.getItem("events").setDisabled(false);
							vm.getItem("folder").setDisabled(false);
							vm.getItem("date").setDisabled(false);
							vm.getItem("time").setDisabled(false);
						}

						if (changedHandler != null)
							changedHandler.onChanged(event);
					}
				});
		cron.setColSpan(5);
		cron.setDisabled((trigger.getEvents() != null && !trigger.getEvents().isEmpty()) || trigger.getDate() != null);

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

		CustomValidator atLeastOneValidator = new CustomValidator() {

			@Override
			protected boolean condition(Object value) {
				if ((automation.getValue() == null || automation.getValue().toString().isEmpty())
						&& routine.getValue() == null)
					return false;
				return true;
			}
		};
		atLeastOneValidator.setErrorMessage(I18N.message("automtriggervalidationmessage"));
		routine.setValidators(atLeastOneValidator);
		automation.setValidators(atLeastOneValidator);

		form1.setItems(events, folderSelector, new SpacerItem(), date, time, cron);
		form2.setItems(routine, automation);
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
				if (vm.getValueAsString("events") != null && !vm.getValueAsString("events").isEmpty()) {
					String buf = vm.getValueAsString("events").toString().trim().toLowerCase();
					buf = buf.replace('[', ' ');
					buf = buf.replace(']', ' ');
					eventsStr = buf.replace(" ", "");
				}
				trigger.setEvents(eventsStr);

				trigger.setCron(vm.getValueAsString("cron"));

				DateTimeFormat dfDate = DateTimeFormat.getFormat("yyyy-MM-dd");
				DateTimeFormat dfTime = DateTimeFormat.getFormat("HH:mm");
				DateTimeFormat df = DateTimeFormat.getFormat("yyyy-MM-dd HH:mm");

				if (vm.getValue("date") != null) {
					String str = dfDate.format((Date) vm.getValue("date"));
					if (vm.getValue("time") != null)
						try {
							trigger.setDate(df.parse(str + " " + vm.getValue("time").toString()));
						} catch (Throwable t) {
							trigger.setDate(df.parse(str + " " + dfTime.format((Date) vm.getValue("time"))));
						}
					else
						trigger.setDate(df.parse(str));
				} else
					trigger.setDate(null);
			} catch (Throwable t) {
				GuiLog.error(t.getMessage(), null, t);
			}
		}
		return !vm.hasErrors();
	}

	/**
	 * Reacts to an error in the cron expression
	 * 
	 * @param errorMessage the error message
	 */
	public void invalidCronExpression(String errorMessage) {
		vm.getItem("cron").setErrors(errorMessage);
		vm.showErrors();
	}
}