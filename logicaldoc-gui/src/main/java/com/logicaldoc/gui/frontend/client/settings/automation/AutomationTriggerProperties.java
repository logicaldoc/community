package com.logicaldoc.gui.frontend.client.settings.automation;

import java.util.Date;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.logicaldoc.gui.common.client.beans.GUIAutomationRoutine;
import com.logicaldoc.gui.common.client.beans.GUIAutomationTrigger;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.EventSelectorOptions;
import com.logicaldoc.gui.common.client.util.ItemFactory;
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

	private static final String EVENTS = "events";

	private static final String ROUTINE = "routine";

	private static final String FOLDER = "folder";

	private HLayout formsContainer = new HLayout();

	private FolderSelector folderSelector;

	private ValuesManager vm = new ValuesManager();

	public AutomationTriggerProperties(GUIAutomationTrigger trigger, final ChangedHandler changedHandler) {
		super(trigger, changedHandler);
		setWidth100();
		setHeight100();

		setMembers(formsContainer);
		folderSelector = new FolderSelector(FOLDER, null);
		folderSelector.setWidth(200);
		folderSelector.setEndRow(true);
		folderSelector.setTitle(I18N.message(FOLDER));
		if (trigger.getFolder() != null)
			folderSelector.setFolder(trigger.getFolder());
		folderSelector.addFolderChangeListener((GUIFolder folder) -> changedHandler.onChanged(null));

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

		final TextAreaItem automation = ItemFactory.newTextAreaItemForAutomation("automation", trigger.getAutomation(),
				changedHandler, false);
		automation.setStartRow(false);
		automation.setWidth("*");
		automation.setDisabled(trigger.getRoutine() != null);

		DynamicForm form1 = new DynamicForm();
		form1.setNumCols(5);
		form1.setTitleOrientation(TitleOrientation.TOP);
		form1.setValuesManager(vm);
		form1.setHeight100();

		SelectItem events = prepareEventsSelector();

		folderSelector.setDisabled(events.isDisabled());

		DateItem date = prepareDateItem();

		TimeItem time = prepareTimeItem();

		TextItem cron = prepareCronItem();

		SpacerItem spacer = new SpacerItem();
		spacer.setHeight(100);

		SelectItem routine = prepareRoutineSelector(automation);

		CustomValidator atLeastOneValidator = new CustomValidator() {

			@Override
			protected boolean condition(Object value) {
				return !((automation.getValue() == null || automation.getValue().toString().isEmpty())
						&& routine.getValue() == null);
			}
		};
		atLeastOneValidator.setErrorMessage(I18N.message("automtriggervalidationmessage"));
		routine.setValidators(atLeastOneValidator);
		automation.setValidators(atLeastOneValidator);

		form1.setItems(events, folderSelector, new SpacerItem(), date, time, cron);
		form2.setItems(routine, automation);
		formsContainer.setMembers(form1, form2);
	}

	private SelectItem prepareRoutineSelector(final TextAreaItem automation) {
		SelectItem routine = ItemFactory.newAutomationRoutineSelector(ROUTINE,
				trigger.getRoutine() != null ? trigger.getRoutine().getId() : null, changedHandler, true);
		routine.setEndRow(true);
		routine.addChangedHandler(changedHandler);
		ChangedHandler cngHandler = (ChangedEvent event) -> {
			if (event == null) {
				automation.setDisabled(false);
			} else {
				automation.setDisabled(event.getValue() != null && !event.getValue().toString().isEmpty());
			}
		};
		routine.addChangedHandler(cngHandler);
		return routine;
	}

	private SelectItem prepareEventsSelector() {
		SelectItem events = ItemFactory.newEventsSelector(EVENTS, I18N.message("triggeronevents"), event -> {
			SelectItem item = (SelectItem) vm.getItem(EVENTS);
			if (item.getValues() != null && item.getValues().length > 0) {
				vm.getItem("date").setDisabled(true);
				vm.getItem("date").clearValue();
				vm.getItem("time").setDisabled(true);
				vm.getItem("time").clearValue();
				vm.getItem("cron").setDisabled(true);
				vm.getItem("cron").clearValue();
			} else {
				vm.getItem(FOLDER).setDisabled(false);
				vm.getItem("date").setDisabled(false);
				vm.getItem("time").setDisabled(false);
				vm.getItem("cron").setDisabled(false);
			}

			if (changedHandler != null)
				changedHandler.onChanged(event);
		}, new EventSelectorOptions(true, true, true, true, true, true, true, false));
		events.setRowSpan(2);
		events.setColSpan(4);
		events.setValues(trigger.getEventsArray());
		events.setDisabled(trigger.getDate() != null || (trigger.getCron() != null && !trigger.getCron().isEmpty()));
		return events;
	}

	private TextItem prepareCronItem() {
		TextItem cronItem = ItemFactory.newCronExpressionItem("cron", I18N.message("triggeroncron"), trigger.getCron(),
				(ChangedEvent event) -> {
					FormItem cron = vm.getItem("cron");
					if (cron.getValue() != null) {
						vm.getItem(EVENTS).setDisabled(true);
						vm.getItem(EVENTS).clearValue();
						vm.getItem(FOLDER).setDisabled(true);
						vm.getItem(FOLDER).clearValue();
						vm.getItem("date").setDisabled(true);
						vm.getItem("date").clearValue();
						vm.getItem("time").setDisabled(true);
						vm.getItem("time").clearValue();
					} else {
						vm.getItem(EVENTS).setDisabled(false);
						vm.getItem(FOLDER).setDisabled(false);
						vm.getItem("date").setDisabled(false);
						vm.getItem("time").setDisabled(false);
					}

					if (changedHandler != null)
						changedHandler.onChanged(event);
				});
		cronItem.setColSpan(5);
		cronItem.setDisabled(
				(trigger.getEvents() != null && !trigger.getEvents().isEmpty()) || trigger.getDate() != null);
		return cronItem;
	}

	private TimeItem prepareTimeItem() {
		TimeItem timeItem = ItemFactory.newTimeItem("time", "time");
		timeItem.setColSpan(4);
		if (trigger.getDate() != null) {
			DateTimeFormat df = DateTimeFormat.getFormat("HH:mm");
			timeItem.setValue(df.format(trigger.getDate()));
		}
		timeItem.addChangedHandler((ChangedEvent event) -> {
			FormItem time = vm.getItem("time");
			FormItem date = vm.getItem("date");
			if (time.getValue() != null || date.getValue() != null) {
				vm.getItem(EVENTS).setDisabled(true);
				vm.getItem(EVENTS).clearValue();
				vm.getItem(FOLDER).setDisabled(true);
				vm.getItem(FOLDER).clearValue();
				vm.getItem("cron").setDisabled(true);
				vm.getItem("cron").clearValue();
			} else {
				vm.getItem(EVENTS).setDisabled(false);
				vm.getItem(FOLDER).setDisabled(true);
				vm.getItem("cron").setDisabled(false);
			}

			if (changedHandler != null)
				changedHandler.onChanged(event);
		});
		timeItem.setDisabled((trigger.getEvents() != null && !trigger.getEvents().isEmpty())
				|| (trigger.getCron() != null && !trigger.getCron().isEmpty()));
		return timeItem;
	}

	private DateItem prepareDateItem() {
		DateItem dateItem = ItemFactory.newDateItem("date", I18N.message("triggerondate"));
		dateItem.setValue(trigger.getDate());
		dateItem.addChangedHandler((ChangedEvent event) -> {
			FormItem time = vm.getItem("time");
			FormItem date = vm.getItem("date");
			if (time.getValue() != null || date.getValue() != null) {
				vm.getItem(EVENTS).setDisabled(true);
				vm.getItem(EVENTS).clearValue();
				vm.getItem(FOLDER).setDisabled(true);
				vm.getItem(FOLDER).clearValue();
				vm.getItem("cron").setDisabled(true);
				vm.getItem("cron").clearValue();
			} else {
				vm.getItem(EVENTS).setDisabled(false);
				vm.getItem(FOLDER).setDisabled(false);
				vm.getItem("cron").setDisabled(false);
			}

			if (changedHandler != null)
				changedHandler.onChanged(event);
		});
		dateItem.setDisabled((trigger.getEvents() != null && !trigger.getEvents().isEmpty())
				|| (trigger.getCron() != null && !trigger.getCron().isEmpty()));
		return dateItem;
	}

	boolean validate() {
		if (Boolean.FALSE.equals(vm.validate()))
			return false;

		if (vm.getValue(ROUTINE) != null) {
			SelectItem item = (SelectItem) vm.getItem(ROUTINE);
			GUIAutomationRoutine routine = new GUIAutomationRoutine(Long.parseLong(vm.getValueAsString(ROUTINE)));
			routine.setName(item.getSelectedRecord().getAttributeAsString("name"));
			trigger.setRoutine(routine);
		} else
			trigger.setRoutine(null);

		if (folderSelector.getFolderId() != null)
			trigger.setFolder(folderSelector.getFolder());
		else
			trigger.setFolder(null);

		trigger.setAutomation(vm.getValueAsString("automation"));

		String eventsStr = null;
		if (vm.getValueAsString(EVENTS) != null && !vm.getValueAsString(EVENTS).isEmpty()) {
			String buf = vm.getValueAsString(EVENTS).trim().toLowerCase();
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
					trigger.setDate(df.parse(str + " " + vm.getValueAsString("time")));
				} catch (Exception t) {
					trigger.setDate(df.parse(str + " " + dfTime.format((Date) vm.getValue("time"))));
				}
			else
				trigger.setDate(df.parse(str));
		} else
			trigger.setDate(null);

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

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}