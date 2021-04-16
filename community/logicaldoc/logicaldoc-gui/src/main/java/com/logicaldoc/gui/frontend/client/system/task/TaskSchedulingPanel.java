package com.logicaldoc.gui.frontend.client.system.task;

import java.util.LinkedHashMap;
import java.util.Map;

import com.logicaldoc.gui.common.client.beans.GUITask;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows the task scheduling settings.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class TaskSchedulingPanel extends VLayout {

	private ValuesManager vm = new ValuesManager();

	private DynamicForm form;

	private ChangedHandler changedHandler;

	private GUITask task;

	private boolean simplePolicy;

	private SpinnerItem maxDuration;

	private SpinnerItem initialDelay;

	private SpinnerItem repeatInterval;

	private TextItem seconds;

	private TextItem minutes;

	private TextItem hours;

	private TextItem dayMonth;

	private TextItem month;

	private TextItem dayWeek;

	public TaskSchedulingPanel(GUITask task, ChangedHandler changedHandler) {
		setWidth100();
		this.changedHandler = changedHandler;
		this.task = task;
		simplePolicy = task.getScheduling().isSimple();
		reloadForm();
	}

	private DynamicForm reloadForm() {
		if (form != null)
			removeMember(form);

		form = new DynamicForm();
		form.setValuesManager(vm);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(3);
		form.setColWidths(190, 200, 190);
		form.setWrapItemTitles(false);
		form.setWidth(700);

		// Policy
		final SelectItem simple = new SelectItem();
		LinkedHashMap<String, String> opts = new LinkedHashMap<String, String>();
		opts.put("true", I18N.message("simple"));
		opts.put("false", I18N.message("advanced"));
		simple.setValueMap(opts);
		simple.setName("simple");
		simple.setValue(simplePolicy ? "true" : "false");
		simple.setTitle(I18N.message("policy"));
		simple.setDefaultValue(Boolean.toString(task.getScheduling().isSimple()));
		simple.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				simplePolicy = "true".equals(simple.getValueAsString());
				changedHandler.onChanged(event);
				reloadForm();
			}
		});

		// Max Lengths
		long max = 0;
		if (task.getScheduling().getMaxLength() > 0)
			max = task.getScheduling().getMaxLength() / 60;
		maxDuration = ItemFactory.newSpinnerItem("maxDuration", "maxduration", max);
		maxDuration.setWidth(80);
		maxDuration.setHint(I18N.message("minutes").toLowerCase());
		maxDuration.setStep(10);
		maxDuration.setMin(0);
		maxDuration.addChangedHandler(changedHandler);

		// Initial delay
		initialDelay = ItemFactory.newSpinnerItem("initialDelay", "initialdelay", task.getScheduling().getDelay());
		initialDelay.setWidth(80);
		initialDelay.setVisible(simplePolicy);
		initialDelay.setStep(10);
		initialDelay.setMin(1);
		initialDelay.addChangedHandler(changedHandler);
		initialDelay.setHint(I18N.message("seconds").toLowerCase());
		initialDelay.setHintStyle("hint");
		initialDelay.setRequired(true);

		// Repeat interval
		repeatInterval = ItemFactory.newSpinnerItem("repeatInterval", "repeatinterval", task.getScheduling().getInterval());
		repeatInterval.setWidth(80);
		repeatInterval.setVisible(simplePolicy);
		repeatInterval.setStep(60);
		repeatInterval.setMin(1);
		repeatInterval.setVisible(simplePolicy);
		repeatInterval.addChangedHandler(changedHandler);
		repeatInterval.setHint(I18N.message("seconds").toLowerCase());
		repeatInterval.setHintStyle("hint");
		repeatInterval.setRequired(true);

		// Seconds
		seconds = ItemFactory.newTextItem("seconds", "seconds", null);
		seconds.setValue(task.getScheduling().getSeconds());
		seconds.setVisible(!simplePolicy);
		seconds.addChangedHandler(changedHandler);
		seconds.setRequired(true);
		seconds.setHint(I18N.message("schedulingsechint"));
		seconds.setHintStyle("hint");

		// Minutes
		minutes = ItemFactory.newTextItem("minutes", "minutes", null);
		minutes.setValue(task.getScheduling().getMinutes());
		minutes.setVisible(!simplePolicy);
		minutes.addChangedHandler(changedHandler);
		minutes.setRequired(true);
		minutes.setHint(I18N.message("schedulingsechint"));
		minutes.setHintStyle("hint");

		// Hours
		hours = ItemFactory.newTextItem("hours", "hours", null);
		hours.setValue(task.getScheduling().getHours());
		hours.setVisible(!simplePolicy);
		hours.addChangedHandler(changedHandler);
		hours.setRequired(true);
		hours.setHint(I18N.message("schedulinghourshint"));
		hours.setHintStyle("hint");

		// Day of month
		dayMonth = ItemFactory.newTextItem("dayMonth", "daymonth", null);
		dayMonth.setValue(task.getScheduling().getDayOfMonth());
		dayMonth.setVisible(!simplePolicy);
		dayMonth.addChangedHandler(changedHandler);
		dayMonth.setRequired(true);
		dayMonth.setHint(I18N.message("schedulingdaymonthhint"));
		dayMonth.setHintStyle("hint");

		// Month
		month = ItemFactory.newTextItem("month", "month", null);
		month.setValue(task.getScheduling().getMonth());
		month.setVisible(!simplePolicy);
		month.addChangedHandler(changedHandler);
		month.setRequired(true);
		month.setHint(I18N.message("schedulingmonthhint"));
		month.setHintStyle("hint");

		// Day of week
		dayWeek = ItemFactory.newTextItem("dayWeek", "dayweek", null);
		dayWeek.setValue(task.getScheduling().getDayOfWeek());
		dayWeek.setVisible(!simplePolicy);
		dayWeek.addChangedHandler(changedHandler);
		dayWeek.setRequired(true);
		dayWeek.setHint(I18N.message("schedulingdayweekhint"));
		dayWeek.setHintStyle("hint");

		form.setItems(simple, initialDelay, repeatInterval, seconds, minutes, hours, dayMonth, month, dayWeek,
				maxDuration);

		IButton restoreDefaults = new IButton();
		restoreDefaults.setTitle(I18N.message("restoredefaults"));
		restoreDefaults.setWidth(150);
		restoreDefaults.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				if (vm.validate()) {
					TaskSchedulingPanel.this.maxDuration.setValue("30");
					TaskSchedulingPanel.this.initialDelay.setValue(1800);
					TaskSchedulingPanel.this.repeatInterval.setValue(1800);
					TaskSchedulingPanel.this.seconds.setValue("0");
					TaskSchedulingPanel.this.minutes.setValue("10");
					TaskSchedulingPanel.this.hours.setValue("4/4");
					TaskSchedulingPanel.this.dayMonth.setValue("*");
					TaskSchedulingPanel.this.month.setValue("*");
					TaskSchedulingPanel.this.dayWeek.setValue("?");
				}
			}
		});

		setMembers(form, restoreDefaults);
		setMembersMargin(10);

		return form;
	}

	@SuppressWarnings("unchecked")
	boolean validate() {
		try {
			Map<String, Object> values = (Map<String, Object>) vm.getValues();
			vm.validate();
			if (!vm.hasErrors()) {
				if (((String) values.get("simple")).equals("true"))
					task.getScheduling().setSimple(true);
				else
					task.getScheduling().setSimple(false);

				long max = Long.parseLong(values.get("maxDuration").toString());
				if (max <= 0)
					max = -1L;
				else
					max = max * 60L;
				task.getScheduling().setMaxLength(max);

				if (task.getScheduling().isSimple() || ((String) values.get("simple")).equals("true")) {
					long longValue = 0;
					if (values.get("initialDelay") instanceof String)
						longValue = Long.parseLong((String) values.get("initialDelay"));
					else
						longValue = ((Integer) values.get("initialDelay")).longValue();
					task.getScheduling().setDelay(longValue);

					if (values.get("repeatInterval") instanceof String)
						longValue = Long.parseLong((String) values.get("repeatInterval"));
					else
						longValue = ((Integer) values.get("repeatInterval")).longValue();
					task.getScheduling().setInterval(longValue);
				} else {
					task.getScheduling().setSeconds((String) values.get("seconds"));
					task.getScheduling().setMinutes((String) values.get("minutes"));
					task.getScheduling().setHours((String) values.get("hours"));
					task.getScheduling().setDayOfMonth((String) values.get("dayMonth"));
					task.getScheduling().setMonth((String) values.get("month"));
					task.getScheduling().setDayOfWeek((String) values.get("dayWeek"));
				}
			}
		} catch (Throwable t) {
			SC.warn(t.getMessage());
		}
		return !vm.hasErrors();
	}
}
