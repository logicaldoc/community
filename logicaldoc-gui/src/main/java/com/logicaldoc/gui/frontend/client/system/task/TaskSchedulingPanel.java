package com.logicaldoc.gui.frontend.client.system.task;

import java.util.LinkedHashMap;

import com.logicaldoc.gui.common.client.beans.GUITask;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows the task scheduling settings.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class TaskSchedulingPanel extends VLayout {

	private static final String REPEAT_INTERVAL = "repeatinterval";

	private static final String INITIALDELAY_STR = "initialdelay";

	private static final String SIMPLE = "simple";

	private ValuesManager vm = new ValuesManager();

	private DynamicForm form;

	private ChangedHandler changedHandler;

	private GUITask task;

	private boolean simplePolicy;

	private SpinnerItem maxDuration;

	private SpinnerItem initialDelay;

	private SpinnerItem repeatInterval;

	private TextItem cronExpression;

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
		LinkedHashMap<String, String> opts = new LinkedHashMap<>();
		opts.put("true", I18N.message(SIMPLE));
		opts.put("false", I18N.message("advanced"));
		simple.setValueMap(opts);
		simple.setName(SIMPLE);
		simple.setValue(simplePolicy ? "true" : "false");
		simple.setTitle(I18N.message("policy"));
		simple.setDefaultValue(Boolean.toString(task.getScheduling().isSimple()));
		simple.addChangedHandler(event -> {
			simplePolicy = Boolean.valueOf(simple.getValueAsString());
			changedHandler.onChanged(event);
			reloadForm();
		});

		// Max Lengths
		long max = 0;
		if (task.getScheduling().getMaxLength() > 0)
			max = task.getScheduling().getMaxLength() / 60;
		maxDuration = ItemFactory.newSpinnerItem("maxduration", max);
		maxDuration.setWidth(80);
		maxDuration.setHint(I18N.message("minutes").toLowerCase());
		maxDuration.setStep(10);
		maxDuration.setMin(0);
		maxDuration.addChangedHandler(changedHandler);

		// Initial delay
		initialDelay = ItemFactory.newSpinnerItem(INITIALDELAY_STR, task.getScheduling().getDelay());
		initialDelay.setWidth(80);
		initialDelay.setVisible(simplePolicy);
		initialDelay.setStep(10);
		initialDelay.setMin(1);
		initialDelay.addChangedHandler(changedHandler);
		initialDelay.setHint(I18N.message("seconds").toLowerCase());
		initialDelay.setHintStyle("hint");
		initialDelay.setRequired(true);

		// Repeat interval
		repeatInterval = ItemFactory.newSpinnerItem(REPEAT_INTERVAL, task.getScheduling().getInterval());
		repeatInterval.setWidth(80);
		repeatInterval.setVisible(simplePolicy);
		repeatInterval.setStep(60);
		repeatInterval.setMin(1);
		repeatInterval.setVisible(simplePolicy);
		repeatInterval.addChangedHandler(changedHandler);
		repeatInterval.setHint(I18N.message("seconds").toLowerCase());
		repeatInterval.setHintStyle("hint");
		repeatInterval.setRequired(true);

		// Cron Expression
		cronExpression = ItemFactory.newCronExpressionItem("cron", "cronexpression",
				task.getScheduling().getCronExpression(), changedHandler);
		cronExpression.setVisible(!simplePolicy);
		cronExpression.setRequired(true);
		cronExpression.setEndRow(true);

		form.setItems(simple, initialDelay, repeatInterval, cronExpression, maxDuration);

		IButton restoreDefaults = new IButton();
		restoreDefaults.setTitle(I18N.message("restoredefaults"));
		restoreDefaults.setWidth(150);
		restoreDefaults.addClickHandler(event -> {
			if (Boolean.TRUE.equals(vm.validate())) {
				TaskSchedulingPanel.this.maxDuration.setValue("30");
				TaskSchedulingPanel.this.initialDelay.setValue(1800);
				TaskSchedulingPanel.this.repeatInterval.setValue(1800);
				TaskSchedulingPanel.this.cronExpression.setValue("* 10 * * * ? *");
			}
		});

		setMembers(form, restoreDefaults);
		setMembersMargin(10);

		return form;
	}

	boolean validate() {
		if (Boolean.FALSE.equals(vm.validate()))
			return false;

		task.getScheduling().setSimple(Boolean.valueOf(vm.getValueAsString(SIMPLE)));

		long max = Long.parseLong(vm.getValueAsString("maxduration"));
		if (max <= 0)
			max = -1L;
		else
			max = max * 60L;
		task.getScheduling().setMaxLength(max);

		if (task.getScheduling().isSimple() || Boolean.parseBoolean(vm.getValueAsString(SIMPLE))) {
			long longValue = 0;
			if (vm.getValue(INITIALDELAY_STR) instanceof String str)
				longValue = Long.parseLong(str);
			else
				longValue = ((Integer) vm.getValue(INITIALDELAY_STR)).longValue();
			task.getScheduling().setDelay(longValue);

			if (vm.getValue(REPEAT_INTERVAL) instanceof String str)
				longValue = Long.parseLong(str);
			else
				longValue = ((Integer) vm.getValue(REPEAT_INTERVAL)).longValue();
			task.getScheduling().setInterval(longValue);
		} else {
			task.getScheduling().setCronExpression(vm.getValueAsString("cron"));
		}

		return !vm.hasErrors();
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
