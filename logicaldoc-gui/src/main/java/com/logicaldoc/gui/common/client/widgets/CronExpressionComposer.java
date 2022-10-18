package com.logicaldoc.gui.common.client.widgets;

import java.util.Date;
import java.util.LinkedHashMap;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.InfoService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.Side;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpacerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TimeItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * A visual editor for cron expressions
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.1
 */
public class CronExpressionComposer extends Window {

	private ValuesManager vm = new ValuesManager();

	private TabSet topTabSet = new TabSet();

	private FormItem sourceItem = null;

	private ChangedHandler changedHandler = null;

	public CronExpressionComposer(FormItem sourceItem, ChangedHandler changedHandler) {
		super();

		this.sourceItem = sourceItem;
		this.changedHandler = changedHandler;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("cronexpressioncomposer"));
		setWidth(650);
		setHeight(340);

		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		initGUI();
	}

	private void initGUI() {
		topTabSet.setTabBarPosition(Side.TOP);
		topTabSet.setTabBarAlign(Side.LEFT);

		Tab minutesTab = prepareMinutesTab();

		Tab hourlyTab = prepareHourlyTab();

		Tab dailyTab = prepareDailyTab();

		Tab weeklyTab = prepareWeeklyTab();

		Tab monthlyTab = prepareMonthlyTab();

		Tab yearlyTab = prepareYearlyTab();

		topTabSet.addTab(minutesTab);
		topTabSet.addTab(hourlyTab);
		topTabSet.addTab(dailyTab);
		topTabSet.addTab(weeklyTab);
		topTabSet.addTab(monthlyTab);
		topTabSet.addTab(yearlyTab);

		StaticTextItem expression = ItemFactory.newStaticTextItem("expression", "expression", null);
		expression.setWidth(300);
		expression.setWrap(false);

		StaticTextItem description = ItemFactory.newStaticTextItem("description", "description", null);
		description.setWidth(400);
		description.setWrap(false);

		DynamicForm expressionForm = new DynamicForm();
		expressionForm.setValuesManager(vm);
		expressionForm.setNumCols(1);
		expressionForm.setWidth(1);
		expressionForm.setItems(new SpacerItem(), expression, description, new SpacerItem());

		ToolStripButton save = new ToolStripButton(I18N.message("save"));
		save.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				if (sourceItem != null) {
					sourceItem.clearErrors();
					sourceItem.setValue(vm.getItem("expression").getValue());
					if (changedHandler != null)
						changedHandler.onChanged(null);
					destroy();
				}
			}
		});
		save.setDisabled(sourceItem == null || sourceItem.isDisabled());

		ToolStripButton close = new ToolStripButton(I18N.message("close"));
		close.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				destroy();
			}
		});

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setWidth100();
		toolStrip.addButton(save);
		toolStrip.addSeparator();
		toolStrip.addButton(close);

		addItem(topTabSet);
		addItem(expressionForm);
		addItem(toolStrip);
	}

	private Tab prepareMinutesTab() {
		SelectItem minute = ItemFactory.newSelectItem("minutes-minute", "every");
		minute.setWidth(50);
		minute.setHint(I18N.message("minutes").toLowerCase());
		minute.setValueMap("1", "2", "3", "4", "5", "6", "10", "15", "20", "30");
		minute.setValue("1");

		ButtonItem generate = new ButtonItem(I18N.message("generate"));
		generate.setStartRow(true);
		generate.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				onGenerate();
			}
		});

		DynamicForm form = new DynamicForm();
		form.setWidth(1);
		form.setValuesManager(vm);
		form.setFields(minute, new SpacerItem(), generate);

		Tab minutesTab = new Tab(I18N.message("minutes"));
		minutesTab.setPane(form);

		return minutesTab;
	}

	private Tab prepareHourlyTab() {
		DynamicForm form = new DynamicForm();
		form.setWidth(1);
		form.setNumCols(1);
		form.setValuesManager(vm);

		SelectItem hour = ItemFactory.newSelectItem("hourly-hour", "every");
		hour.setWidth(50);
		hour.setHint(I18N.message("hours").toLowerCase());
		hour.setValueMap("", "1", "2", "3", "4", "6", "12");
		hour.setValue("1");
		hour.setDefaultValue("1");
		hour.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				form.getItem("hourly-time").setDisabled(event.getValue() != null && !"".equals(event.getValue()));
			}
		});

		TimeItem startsAt = ItemFactory.newTimeItemPicklist("hourly-time", "startsat");
		startsAt.setWrapTitle(false);
		startsAt.setDisabled(true);
		startsAt.setValue(new Date());

		ButtonItem generate = new ButtonItem(I18N.message("generate"));
		generate.setStartRow(true);
		generate.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				onGenerate();
			}
		});

		form.setFields(hour, startsAt, new SpacerItem(), generate);

		Tab tab = new Tab(I18N.message("hourly"));
		tab.setPane(form);

		return tab;
	}

	private Tab prepareDailyTab() {
		DynamicForm form = new DynamicForm();
		form.setWidth(1);
		form.setNumCols(2);
		form.setValuesManager(vm);

		RadioGroupItem frequency = new RadioGroupItem("daily-frequency", " ");
		frequency.setWrap(false);
		frequency.setShowTitle(false);
		frequency.setColSpan(2);

		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put("everyday", I18N.message("everyday"));
		map.put("everyweekday", I18N.message("everyweekday"));
		frequency.setValueMap(map);
		frequency.setValue("everyday");

		TimeItem startsAt = ItemFactory.newTimeItemPicklist("daily-time", "startsat");
		startsAt.setWrapTitle(false);
		startsAt.setValue(new Date());

		ButtonItem generate = new ButtonItem(I18N.message("generate"));
		generate.setStartRow(true);
		generate.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				onGenerate();
			}
		});

		form.setFields(frequency, startsAt, new SpacerItem(), generate);

		Tab tab = new Tab(I18N.message("daily"));
		tab.setPane(form);

		return tab;
	}

	private Tab prepareWeeklyTab() {
		DynamicForm form = new DynamicForm();
		form.setWidth(1);
		form.setNumCols(1);
		form.setValuesManager(vm);

		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put("MON", I18N.message("monday"));
		map.put("TUE", I18N.message("tuesday"));
		map.put("WED", I18N.message("wednesday"));
		map.put("THU", I18N.message("thursday"));
		map.put("FRI", I18N.message("friday"));
		map.put("SAT", I18N.message("saturday"));
		map.put("SUN", I18N.message("sunday"));

		SelectItem days = ItemFactory.newSelectItem("weekly-days", "ddays");
		days.setWidth(200);
		days.setMultiple(true);
		days.setValueMap(map);
		days.setRequired(true);
		days.setValue("MON");

		TimeItem startsAt = ItemFactory.newTimeItemPicklist("weekly-time", "startsat");
		startsAt.setWrapTitle(false);
		startsAt.setValue(new Date());

		ButtonItem generate = new ButtonItem(I18N.message("generate"));
		generate.setStartRow(true);
		generate.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				onGenerate();
			}
		});

		form.setFields(days, startsAt, new SpacerItem(), generate);

		Tab tab = new Tab(I18N.message("weekly"));
		tab.setPane(form);

		return tab;
	}

	private Tab prepareMonthlyTab() {
		DynamicForm form = new DynamicForm();
		form.setWidth(1);
		form.setNumCols(6);
		form.setValuesManager(vm);

		SelectItem day = ItemFactory.newSelectItem("monthly-day", "day");
		day.setWidth(50);
		day.setValue("1");
		day.setDefaultValue("1");
		day.setValueMap("", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17",
				"18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31");
		day.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				form.getItem("monthly-day-position")
						.setDisabled(event.getValue() != null && !"".equals(event.getValue()));
				form.getItem("monthly-day-name").setDisabled(event.getValue() != null && !"".equals(event.getValue()));
				form.getItem("monthly-day-name-months")
						.setDisabled(event.getValue() != null && !"".equals(event.getValue()));

				if (event.getValue() != null && !"".equals(event.getValue()))
					form.getItem("monthly-day-position").setValue("");
			}
		});

		SelectItem dayMonths = ItemFactory.newSelectItem("monthly-day-months", "ofevery");
		dayMonths.setHint(I18N.message("months"));
		dayMonths.setWidth(50);
		dayMonths.setEndRow(true);
		dayMonths.setColSpan(3);
		dayMonths.setValueMap("1", "2", "3", "4", "6");
		dayMonths.setValue("1");
		dayMonths.setDefaultValue("1");

		LinkedHashMap<String, String> dayPositionMap = new LinkedHashMap<String, String>();
		dayPositionMap.put("", "");
		dayPositionMap.put("1", I18N.message("first"));
		dayPositionMap.put("2", I18N.message("second"));
		dayPositionMap.put("3", I18N.message("third"));
		dayPositionMap.put("4", I18N.message("fourth"));
		SelectItem dayPosition = ItemFactory.newSelectItem("monthly-day-position", "the");
		dayPosition.setWidth(70);
		dayPosition.setDisabled(true);
		dayPosition.setValueMap(dayPositionMap);
		dayPosition.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				form.getItem("monthly-day").setDisabled(event.getValue() != null && !"".equals(event.getValue()));
				form.getItem("monthly-day-months")
						.setDisabled(event.getValue() != null && !"".equals(event.getValue()));

				if (event.getValue() != null && !"".equals(event.getValue()))
					form.getItem("monthly-day-months").setValue("");
			}
		});

		LinkedHashMap<String, String> dayMap = new LinkedHashMap<String, String>();
		dayMap.put("MON", I18N.message("monday"));
		dayMap.put("TUE", I18N.message("tuesday"));
		dayMap.put("WED", I18N.message("wednesday"));
		dayMap.put("THU", I18N.message("thursday"));
		dayMap.put("FRI", I18N.message("friday"));
		dayMap.put("SAT", I18N.message("saturday"));
		dayMap.put("SUN", I18N.message("sunday"));
		SelectItem dayName = ItemFactory.newSelectItem("monthly-day-name", "dayname");
		dayName.setDisabled(true);
		dayName.setWidth(80);
		dayName.setShowTitle(false);
		dayName.setValueMap(dayMap);
		dayName.setValue("MON");

		SelectItem dayNameMonths = ItemFactory.newSelectItem("monthly-day-name-months", "ofevery");
		dayNameMonths.setHint(I18N.message("months"));
		dayNameMonths.setWidth(50);
		dayNameMonths.setEndRow(true);
		dayNameMonths.setColSpan(2);
		dayNameMonths.setValueMap("1", "2", "3", "4", "6");
		dayNameMonths.setValue("1");
		dayNameMonths.setDefaultValue("1");
		dayNameMonths.setDisabled(true);

		TimeItem startsAt = ItemFactory.newTimeItemPicklist("monthly-time", "startsat");
		startsAt.setWrapTitle(false);
		startsAt.setColSpan(6);
		startsAt.setValue(new Date());

		ButtonItem generate = new ButtonItem(I18N.message("generate"));
		generate.setStartRow(true);
		generate.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				onGenerate();
			}
		});

		form.setFields(day, dayMonths, dayPosition, dayName, dayNameMonths, startsAt, new SpacerItem(), generate);

		Tab tab = new Tab(I18N.message("monthly"));
		tab.setPane(form);

		return tab;
	}

	private Tab prepareYearlyTab() {
		DynamicForm form = new DynamicForm();
		form.setWidth(1);
		form.setNumCols(6);
		form.setValuesManager(vm);

		LinkedHashMap<String, String> monthMap = new LinkedHashMap<String, String>();
		monthMap.put("", "");
		monthMap.put("1", I18N.message("january"));
		monthMap.put("2", I18N.message("february"));
		monthMap.put("3", I18N.message("march"));
		monthMap.put("4", I18N.message("april"));
		monthMap.put("5", I18N.message("may"));
		monthMap.put("6", I18N.message("june"));
		monthMap.put("7", I18N.message("july"));
		monthMap.put("8", I18N.message("august"));
		monthMap.put("9", I18N.message("september"));
		monthMap.put("10", I18N.message("october"));
		monthMap.put("11", I18N.message("november"));
		monthMap.put("12", I18N.message("december"));

		SelectItem month = ItemFactory.newSelectItem("yearly-month", "every");
		month.setWidth(80);
		month.setValueMap(monthMap);
		month.setValue("1");
		month.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				form.getItem("yearly-day-position")
						.setDisabled(event.getValue() != null && !"".equals(event.getValue()));
				form.getItem("yearly-day-name").setDisabled(event.getValue() != null && !"".equals(event.getValue()));
				form.getItem("yearly-day-name-month")
						.setDisabled(event.getValue() != null && !"".equals(event.getValue()));

				if (event.getValue() != null && !"".equals(event.getValue()))
					form.getItem("yearly-day-position").setValue("");
			}
		});

		SelectItem day = ItemFactory.newSelectItem("yearly-day", "day");
		day.setWidth(50);
		day.setValue("1");
		day.setDefaultValue("1");
		day.setEndRow(true);
		day.setValueMap("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17",
				"18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31");

		LinkedHashMap<String, String> dayPositionMap = new LinkedHashMap<String, String>();
		dayPositionMap.put("", "");
		dayPositionMap.put("1", I18N.message("first"));
		dayPositionMap.put("2", I18N.message("second"));
		dayPositionMap.put("3", I18N.message("third"));
		dayPositionMap.put("4", I18N.message("fourth"));
		SelectItem dayPosition = ItemFactory.newSelectItem("yearly-day-position", "the");
		dayPosition.setWidth(70);
		dayPosition.setDisabled(true);
		dayPosition.setValueMap(dayPositionMap);
		dayPosition.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				form.getItem("yearly-month").setDisabled(event.getValue() != null && !"".equals(event.getValue()));
				form.getItem("yearly-day").setDisabled(event.getValue() != null && !"".equals(event.getValue()));

				if (event.getValue() != null && !"".equals(event.getValue()))
					form.getItem("yearly-month").setValue("");
			}
		});

		LinkedHashMap<String, String> dayMap = new LinkedHashMap<String, String>();
		dayMap.put("MON", I18N.message("monday"));
		dayMap.put("TUE", I18N.message("tuesday"));
		dayMap.put("WED", I18N.message("wednesday"));
		dayMap.put("THU", I18N.message("thursday"));
		dayMap.put("FRI", I18N.message("friday"));
		dayMap.put("SAT", I18N.message("saturday"));
		dayMap.put("SUN", I18N.message("sunday"));
		SelectItem dayName = ItemFactory.newSelectItem("yearly-day-name", "dayname");
		dayName.setDisabled(true);
		dayName.setWidth(80);
		dayName.setShowTitle(false);
		dayName.setValueMap(dayMap);
		dayName.setValue("MON");

		LinkedHashMap<String, String> monthMap2 = new LinkedHashMap<String, String>();
		monthMap2.put("1", I18N.message("january"));
		monthMap2.put("2", I18N.message("february"));
		monthMap2.put("3", I18N.message("march"));
		monthMap2.put("4", I18N.message("april"));
		monthMap2.put("5", I18N.message("may"));
		monthMap2.put("6", I18N.message("june"));
		monthMap2.put("7", I18N.message("july"));
		monthMap2.put("8", I18N.message("august"));
		monthMap2.put("9", I18N.message("september"));
		monthMap2.put("10", I18N.message("october"));
		monthMap2.put("11", I18N.message("november"));
		monthMap2.put("12", I18N.message("december"));

		SelectItem dayNameMonth = ItemFactory.newSelectItem("yearly-day-name-month", "of");
		dayNameMonth.setWidth(80);
		dayNameMonth.setEndRow(true);
		dayNameMonth.setValueMap(monthMap2);
		dayNameMonth.setDisabled(true);

		TimeItem startsAt = ItemFactory.newTimeItemPicklist("yearly-time", "startsat");
		startsAt.setWrapTitle(false);
		startsAt.setColSpan(6);
		startsAt.setValue(new Date());

		ButtonItem generate = new ButtonItem(I18N.message("generate"));
		generate.setStartRow(true);
		generate.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				onGenerate();
			}
		});

		form.setFields(month, day, dayPosition, dayName, dayNameMonth, startsAt, new SpacerItem(), generate);

		Tab tab = new Tab(I18N.message("yearly"));
		tab.setPane(form);

		return tab;
	}

	private void onGenerate() {
		if (!vm.validate())
			return;

		int selectedTab = topTabSet.getSelectedTabNumber();
		switch (selectedTab) {
		case 0:
			vm.getItem("expression").setValue("0 0/" + vm.getValue("minutes-minute") + " * 1/1 * ? *");
			break;
		case 1:
			String hour = vm.getValueAsString("hourly-hour");
			if (hour != null && !"".equals(hour))
				vm.getItem("expression").setValue("0 0 0/" + hour + " 1/1 * ? *");
			else {
				TimeItem timeItem = (TimeItem) vm.getItem("hourly-time");
				String m = timeItem.getMinuteItem().getValueAsString();
				if (m.length() > 1 && m.startsWith("0"))
					m = m.substring(1);
				String h = timeItem.getHourItem().getValueAsString();
				if (h.length() > 1 && h.startsWith("0"))
					h = h.substring(1);
				vm.getItem("expression").setValue("0 " + m + " " + h + " 1/1 * ? *");
			}
			break;
		case 2: {
			TimeItem timeItem = (TimeItem) vm.getItem("daily-time");
			String m = timeItem.getMinuteItem().getValueAsString();
			if (m.length() > 1 && m.startsWith("0"))
				m = m.substring(1);
			String h = timeItem.getHourItem().getValueAsString();
			if (h.length() > 1 && h.startsWith("0"))
				h = h.substring(1);

			if ("everyday".equals(vm.getValueAsString("daily-frequency")))
				vm.getItem("expression").setValue("0 " + m + " " + h + " 1/1 * ? *");
			else {

				vm.getItem("expression").setValue("0 " + m + " " + h + " ? * MON-FRI *");
			}
			break;
		}
		case 3: {
			TimeItem timeItem = (TimeItem) vm.getItem("weekly-time");
			String m = timeItem.getMinuteItem().getValueAsString();
			if (m.length() > 1 && m.startsWith("0"))
				m = m.substring(1);
			String h = timeItem.getHourItem().getValueAsString();
			if (h.length() > 1 && h.startsWith("0"))
				h = h.substring(1);
			vm.getItem("expression").setValue("0 " + m + " " + h + " ? * " + vm.getValueAsString("weekly-days") + " *");
			break;
		}
		case 4: {
			TimeItem timeItem = (TimeItem) vm.getItem("monthly-time");
			String m = timeItem.getMinuteItem().getValueAsString();
			if (m.length() > 1 && m.startsWith("0"))
				m = m.substring(1);
			String h = timeItem.getHourItem().getValueAsString();
			if (h.length() > 1 && h.startsWith("0"))
				h = h.substring(1);

			String day = vm.getValueAsString("monthly-day");
			if (day != null && !"".equals(day)) {
				vm.getItem("expression").setValue(
						"0 " + m + " " + h + " " + day + " 1/" + vm.getValueAsString("monthly-day-months") + " ? *");
			} else
				vm.getItem("expression")
						.setValue("0 " + m + " " + h + " ? 1/" + vm.getValueAsString("monthly-day-name-months") + " "
								+ vm.getValueAsString("monthly-day-name") + "#"
								+ vm.getValueAsString("monthly-day-position") + " *");
			break;
		}
		case 5: {
			TimeItem timeItem = (TimeItem) vm.getItem("yearly-time");
			String m = timeItem.getMinuteItem().getValueAsString();
			if (m.length() > 1 && m.startsWith("0"))
				m = m.substring(1);
			String h = timeItem.getHourItem().getValueAsString();
			if (h.length() > 1 && h.startsWith("0"))
				h = h.substring(1);

			String month = vm.getValueAsString("yearly-month");
			if (month != null && !"".equals(month)) {
				vm.getItem("expression")
						.setValue("0 " + m + " " + h + " " + vm.getValueAsString("yearly-day") + " " + month + " ? *");
			} else
				vm.getItem("expression")
						.setValue("0 " + m + " " + h + " ? " + vm.getValueAsString("yearly-day-name-month") + " "
								+ vm.getValueAsString("yearly-day-name") + "#"
								+ vm.getValueAsString("yearly-day-position") + " *");
			break;
		}
		default:
			// do nothing
		}

		InfoService.Instance.get().getCronDescription("" + vm.getItem("expression").getValue(), I18N.getLocale(),
				new AsyncCallback<String>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(String description) {
						vm.getItem("description").setValue(description);
					}
				});
	}
}