package com.logicaldoc.gui.common.client.widgets;

import java.util.Date;
import java.util.LinkedHashMap;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
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

	private static final String YEARLY_DAY = "yearly-day";

	private static final String YEARLY_MONTH = "yearly-month";

	private static final String MONTHLY_DAY = "monthly-day";

	private static final String EVERYDAY = "everyday";

	private static final String STARTSAT = "startsat";

	private static final String DEFAULT_CRON_END_EXPRESSION = " 1/1 * ? *";

	private static final String YEARLY_DAY_NAME_MONTH = "yearly-day-name-month";

	private static final String YEARLY_DAY_NAME = "yearly-day-name";

	private static final String YEARLY_DAY_POSITION = "yearly-day-position";

	private static final String MONTHLY_DAY_MONTHS = "monthly-day-months";

	private static final String MONTHLY_DAY_NAME_MONTHS = "monthly-day-name-months";

	private static final String MONTHLY_DAY_NAME = "monthly-day-name";

	private static final String MONTHLY_DAY_POSITION = "monthly-day-position";

	private static final String SUNDAY = "sunday";

	private static final String SATURDAY = "saturday";

	private static final String FRIDAY = "friday";

	private static final String THURSDAY = "thursday";

	private static final String WEDNESDAY = "wednesday";

	private static final String TUESDAY = "tuesday";

	private static final String MONDAY = "monday";

	private static final String EVERY = "every";

	private static final String HOURLY_TIME = "hourly-time";

	private static final String GENERATE = "generate";

	private static final String EXPRESSION = "expression";

	private static final String DESCRIPTION = "description";

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
		setHeight(400);

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

		StaticTextItem expression = ItemFactory.newStaticTextItem(EXPRESSION, null);
		expression.setWidth(300);
		expression.setWrap(false);

		StaticTextItem description = ItemFactory.newStaticTextItem(DESCRIPTION, null);
		description.setWidth(400);
		description.setWrap(false);

		DynamicForm expressionForm = new DynamicForm();
		expressionForm.setValuesManager(vm);
		expressionForm.setNumCols(1);
		expressionForm.setWidth(1);
		expressionForm.setItems(new SpacerItem(), expression, description, new SpacerItem());

		ToolStripButton save = new ToolStripButton(I18N.message("save"));
		save.addClickHandler((com.smartgwt.client.widgets.events.ClickEvent event) -> {
			if (sourceItem != null) {
				sourceItem.clearErrors();
				sourceItem.setValue(vm.getItem(EXPRESSION).getValue());
				if (changedHandler != null)
					changedHandler.onChanged(null);
				destroy();
			}
		});
		save.setDisabled(sourceItem == null || sourceItem.isDisabled());

		ToolStripButton close = new ToolStripButton(I18N.message("close"));
		close.addClickHandler((com.smartgwt.client.widgets.events.ClickEvent event) -> destroy());

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
		SelectItem minute = ItemFactory.newSelectItem("minutes-minute", EVERY);
		minute.setWidth(50);
		minute.setHint(I18N.message("minutes").toLowerCase());
		minute.setValueMap("1", "2", "3", "4", "5", "6", "10", "15", "20", "30");
		minute.setValue("1");

		ButtonItem generate = new ButtonItem(I18N.message(GENERATE));
		generate.setStartRow(true);
		generate.addClickHandler((ClickEvent event) -> onGenerate());

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

		SelectItem hour = ItemFactory.newSelectItem("hourly-hour", EVERY);
		hour.setWidth(50);
		hour.setHint(I18N.message("hours").toLowerCase());
		hour.setValueMap("", "1", "2", "3", "4", "6", "12");
		hour.setValue("1");
		hour.setDefaultValue("1");
		hour.addChangedHandler((ChangedEvent event) -> form.getItem(HOURLY_TIME)
				.setDisabled(event.getValue() != null && !"".equals(event.getValue())));

		TimeItem startsAt = ItemFactory.newTimeItemPicklist(HOURLY_TIME, STARTSAT);
		startsAt.setWrapTitle(false);
		startsAt.setDisabled(true);
		startsAt.setValue(new Date());

		ButtonItem generate = new ButtonItem(I18N.message(GENERATE));
		generate.setStartRow(true);
		generate.addClickHandler((ClickEvent event) -> onGenerate());

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

		LinkedHashMap<String, String> map = new LinkedHashMap<>();
		map.put(EVERYDAY, I18N.message(EVERYDAY));
		map.put("everyweekday", I18N.message("everyweekday"));
		frequency.setValueMap(map);
		frequency.setValue(EVERYDAY);

		TimeItem startsAt = ItemFactory.newTimeItemPicklist("daily-time", STARTSAT);
		startsAt.setWrapTitle(false);
		startsAt.setValue(new Date());

		ButtonItem generate = new ButtonItem(I18N.message(GENERATE));
		generate.setStartRow(true);
		generate.addClickHandler((ClickEvent event) -> onGenerate());

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

		LinkedHashMap<String, String> map = new LinkedHashMap<>();
		map.put("MON", I18N.message(MONDAY));
		map.put("TUE", I18N.message(TUESDAY));
		map.put("WED", I18N.message(WEDNESDAY));
		map.put("THU", I18N.message(THURSDAY));
		map.put("FRI", I18N.message(FRIDAY));
		map.put("SAT", I18N.message(SATURDAY));
		map.put("SUN", I18N.message(SUNDAY));

		SelectItem days = ItemFactory.newSelectItem("weekly-days", "ddays");
		days.setWidth(200);
		days.setMultiple(true);
		days.setValueMap(map);
		days.setRequired(true);
		days.setValue("MON");

		TimeItem startsAt = ItemFactory.newTimeItemPicklist("weekly-time", STARTSAT);
		startsAt.setWrapTitle(false);
		startsAt.setValue(new Date());

		ButtonItem generate = new ButtonItem(I18N.message(GENERATE));
		generate.setStartRow(true);
		generate.addClickHandler((ClickEvent event) -> onGenerate());

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

		SelectItem day = ItemFactory.newSelectItem(MONTHLY_DAY, "day");
		day.setWidth(50);
		day.setValue("1");
		day.setDefaultValue("1");
		day.setValueMap("", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17",
				"18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31");
		day.addChangedHandler((ChangedEvent event) -> {
			form.getItem(MONTHLY_DAY_POSITION).setDisabled(event.getValue() != null && !"".equals(event.getValue()));
			form.getItem(MONTHLY_DAY_NAME).setDisabled(event.getValue() != null && !"".equals(event.getValue()));
			form.getItem(MONTHLY_DAY_NAME_MONTHS).setDisabled(event.getValue() != null && !"".equals(event.getValue()));

			if (event.getValue() != null && !"".equals(event.getValue()))
				form.getItem(MONTHLY_DAY_POSITION).setValue("");
		});

		SelectItem dayMonths = ItemFactory.newSelectItem(MONTHLY_DAY_MONTHS, "ofevery");
		dayMonths.setHint(I18N.message("months"));
		dayMonths.setWidth(50);
		dayMonths.setEndRow(true);
		dayMonths.setColSpan(3);
		dayMonths.setValueMap("1", "2", "3", "4", "6");
		dayMonths.setValue("1");
		dayMonths.setDefaultValue("1");

		LinkedHashMap<String, String> dayPositionMap = new LinkedHashMap<>();
		dayPositionMap.put("", "");
		dayPositionMap.put("1", I18N.message("first"));
		dayPositionMap.put("2", I18N.message("second"));
		dayPositionMap.put("3", I18N.message("third"));
		dayPositionMap.put("4", I18N.message("fourth"));
		SelectItem dayPosition = ItemFactory.newSelectItem(MONTHLY_DAY_POSITION, "the");
		dayPosition.setWidth(70);
		dayPosition.setDisabled(true);
		dayPosition.setValueMap(dayPositionMap);
		dayPosition.addChangedHandler((ChangedEvent event) -> {
			form.getItem(MONTHLY_DAY).setDisabled(event.getValue() != null && !"".equals(event.getValue()));
			form.getItem(MONTHLY_DAY_MONTHS).setDisabled(event.getValue() != null && !"".equals(event.getValue()));

			if (event.getValue() != null && !"".equals(event.getValue()))
				form.getItem(MONTHLY_DAY_MONTHS).setValue("");
		});

		LinkedHashMap<String, String> dayMap = new LinkedHashMap<>();
		dayMap.put("MON", I18N.message(MONDAY));
		dayMap.put("TUE", I18N.message(TUESDAY));
		dayMap.put("WED", I18N.message(WEDNESDAY));
		dayMap.put("THU", I18N.message(THURSDAY));
		dayMap.put("FRI", I18N.message(FRIDAY));
		dayMap.put("SAT", I18N.message(SATURDAY));
		dayMap.put("SUN", I18N.message(SUNDAY));
		SelectItem dayName = ItemFactory.newSelectItem(MONTHLY_DAY_NAME, "dayname");
		dayName.setDisabled(true);
		dayName.setWidth(80);
		dayName.setShowTitle(false);
		dayName.setValueMap(dayMap);
		dayName.setValue("MON");

		SelectItem dayNameMonths = ItemFactory.newSelectItem(MONTHLY_DAY_NAME_MONTHS, "ofevery");
		dayNameMonths.setHint(I18N.message("months"));
		dayNameMonths.setWidth(50);
		dayNameMonths.setEndRow(true);
		dayNameMonths.setColSpan(2);
		dayNameMonths.setValueMap("1", "2", "3", "4", "6");
		dayNameMonths.setValue("1");
		dayNameMonths.setDefaultValue("1");
		dayNameMonths.setDisabled(true);

		TimeItem startsAt = ItemFactory.newTimeItemPicklist("monthly-time", STARTSAT);
		startsAt.setWrapTitle(false);
		startsAt.setColSpan(6);
		startsAt.setValue(new Date());

		ButtonItem generate = new ButtonItem(I18N.message(GENERATE));
		generate.setStartRow(true);
		generate.addClickHandler((ClickEvent event) -> onGenerate());

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

		LinkedHashMap<String, String> monthMap = new LinkedHashMap<>();
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

		SelectItem month = ItemFactory.newSelectItem(YEARLY_MONTH, EVERY);
		month.setWidth(80);
		month.setValueMap(monthMap);
		month.setValue("1");
		month.addChangedHandler((ChangedEvent event) -> {
			form.getItem(YEARLY_DAY_POSITION).setDisabled(event.getValue() != null && !"".equals(event.getValue()));
			form.getItem(YEARLY_DAY_NAME).setDisabled(event.getValue() != null && !"".equals(event.getValue()));
			form.getItem(YEARLY_DAY_NAME_MONTH).setDisabled(event.getValue() != null && !"".equals(event.getValue()));

			if (event.getValue() != null && !"".equals(event.getValue()))
				form.getItem(YEARLY_DAY_POSITION).setValue("");
		});

		SelectItem day = ItemFactory.newSelectItem(YEARLY_DAY, "day");
		day.setWidth(50);
		day.setValue("1");
		day.setDefaultValue("1");
		day.setEndRow(true);
		day.setValueMap("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17",
				"18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31");

		LinkedHashMap<String, String> dayPositionMap = new LinkedHashMap<>();
		dayPositionMap.put("", "");
		dayPositionMap.put("1", I18N.message("first"));
		dayPositionMap.put("2", I18N.message("second"));
		dayPositionMap.put("3", I18N.message("third"));
		dayPositionMap.put("4", I18N.message("fourth"));
		SelectItem dayPosition = ItemFactory.newSelectItem(YEARLY_DAY_POSITION, "the");
		dayPosition.setWidth(70);
		dayPosition.setDisabled(true);
		dayPosition.setValueMap(dayPositionMap);
		dayPosition.addChangedHandler((ChangedEvent event) -> {
			form.getItem(YEARLY_MONTH).setDisabled(event.getValue() != null && !"".equals(event.getValue()));
			form.getItem(YEARLY_DAY).setDisabled(event.getValue() != null && !"".equals(event.getValue()));

			if (event.getValue() != null && !"".equals(event.getValue()))
				form.getItem(YEARLY_MONTH).setValue("");
		});

		LinkedHashMap<String, String> dayMap = new LinkedHashMap<>();
		dayMap.put("MON", I18N.message(MONDAY));
		dayMap.put("TUE", I18N.message(TUESDAY));
		dayMap.put("WED", I18N.message(WEDNESDAY));
		dayMap.put("THU", I18N.message(THURSDAY));
		dayMap.put("FRI", I18N.message(FRIDAY));
		dayMap.put("SAT", I18N.message(SATURDAY));
		dayMap.put("SUN", I18N.message(SUNDAY));
		SelectItem dayName = ItemFactory.newSelectItem(YEARLY_DAY_NAME, "dayname");
		dayName.setDisabled(true);
		dayName.setWidth(80);
		dayName.setShowTitle(false);
		dayName.setValueMap(dayMap);
		dayName.setValue("MON");

		LinkedHashMap<String, String> monthMap2 = new LinkedHashMap<>();
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

		SelectItem dayNameMonth = ItemFactory.newSelectItem(YEARLY_DAY_NAME_MONTH, "of");
		dayNameMonth.setWidth(80);
		dayNameMonth.setEndRow(true);
		dayNameMonth.setValueMap(monthMap2);
		dayNameMonth.setDisabled(true);

		TimeItem startsAt = ItemFactory.newTimeItemPicklist("yearly-time", STARTSAT);
		startsAt.setWrapTitle(false);
		startsAt.setColSpan(6);
		startsAt.setValue(new Date());

		ButtonItem generate = new ButtonItem(I18N.message(GENERATE));
		generate.setStartRow(true);
		generate.addClickHandler((ClickEvent event) -> onGenerate());

		form.setFields(month, day, dayPosition, dayName, dayNameMonth, startsAt, new SpacerItem(), generate);

		Tab tab = new Tab(I18N.message("yearly"));
		tab.setPane(form);

		return tab;
	}

	private void onGenerate() {
		if (Boolean.FALSE.equals(vm.validate()))
			return;

		int selectedTab = topTabSet.getSelectedTabNumber();
		switch (selectedTab) {
		case 0:
			vm.getItem(EXPRESSION).setValue("0 0/" + vm.getValue("minutes-minute") + " * 1/1 * ? *");
			break;
		case 1:
			generateHourly();
			break;
		case 2: {
			generateDaily();
			break;
		}
		case 3: {
			generateWeekly();
			break;
		}
		case 4: {
			generateMonthly();
			break;
		}
		case 5: {
			generateYearly();
			break;
		}
		default:
			// do nothing
		}

		InfoService.Instance.get().getCronDescription("" + vm.getItem(EXPRESSION).getValue(), I18N.getLocale(),
				new DefaultAsyncCallback<>() {

					@Override
					public void handleSuccess(String description) {
						vm.getItem(DESCRIPTION).setValue(description);
					}
				});
	}

	private void generateYearly() {
		TimeItem timeItem = (TimeItem) vm.getItem("yearly-time");
		String m = timeItem.getMinuteItem().getValueAsString();
		if (m.length() > 1 && m.startsWith("0"))
			m = m.substring(1);
		String h = timeItem.getHourItem().getValueAsString();
		if (h.length() > 1 && h.startsWith("0"))
			h = h.substring(1);

		String month = vm.getValueAsString(YEARLY_MONTH);
		if (month != null && !"".equals(month)) {
			vm.getItem(EXPRESSION)
					.setValue("0 " + m + " " + h + " " + vm.getValueAsString(YEARLY_DAY) + " " + month + " ? *");
		} else
			vm.getItem(EXPRESSION)
					.setValue("0 " + m + " " + h + " ? " + vm.getValueAsString(YEARLY_DAY_NAME_MONTH) + " "
							+ vm.getValueAsString(YEARLY_DAY_NAME) + "#" + vm.getValueAsString(YEARLY_DAY_POSITION)
							+ " *");
	}

	private void generateMonthly() {
		TimeItem timeItem = (TimeItem) vm.getItem("monthly-time");
		String m = timeItem.getMinuteItem().getValueAsString();
		if (m.length() > 1 && m.startsWith("0"))
			m = m.substring(1);
		String h = timeItem.getHourItem().getValueAsString();
		if (h.length() > 1 && h.startsWith("0"))
			h = h.substring(1);

		String day = vm.getValueAsString(MONTHLY_DAY);
		if (day != null && !"".equals(day)) {
			vm.getItem(EXPRESSION).setValue(
					"0 " + m + " " + h + " " + day + " 1/" + vm.getValueAsString(MONTHLY_DAY_MONTHS) + " ? *");
		} else
			vm.getItem(EXPRESSION)
					.setValue("0 " + m + " " + h + " ? 1/" + vm.getValueAsString(MONTHLY_DAY_NAME_MONTHS) + " "
							+ vm.getValueAsString(MONTHLY_DAY_NAME) + "#" + vm.getValueAsString(MONTHLY_DAY_POSITION)
							+ " *");
	}

	private void generateWeekly() {
		TimeItem timeItem = (TimeItem) vm.getItem("weekly-time");
		String m = timeItem.getMinuteItem().getValueAsString();
		if (m.length() > 1 && m.startsWith("0"))
			m = m.substring(1);
		String h = timeItem.getHourItem().getValueAsString();
		if (h.length() > 1 && h.startsWith("0"))
			h = h.substring(1);
		vm.getItem(EXPRESSION).setValue("0 " + m + " " + h + " ? * " + vm.getValueAsString("weekly-days") + " *");
	}

	private void generateDaily() {
		TimeItem timeItem = (TimeItem) vm.getItem("daily-time");
		String m = timeItem.getMinuteItem().getValueAsString();
		if (m.length() > 1 && m.startsWith("0"))
			m = m.substring(1);
		String h = timeItem.getHourItem().getValueAsString();
		if (h.length() > 1 && h.startsWith("0"))
			h = h.substring(1);

		if (EVERYDAY.equals(vm.getValueAsString("daily-frequency")))
			vm.getItem(EXPRESSION).setValue("0 " + m + " " + h + DEFAULT_CRON_END_EXPRESSION);
		else {

			vm.getItem(EXPRESSION).setValue("0 " + m + " " + h + " ? * MON-FRI *");
		}
	}

	private void generateHourly() {
		String hour = vm.getValueAsString("hourly-hour");
		if (hour != null && !"".equals(hour))
			vm.getItem(EXPRESSION).setValue("0 0 0/" + hour + DEFAULT_CRON_END_EXPRESSION);
		else {
			TimeItem timeItem = (TimeItem) vm.getItem(HOURLY_TIME);
			String m = timeItem.getMinuteItem().getValueAsString();
			if (m.length() > 1 && m.startsWith("0"))
				m = m.substring(1);
			String h = timeItem.getHourItem().getValueAsString();
			if (h.length() > 1 && h.startsWith("0"))
				h = h.substring(1);
			vm.getItem(EXPRESSION).setValue("0 " + m + " " + h + DEFAULT_CRON_END_EXPRESSION);
		}
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((sourceItem == null) ? 0 : sourceItem.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		CronExpressionComposer other = (CronExpressionComposer) obj;
		if (sourceItem == null) {
			if (other.sourceItem != null)
				return false;
		} else if (!sourceItem.equals(other.sourceItem))
			return false;
		return true;
	}
}