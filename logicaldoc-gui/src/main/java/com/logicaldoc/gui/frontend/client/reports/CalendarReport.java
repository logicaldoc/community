package com.logicaldoc.gui.frontend.client.reports;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUICalendarEvent;
import com.logicaldoc.gui.common.client.beans.GUICalendarEventSearchCriteria;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.CalendarService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ExpansionMode;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HStack;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel is used to show a report of a selection of events.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.7
 */
public class CalendarReport extends AdminPanel {

	private static final String DESCRIPTION = "description";

	private static final String ATTENDEES = "attendees";

	private static final String STATUS = "status";

	private static final String FREQUENCY = "frequency";

	private static final String DISPLAYMAX = "displaymax";

	private static final String SUBTYPE = "subtype";

	private static final String TITLE = "title";

	private static final String END_TO = "endTo";

	private static final String END_FROM = "endFrom";

	private static final String TO_DATE = "toDate";

	private static final String FROM_DATE = "fromDate";

	private Layout search = new VLayout();

	private Layout results = new VLayout();

	private VLayout layout = new VLayout();

	private ValuesManager vm = new ValuesManager();

	private ListGrid list;

	private InfoPanel infoPanel;

	public CalendarReport() {
		super("calendar");
	}

	@Override
	public void onDraw() {
		HStack formsLayout = new HStack(10);

		DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setAlign(Alignment.LEFT);
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setNumCols(4);
		form.setWrapItemTitles(false);

		// From
		DateItem fromDate = ItemFactory.newDateItem(FROM_DATE, "from");

		// To
		DateItem toDate = ItemFactory.newDateItem(TO_DATE, "till");

		// End date From
		DateItem deadLineFrom = ItemFactory.newDateItem(END_FROM, "endsfrom");

		// End date To
		DateItem deadLineTo = ItemFactory.newDateItem(END_TO, "till");

		SelectItem frequencySelector = ItemFactory.newFrequencySelector();

		TextItem title = ItemFactory.newTextItem(TITLE, null);

		TextItem type = ItemFactory.newTextItem("type", null);

		TextItem subtype = ItemFactory.newTextItem(SUBTYPE, null);

		SelectItem statusSelector = ItemFactory.newCalendarEventStatusSelector();

		// Max results
		SpinnerItem displayMax = ItemFactory.newSpinnerItem(DISPLAYMAX, (Integer) null);
		displayMax.setValue(100);
		displayMax.setDefaultValue(100);
		displayMax.setStep(10);
		displayMax.setHint(I18N.message("elements"));

		ButtonItem searchButton = new ButtonItem();
		searchButton.setTitle(I18N.message("search"));
		searchButton.setAutoFit(true);
		searchButton.setEndRow(true);
		searchButton.setColSpan(2);
		searchButton.addClickHandler(event -> onSearch());

		form.setItems(fromDate, toDate, deadLineFrom, deadLineTo, title, statusSelector, type, subtype,
				frequencySelector, displayMax, searchButton);

		formsLayout.addMember(form);
		formsLayout.setMembersMargin(80);

		search.setMembersMargin(10);
		search.setMembers(formsLayout);
		search.setHeight("30%");
		search.setShowResizeBar(true);
		search.setWidth100();
		search.setMargin(10);

		ListGridField titleCol = new ListGridField(TITLE, I18N.message(TITLE));
		titleCol.setWidth("*");
		titleCol.setCanFilter(true);

		ListGridField typeCol = new ListGridField("type", I18N.message("type"));
		typeCol.setWidth(100);
		typeCol.setCanFilter(true);

		ListGridField subtypeCol = new ListGridField(SUBTYPE, I18N.message(SUBTYPE));
		subtypeCol.setWidth(100);
		subtypeCol.setCanFilter(true);

		ListGridField date = new DateListGridField("date", "date", DateCellFormatter.FORMAT_LONG);

		ListGridField endDate = new DateListGridField("endDate", "enddate");

		ListGridField frequency = new ListGridField(FREQUENCY, I18N.message(FREQUENCY), 90);
		frequency.setType(ListGridFieldType.INTEGER);
		frequency.setAlign(Alignment.CENTER);
		frequency.setCanFilter(false);
		frequency.setCellFormatter((value, rec, rowNum, colNum) -> {
			String v = value.toString();

			if ("1".equals(v)) {
				return I18N.message("daily");
			} else if ("7".equals(v)) {
				return I18N.message("weekly");
			} else if ("15".equals(v)) {
				return I18N.message("biweekly");
			} else if ("30".equals(v)) {
				return I18N.message("monthly");
			} else if ("180".equals(v)) {
				return I18N.message("sixmonthly");
			} else if ("365".equals(v)) {
				return I18N.message("yearly");
			}

			return null;
		});

		ListGridField status = new ListGridField(STATUS, I18N.message(STATUS), 90);
		status.setType(ListGridFieldType.INTEGER);
		status.setAlign(Alignment.CENTER);
		status.setCanFilter(false);
		status.setCellFormatter((value, rec, rowNum, colNum) -> {
			String v = value.toString();

			if ("1".equals(v)) {
				return I18N.message("working");
			} else if ("2".equals(v)) {
				return I18N.message("completed");
			} else if ("3".equals(v)) {
				return I18N.message("canceled");
			}

			return null;
		});

		ListGridField description = new ListGridField(DESCRIPTION, I18N.message(DESCRIPTION));
		description.setWidth(400);
		description.setHidden(true);

		ListGridField participants = new ListGridField(ATTENDEES, I18N.message(ATTENDEES));
		participants.setWidth(300);

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setWidth100();
		list.setHeight100();
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setFilterOnKeypress(true);
		list.setAutoFetchData(true);
		list.setFields(date, endDate, titleCol, typeCol, subtypeCol, status, frequency, participants, description);
		list.setDetailField(DESCRIPTION);
		list.setCanExpandRecords(true);
		list.setExpansionMode(ExpansionMode.DETAIL_FIELD);

		results.addMember(list);
		layout.addMember(search, 0);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();

		ToolStripButton print = new ToolStripButton();
		print.setIcon(ItemFactory.newImgIcon("printer.png").getSrc());
		print.setTooltip(I18N.message("print"));
		print.setAutoFit(true);
		print.addClickHandler(event -> GridUtil.print(list));
		toolStrip.addButton(print);

		if (Feature.visible(Feature.EXPORT_CSV)) {
			toolStrip.addSeparator();
			ToolStripButton export = new ToolStripButton();
			export.setIcon(ItemFactory.newImgIcon("table_row_insert.png").getSrc());
			export.setTooltip(I18N.message("export"));
			export.setAutoFit(true);
			toolStrip.addButton(export);
			export.addClickHandler(event -> GridUtil.exportCSV(list, true));
			if (!Feature.enabled(Feature.EXPORT_CSV)) {
				export.setDisabled(true);
				export.setTooltip(I18N.message("featuredisabled"));
			}
		}

		toolStrip.addFill();
		layout.addMember(toolStrip, 1);

		// Prepare a panel containing the events list
		infoPanel = new InfoPanel("");
		layout.addMember(infoPanel, 2);

		layout.addMember(results, 3);

		body.setMembers(layout);
	}

	@SuppressWarnings("unchecked")
	private void onSearch() {
		list.setData();

		final Map<String, Object> values = vm.getValues();

		if (Boolean.FALSE.equals(vm.validate()))
			return;

		Date fromValue = null;
		if (values.get(FROM_DATE) != null)
			fromValue = (Date) values.get(FROM_DATE);
		Date toValue = null;
		if (values.get(TO_DATE) != null)
			toValue = (Date) values.get(TO_DATE);
		Date endDateFrom = null;
		if (values.get(END_FROM) != null)
			endDateFrom = (Date) values.get(END_FROM);
		Date endDateTo = null;
		if (values.get(END_TO) != null)
			endDateTo = (Date) values.get(END_TO);

		Integer frequencyValue = null;
		if (values.get(FREQUENCY) != null)
			frequencyValue = Integer.parseInt(values.get(FREQUENCY).toString());

		Integer statusValue = null;
		if (values.get(STATUS) != null)
			statusValue = Integer.parseInt(values.get(STATUS).toString());

		String titleValue = values.get(TITLE) != null ? values.get(TITLE).toString() : null;

		String typeValue = values.get("type") != null ? values.get("type").toString() : null;

		String subtypeValue = values.get(SUBTYPE) != null ? values.get(SUBTYPE).toString() : null;

		int maxRecords = getMaxRecords(values);

		GUICalendarEventSearchCriteria criteria = new GUICalendarEventSearchCriteria(fromValue, toValue, endDateFrom,
				endDateTo, frequencyValue);
		criteria.setStatus(statusValue);
		criteria.setTitle(titleValue);
		criteria.setType(typeValue);
		criteria.setSubtype(subtypeValue);
		criteria.setMaxRecords(maxRecords);

		doSearch(criteria);
	}

	private int getMaxRecords(final Map<String, Object> values) {
		int maxRecords = 0;
		if (values.get(DISPLAYMAX) != null) {
			if (values.get(DISPLAYMAX) instanceof Integer intValue)
				maxRecords = intValue;
			else
				maxRecords = Integer.parseInt((String) values.get(DISPLAYMAX));
		}
		return maxRecords;
	}

	private void doSearch(GUICalendarEventSearchCriteria criteria) {
		CalendarService.Instance.get().find(criteria, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(List<GUICalendarEvent> result) {
				List<ListGridRecord> records = new ArrayList<>();
				for (GUICalendarEvent event : result) {
					ListGridRecord rec = new ListGridRecord();
					rec.setAttribute("date", event.getStart());
					rec.setAttribute(TITLE, event.getTitle());
					rec.setAttribute("type", event.getType());
					rec.setAttribute(SUBTYPE, event.getSubType());
					rec.setAttribute(FREQUENCY, event.getFrequency());
					rec.setAttribute(STATUS, event.getStatus());
					rec.setAttribute(DESCRIPTION, event.getDescription());
					rec.setAttribute("endDate", event.getDeadline());
					rec.setAttribute(ATTENDEES,
							event.getAttendees().stream().map(Object::toString).collect(Collectors.joining(", ")));
					records.add(rec);
				}
				list.setData(records.toArray(new ListGridRecord[0]));

				layout.removeMember(infoPanel);
				infoPanel = new InfoPanel("");
				infoPanel.setMessage(I18N.message("showelements", Integer.toString(list.getTotalRows())));
				layout.addMember(infoPanel, 2);
			}
		});
	}
}