package com.logicaldoc.gui.frontend.client.settings.protocols;

import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIHistory;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.frontend.client.services.SystemService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Displays the API Calls with search form
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.4
 *
 */
public class ApiCallsPanel extends VLayout {

	private static final String VALUE = "value";

	private static final String DISPLAY_MAX = "displaymax";

	private static final String USERNAME = "username";

	private static final String PAYLOAD = "payload";

	private static final String GEOLOCATION = "geolocation";

	private static final String DEVICE = "device";

	private static final String TENANT = "tenant";

	private static final String USER_ID = "userId";

	private static final String TILL_DATE = "tillDate";

	private static final String FROM_DATE = "fromDate";

	private static final String PROTOCOL = "protocol";

	private Layout search = new VLayout();

	private Layout results = new VLayout();

	private ValuesManager vm = new ValuesManager();

	private ListGrid calls;

	private InfoPanel infoPanel;

	@Override
	public void onDraw() {
		SelectItem userSelector = ItemFactory.newUserSelector("user", "user", null, false, false);

		// Protocol
		SelectItem proto = ItemFactory.newSelectItem(PROTOCOL, PROTOCOL);
		LinkedHashMap<String, String> map = new LinkedHashMap<>();
		map.put("soap", "soap");
		map.put("rest", "rest");
		proto.setWidth(120);
		proto.setValueMap(map);
		proto.setAllowEmptyValue(true);

		// Session ID
		TextItem sessionId = ItemFactory.newTextItem("sid", null);
		sessionId.setWidth(250);
		sessionId.setEndRow(true);

		// From
		DateItem fromDate = ItemFactory.newDateItem(FROM_DATE, "from");

		// To
		DateItem tillDate = ItemFactory.newDateItem(TILL_DATE, "till");
		tillDate.setEndRow(true);

		// Max results
		SpinnerItem displayMax = ItemFactory.newSpinnerItem(DISPLAY_MAX, 100, 5, null);
		displayMax.setHint(I18N.message("elements"));
		displayMax.setStep(10);
		displayMax.setEndRow(true);

		DynamicForm form = new DynamicForm();
		form.setWidth(1);
		form.setValuesManager(vm);
		form.setAlign(Alignment.LEFT);
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setNumCols(6);
		form.setWrapItemTitles(false);
		form.setItems(userSelector, sessionId, fromDate, tillDate, proto, displayMax);

		IButton searchButton = new IButton(I18N.message("search"));
		searchButton.setAutoFit(true);
		searchButton.addClickHandler((ClickEvent event) -> onSearch());

		IButton resetButton = new IButton(I18N.message("reset"));
		resetButton.addClickHandler((ClickEvent event) -> vm.clearValues());
		resetButton.setAutoFit(true);

		IButton print = new IButton(I18N.message("print"));
		print.addClickHandler((ClickEvent event) -> GridUtil.print(calls));
		print.setAutoFit(true);

		IButton export = new IButton(I18N.message("export"));
		export.addClickHandler((ClickEvent event) -> GridUtil.exportCSV(calls, false));
		if (!Feature.enabled(Feature.EXPORT_CSV)) {
			export.setDisabled(true);
			export.setTooltip(I18N.message("featuredisabled"));
		}
		export.setAutoFit(true);

		HLayout buttons = new HLayout(10);
		buttons.setMembers(searchButton, resetButton, print, export);

		VLayout formLayout = new VLayout(10);
		formLayout.setMembers(form, buttons);
		formLayout.setAutoHeight();

		search.setMembersMargin(10);
		search.setMembers(formLayout);
		search.setShowResizeBar(true);
		search.setWidth100();
		search.setAutoHeight();
		search.setMargin(10);

		ListGridField uri = new ListGridField("uri", I18N.message("uri"));
		uri.setWidth("*");
		uri.setCanFilter(true);

		ListGridField date = new DateListGridField("date", "date", DateCellFormatter.FORMAT_LONG);

		ListGridField userField = new UserListGridField("user", USER_ID, "user");
		userField.setCanFilter(true);
		userField.setAlign(Alignment.CENTER);

		ListGridField protocol = new ListGridField(PROTOCOL, I18N.message(PROTOCOL));
		protocol.setCanFilter(true);
		protocol.setAutoFitWidth(true);
		protocol.setAlign(Alignment.CENTER);
		protocol.setMinWidth(70);

		ListGridField sid = new ListGridField("sid", I18N.message("sid"), 250);
		sid.setCanFilter(true);
		sid.setHidden(true);
		sid.setAlign(Alignment.CENTER);

		ListGridField key = new ListGridField("key", I18N.message("key"), 90);
		key.setCanFilter(true);
		key.setHidden(true);
		key.setAlign(Alignment.CENTER);

		ListGridField tenant = new ListGridField(TENANT, I18N.message(TENANT));
		tenant.setCanFilter(true);
		tenant.setHidden(true);
		tenant.setAutoFitWidth(true);

		ListGridField userId = new ListGridField(USER_ID, I18N.message("userid"), 100);
		userId.setCanFilter(true);
		userId.setHidden(true);

		ListGridField ip = new ListGridField("ip", I18N.message("ip"), 100);
		ip.setCanFilter(true);

		ListGridField device = new ListGridField(DEVICE, I18N.message(DEVICE), 200);
		device.setHidden(true);
		ListGridField geolocation = new ListGridField(GEOLOCATION, I18N.message(GEOLOCATION), 200);
		geolocation.setHidden(true);

		ListGridField username = new ListGridField(USERNAME, I18N.message(USERNAME), 100);
		username.setCanFilter(true);
		username.setHidden(true);

		ListGridField payload = new ListGridField(PAYLOAD, I18N.message(PAYLOAD));
		payload.setCanFilter(true);
		payload.setWidth("*");
		payload.setHidden(true);
		payload.setEscapeHTML(true);

		calls = new ListGrid();
		calls.setEmptyMessage(I18N.message("notitemstoshow"));
		calls.setWidth100();
		calls.setHeight100();
		calls.setFields(date, protocol, ip, userField, uri, tenant, sid, key, userId, username, device, geolocation,
				payload);
		calls.setSelectionType(SelectionStyle.SINGLE);
		calls.setShowRecordComponents(true);
		calls.setShowRecordComponentsByCell(true);
		calls.setCanFreezeFields(true);
		calls.setFilterOnKeypress(true);
		calls.setAutoFetchData(true);
		calls.sort("date", SortDirection.DESCENDING);

		results.addMember(calls);

		addMember(search, 0);

		// Prepare a panel containing a title and the documents list
		infoPanel = new InfoPanel("");
		addMember(infoPanel, 1);

		addMember(results, 2);

		calls.addCellDoubleClickHandler(event -> {
			String value = event.getRecord().getAttributeAsString(calls.getField(event.getColNum()).getName());
			TextAreaItem item = ItemFactory.newTextAreaItem(VALUE, value);
			item.setWidth(500);
			item.setHeight(400);
			LD.askForValue(I18N.message(VALUE), I18N.message(VALUE), value, item, 500, val -> {
				// Nothing to do
			});
		});
	}

	/**
	 * Gets the option items to choose events types
	 * 
	 * @return an array of select items
	 */
	public SelectItem[] getEventTypes() {
		List<SelectItem> items = new ArrayList<>();

		return items.toArray(new SelectItem[0]);
	}

	@SuppressWarnings("unchecked")
	private void onSearch() {
		calls.setData();

		final Map<String, Object> values = vm.getValues();

		if (Boolean.FALSE.equals(vm.validate()))
			return;

		Long userId = getUserId(values);

		Date fromValue = null;
		if (values.get(FROM_DATE) != null)
			fromValue = (Date) values.get(FROM_DATE);

		Date tillValue = null;
		if (values.get(TILL_DATE) != null)
			tillValue = (Date) values.get(TILL_DATE);

		String sid = null;
		if (values.get("sid") != null)
			sid = (String) values.get("sid");

		String protocol = null;
		if (values.get(PROTOCOL) != null)
			protocol = (String) values.get(PROTOCOL);

		String uri = null;
		if (values.get("uri") != null)
			uri = (String) values.get("uri");

		int displayMaxValue = getDisplayMaxValue(values);

		doSearch(userId, fromValue, tillValue, sid, protocol, uri, displayMaxValue);
	}

	private int getDisplayMaxValue(final Map<String, Object> values) {
		int displayMaxValue = 0;
		if (values.get(DISPLAY_MAX) != null) {
			if (values.get(DISPLAY_MAX) instanceof Integer)
				displayMaxValue = (Integer) values.get(DISPLAY_MAX);
			else
				displayMaxValue = Integer.parseInt((String) values.get(DISPLAY_MAX));
		}
		return displayMaxValue;
	}

	private Long getUserId(final Map<String, Object> values) {
		Long userId = null;
		if (values.get("user") != null) {
			if (values.get("user") instanceof Long)
				userId = (Long) values.get("user");
			else
				userId = Long.parseLong(values.get("user").toString());
		}
		return userId;
	}

	private void doSearch(Long userId, Date fromValue, Date tillValue, String sid, String protocol, String uri,
			int displayMaxValue) {
		LD.contactingServer();
		SystemService.Instance.get().searchApiCalls(userId, fromValue, tillValue, sid, protocol, uri, displayMaxValue,
				new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						LD.clearPrompt();
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(List<GUIHistory> histories) {
						LD.clearPrompt();

						if (!histories.isEmpty()) {
							List<ListGridRecord> records = new ArrayList<>();
							for (GUIHistory history : histories) {
								ListGridRecord rec = new ListGridRecord();
								rec.setAttribute("date", history.getDate());
								rec.setAttribute("user", history.getUsername());
								rec.setAttribute("sid", history.getSessionId());
								rec.setAttribute("key", history.getKeyLabel());
								rec.setAttribute(USER_ID, history.getUserId());
								rec.setAttribute("ip", history.getIp());
								rec.setAttribute(DEVICE, history.getDevice());
								rec.setAttribute(GEOLOCATION, history.getGeolocation());
								rec.setAttribute(USERNAME, history.getUserLogin());
								rec.setAttribute(PAYLOAD, history.getComment());
								rec.setAttribute("uri", history.getPath());
								rec.setAttribute(PROTOCOL, history.getProtocol());
								rec.setAttribute(TENANT, history.getTenant());
								records.add(rec);
							}
							calls.setData(records.toArray(new ListGridRecord[0]));
						}
						removeMember(infoPanel);
						infoPanel = new InfoPanel("");
						infoPanel.setMessage(I18N.message("showelements", Integer.toString(calls.getTotalRows())));
						addMember(infoPanel, 1);
					}
				});
	}
}
