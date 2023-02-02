package com.logicaldoc.gui.frontend.client.reports;

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
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.SystemService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
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
import com.smartgwt.client.widgets.grid.events.CellDoubleClickEvent;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel is used to show the API calls.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class ApiCallsReport extends AdminPanel {

	private Layout search = new VLayout();

	private Layout results = new VLayout();

	private VLayout callsLayout = new VLayout();

	private ValuesManager vm = new ValuesManager();

	private ListGrid calls;

	private InfoPanel infoPanel;

	public ApiCallsReport() {
		super("apicalls");
	}

	@Override
	public void onDraw() {
		SelectItem userSelector = ItemFactory.newUserSelector("user", "user", null, false, false);

		// Protocol
		SelectItem proto = ItemFactory.newSelectItem("protocol", "protocol");
		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
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
		DateItem fromDate = ItemFactory.newDateItem("fromDate", "from");

		// To
		DateItem tillDate = ItemFactory.newDateItem("tillDate", "till");
		tillDate.setEndRow(true);

		// Max results
		SpinnerItem displayMax = ItemFactory.newSpinnerItem("displayMax", "displaymax", 100, 5, null);
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
		searchButton.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				onSearch();
			}
		});

		IButton resetButton = new IButton(I18N.message("reset"));
		resetButton.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				vm.clearValues();
			}
		});
		resetButton.setAutoFit(true);

		IButton print = new IButton(I18N.message("print"));
		print.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				GridUtil.print(calls);
			}
		});
		print.setAutoFit(true);

		IButton export = new IButton(I18N.message("export"));
		export.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				GridUtil.exportCSV(calls, false);
			}
		});
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

		ListGridField userField = new UserListGridField("user", "userId", "user");
		userField.setCanFilter(true);
		userField.setAlign(Alignment.CENTER);

		ListGridField protocol = new ListGridField("protocol", I18N.message("protocol"));
		protocol.setCanFilter(true);
		protocol.setAutoFitWidth(true);
		protocol.setAlign(Alignment.CENTER);
		protocol.setMinWidth(70);

		ListGridField sid = new ListGridField("sid", I18N.message("sid"), 250);
		sid.setCanFilter(true);
		sid.setHidden(true);
		sid.setAlign(Alignment.CENTER);

		ListGridField tenant = new ListGridField("tenant", I18N.message("tenant"));
		tenant.setCanFilter(true);
		tenant.setHidden(true);
		tenant.setAutoFitWidth(true);

		ListGridField userId = new ListGridField("userId", I18N.message("userid"), 100);
		userId.setCanFilter(true);
		userId.setHidden(true);

		ListGridField ip = new ListGridField("ip", I18N.message("ip"), 100);
		ip.setCanFilter(true);

		ListGridField device = new ListGridField("device", I18N.message("device"), 200);
		device.setHidden(true);
		ListGridField geolocation = new ListGridField("geolocation", I18N.message("geolocation"), 200);
		geolocation.setHidden(true);

		ListGridField username = new ListGridField("username", I18N.message("username"), 100);
		username.setCanFilter(true);
		username.setHidden(true);

		ListGridField payload = new ListGridField("payload", I18N.message("payload"));
		payload.setCanFilter(true);
		payload.setWidth("*");
		payload.setHidden(true);
		payload.setEscapeHTML(true);

		calls = new ListGrid();
		calls.setEmptyMessage(I18N.message("notitemstoshow"));
		calls.setWidth100();
		calls.setHeight100();
		calls.setFields(date, protocol, ip, userField, uri, tenant, sid, userId, username, device, geolocation,
				payload);
		calls.setSelectionType(SelectionStyle.SINGLE);
		calls.setShowRecordComponents(true);
		calls.setShowRecordComponentsByCell(true);
		calls.setCanFreezeFields(true);
		calls.setFilterOnKeypress(true);
		calls.setAutoFetchData(true);
		calls.sort("date", SortDirection.DESCENDING);

		results.addMember(calls);

		callsLayout.addMember(search, 0);

		// Prepare a panel containing a title and the documents list
		infoPanel = new InfoPanel("");
		callsLayout.addMember(infoPanel, 1);

		callsLayout.addMember(results, 2);

		body.setMembers(callsLayout);

		calls.addCellDoubleClickHandler(new CellDoubleClickHandler() {

			@Override
			public void onCellDoubleClick(CellDoubleClickEvent event) {
				String value = event.getRecord().getAttributeAsString(calls.getField(event.getColNum()).getName());
				TextAreaItem item = ItemFactory.newTextAreaItem("value", "value", value);
				item.setWidth(500);
				item.setHeight(400);
				LD.askForValue(I18N.message("value"), I18N.message("value"), value, item, 500, new ValueCallback() {

					@Override
					public void execute(String value) {
						// Nothing to do
					}
				});
			}
		});
	}

	/**
	 * Gets the option items to choose events types
	 * 
	 * @return an array of select items
	 */
	public SelectItem[] getEventTypes() {
		List<SelectItem> items = new ArrayList<SelectItem>();

		return items.toArray(new SelectItem[0]);
	}

	@SuppressWarnings("unchecked")
	private void onSearch() {
		calls.setData(new ListGridRecord[0]);

		final Map<String, Object> values = (Map<String, Object>) vm.getValues();

		if (!vm.validate())
			return;

		Long userId = getUserId(values);

		Date fromValue = null;
		if (values.get("fromDate") != null)
			fromValue = (Date) values.get("fromDate");

		Date tillValue = null;
		if (values.get("tillDate") != null)
			tillValue = (Date) values.get("tillDate");

		String sid = null;
		if (values.get("sid") != null)
			sid = (String) values.get("sid");

		String protocol = null;
		if (values.get("protocol") != null)
			protocol = (String) values.get("protocol");

		String uri = null;
		if (values.get("uri") != null)
			uri = (String) values.get("uri");

		int displayMaxValue = getDisplayMaxValue(values);

		doSearch(userId, fromValue, tillValue, sid, protocol, uri, displayMaxValue);
	}

	private int getDisplayMaxValue(final Map<String, Object> values) {
		int displayMaxValue = 0;
		if (values.get("displayMax") != null) {
			if (values.get("displayMax") instanceof Integer)
				displayMaxValue = (Integer) values.get("displayMax");
			else
				displayMaxValue = Integer.parseInt((String) values.get("displayMax"));
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
				new AsyncCallback<GUIHistory[]>() {

					@Override
					public void onFailure(Throwable caught) {
						LD.clearPrompt();
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIHistory[] result) {
						LD.clearPrompt();

						if (result != null && result.length > 0) {
							ListGridRecord[] records = new ListGridRecord[result.length];
							for (int i = 0; i < result.length; i++) {
								ListGridRecord rec = new ListGridRecord();
								rec.setAttribute("date", result[i].getDate());
								rec.setAttribute("user", result[i].getUsername());
								rec.setAttribute("sid", result[i].getSessionId());
								rec.setAttribute("userId", result[i].getUserId());
								rec.setAttribute("ip", result[i].getIp());
								rec.setAttribute("device", result[i].getDevice());
								rec.setAttribute("geolocation", result[i].getGeolocation());
								rec.setAttribute("username", result[i].getUserLogin());
								rec.setAttribute("payload", result[i].getComment());
								rec.setAttribute("uri", result[i].getPath());
								rec.setAttribute("protocol", result[i].getProtocol());
								rec.setAttribute("tenant", result[i].getTenant());
								records[i] = rec;
							}
							calls.setData(records);
						}
						callsLayout.removeMember(infoPanel);
						infoPanel = new InfoPanel("");
						infoPanel.setMessage(I18N.message("showelements", Integer.toString(calls.getTotalRows())));
						callsLayout.addMember(infoPanel, 1);
					}
				});
	}
}