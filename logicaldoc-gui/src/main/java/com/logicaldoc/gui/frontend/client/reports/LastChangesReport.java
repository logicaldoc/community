package com.logicaldoc.gui.frontend.client.reports;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIHistory;
import com.logicaldoc.gui.common.client.grid.CopyCellClickHandler;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.grid.UserListGridField;
import com.logicaldoc.gui.common.client.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.preview.PreviewPopup;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.EventSelectorOptions;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.SystemService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.SortDirection;
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
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * This panel is used to show the last changes events.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class LastChangesReport extends AdminPanel {

	private static final String NAME = "name";

	private static final String DISPLAYMAX = "displaymax";

	private static final String REASON = "reason";

	private static final String COMMENT = "comment";

	private static final String USERNAME = "username";

	private static final String GEOLOCATION = "geolocation";

	private static final String DEVICE = "device";

	private static final String FOLDER_ID = "folderId";

	private static final String DOC_ID = "docId";

	private static final String FOLDER_STR = "folder";

	private static final String USER_ID = "userId";

	private static final String EVENT = "event";

	private static final String TILL_DATE = "tillDate";

	private static final String FROM_DATE = "fromDate";

	private Layout search = new VLayout();

	private Layout results = new VLayout();

	private VLayout lastchanges = new VLayout();

	private ValuesManager vm = new ValuesManager();

	private ListGrid histories;

	private InfoPanel infoPanel;

	private FolderSelector folder;

	public LastChangesReport() {
		super("lastchanges");
	}

	@Override
	public void onDraw() {
		HStack formsLayout = new HStack(10);

		DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setAlign(Alignment.LEFT);
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setNumCols(8);
		form.setWrapItemTitles(false);

		// Username
		SelectItem user = ItemFactory.newUserSelector("user", "user", null, false, false);
		user.setEndRow(true);

		// From
		DateItem fromDate = ItemFactory.newDateItem(FROM_DATE, "from");
		fromDate.setColSpan(4);

		// To
		DateItem tillDate = ItemFactory.newDateItem(TILL_DATE, "till");
		tillDate.setEndRow(true);
		tillDate.setColSpan(4);

		// Session ID
		TextItem sessionId = ItemFactory.newTextItem("sid", null);
		sessionId.setWidth(250);
		sessionId.setColSpan(8);
		sessionId.setEndRow(true);

		folder = new FolderSelector(null, null);
		folder.setWidth(200);
		folder.setEndRow(true);
		folder.setColSpan(8);

		// Max results
		SpinnerItem displayMax = ItemFactory.newSpinnerItem(DISPLAYMAX, 100, 5, null);
		displayMax.setHint(I18N.message("elements"));
		displayMax.setStep(10);
		displayMax.setStartRow(true);

		ButtonItem searchButton = new ButtonItem();
		searchButton.setTitle(I18N.message("search"));
		searchButton.setAutoFit(true);
		searchButton.setEndRow(false);
		searchButton.addClickHandler(event -> onSearch());

		ButtonItem resetButton = new ButtonItem();
		resetButton.setTitle(I18N.message("reset"));
		resetButton.addClickHandler(event -> vm.clearValues());
		resetButton.setColSpan(2);
		resetButton.setAutoFit(true);
		resetButton.setEndRow(true);
		resetButton.setStartRow(false);

		ButtonItem print = new ButtonItem();
		print.setTitle(I18N.message("print"));
		print.addClickHandler(event -> GridUtil.print(histories));
		print.setAutoFit(true);
		print.setEndRow(true);
		print.setStartRow(false);

		ButtonItem export = new ButtonItem();
		export.setTitle(I18N.message("export"));
		export.addClickHandler(event -> GridUtil.exportCSV(histories, false));
		if (!Feature.enabled(Feature.EXPORT_CSV)) {
			export.setDisabled(true);
			export.setTooltip(I18N.message("featuredisabled"));
		}
		export.setAutoFit(true);
		export.setStartRow(true);
		export.setEndRow(false);

		if (Feature.visible(Feature.EXPORT_CSV))
			form.setItems(user, sessionId, fromDate, tillDate, folder, displayMax, searchButton, resetButton, export,
					print);
		else
			form.setItems(user, sessionId, fromDate, tillDate, folder, displayMax, searchButton, resetButton, print);

		DynamicForm eventForm = new DynamicForm();
		eventForm.setValuesManager(vm);
		eventForm.setAlign(Alignment.LEFT);
		eventForm.setTitleOrientation(TitleOrientation.LEFT);
		eventForm.setNumCols(2);
		eventForm.setColWidths(1, "*");

		// Event
		SelectItem event = ItemFactory.newEventsSelector(EVENT, I18N.message(EVENT), null,
				new EventSelectorOptions(true, true, true, true, true, false, true, false));
		event.setColSpan(2);
		event.setEndRow(true);

		eventForm.setItems(event);

		formsLayout.addMember(form);
		formsLayout.addMember(eventForm);
		formsLayout.setMembersMargin(80);

		search.setMembersMargin(10);
		search.setMembers(formsLayout);
		search.setHeight("30%");
		search.setShowResizeBar(true);
		search.setWidth100();
		search.setMargin(10);

		ListGridField eventField = new ListGridField(EVENT, I18N.message(EVENT), 200);
		eventField.setCanFilter(true);

		ListGridField date = new DateListGridField("date", "date", DateCellFormatter.FORMAT_LONG);

		ListGridField userField = new UserListGridField("user", USER_ID, "user");
		userField.setCanFilter(true);
		userField.setAlign(Alignment.CENTER);

		FileNameListGridField name = new FileNameListGridField(NAME, "icon", I18N.message(NAME), 150);
		name.setCanFilter(true);

		ListGridField folderField = new ListGridField(FOLDER_STR, I18N.message(FOLDER_STR), 100);
		folderField.setCanFilter(true);

		ListGridField sid = new ListGridField("sid", I18N.message("sid"), 250);
		sid.setCanFilter(true);
		sid.setAlign(Alignment.CENTER);

		ListGridField key = new ListGridField("key", I18N.message("key"), 90);
		key.setCanFilter(true);
		key.setHidden(true);
		key.setAlign(Alignment.CENTER);

		ListGridField docId = new ListGridField(DOC_ID, I18N.message("documentid"), 100);
		docId.setCanFilter(true);
		docId.setHidden(true);

		ListGridField folderId = new ListGridField(FOLDER_ID, I18N.message("folderid"), 100);
		folderId.setCanFilter(true);
		folderId.setHidden(true);

		ListGridField userId = new ListGridField(USER_ID, I18N.message("userid"), 100);
		userId.setCanFilter(true);
		userId.setHidden(true);

		ListGridField ip = new ListGridField("ip", I18N.message("ip"), 100);
		ip.setCanFilter(true);
		ip.setHidden(true);

		ListGridField device = new ListGridField(DEVICE, I18N.message(DEVICE), 200);
		device.setHidden(true);
		ListGridField geolocation = new ListGridField(GEOLOCATION, I18N.message(GEOLOCATION), 200);
		geolocation.setHidden(true);

		ListGridField username = new ListGridField(USERNAME, I18N.message(USERNAME), 100);
		username.setCanFilter(true);
		username.setHidden(true);

		ListGridField comment = new ListGridField(COMMENT, I18N.message(COMMENT), 200);
		comment.setCanFilter(true);
		comment.setHidden(true);

		ListGridField reason = new ListGridField(REASON, I18N.message(REASON), 200);
		reason.setCanFilter(true);
		reason.setHidden(true);

		histories = new ListGrid();
		histories.setEmptyMessage(I18N.message("notitemstoshow"));
		histories.setWidth100();
		histories.setHeight100();
		histories.setFields(eventField, date, userField, name, folderField, sid, key, docId, folderId, userId, username,
				ip, device, geolocation, comment, reason);
		histories.setSelectionType(SelectionStyle.SINGLE);
		histories.setShowRecordComponents(true);
		histories.setShowRecordComponentsByCell(true);
		histories.setCanFreezeFields(true);
		histories.setFilterOnKeypress(true);
		histories.setAutoFetchData(true);
		histories.sort("date", SortDirection.DESCENDING);
		histories.addCellDoubleClickHandler(new CopyCellClickHandler());
		histories.addCellContextClickHandler(click -> {
			showContextMenu();
			click.cancel();
		});

		results.addMember(histories);

		lastchanges.addMember(search, 0);

		// Prepare a panel containing a title and the documents list
		infoPanel = new InfoPanel("");
		lastchanges.addMember(infoPanel, 1);

		lastchanges.addMember(results, 2);

		body.setMembers(lastchanges);

		histories.addDoubleClickHandler(
				evn -> vm.setValue("sid", histories.getSelectedRecord().getAttributeAsString("sid")));
	}

	/**
	 * Gets the option items to choose events types
	 * 
	 * @return an array of select items
	 */
	public SelectItem[] getEventTypes() {
		return new ArrayList<>().toArray(new SelectItem[0]);
	}

	private void onSearch() {
		histories.setData();
		if (Boolean.FALSE.equals(vm.validate()))
			return;

		List<String> eventValues = getEvents();

		Long userId = getUserId();

		Date fromValue = null;
		if (vm.getValue(FROM_DATE) != null)
			fromValue = (Date) vm.getValue(FROM_DATE);
		Date tillValue = null;
		if (vm.getValue(TILL_DATE) != null)
			tillValue = (Date) vm.getValue(TILL_DATE);

		String sid = null;
		if (vm.getValue("sid") != null)
			sid = vm.getValueAsString("sid");

		int displayMaxValue = getDisplayMax();

		doSearch(eventValues, userId, fromValue, tillValue, sid, displayMaxValue);
	}

	private List<String> getEvents() {
		String[] eventValues = new String[0];
		if (vm.getValue(EVENT) != null) {
			String buf = vm.getValueAsString(EVENT).trim().toLowerCase();
			buf = buf.replace('[', ' ');
			buf = buf.replace(']', ' ');
			buf = buf.replace(" ", "");
			eventValues = buf.split(",");
		}
		return Arrays.asList(eventValues);
	}

	private Long getUserId() {
		Long userId = null;
		if (vm.getValue("user") != null) {
			if (vm.getValue("user") instanceof Long longVal)
				userId = longVal;
			else
				userId = Long.parseLong(vm.getValueAsString("user"));
		}
		return userId;
	}

	private int getDisplayMax() {
		int displayMaxValue = 0;
		if (vm.getValue(DISPLAYMAX) != null) {
			if (vm.getValue(DISPLAYMAX) instanceof Integer intVal)
				displayMaxValue = intVal;
			else
				displayMaxValue = Integer.parseInt(vm.getValueAsString(DISPLAYMAX));
		}
		return displayMaxValue;
	}

	private void doSearch(List<String> eventValues, Long userId, Date fromValue, Date tillValue, String sid,
			int displayMaxValue) {
		LD.contactingServer();
		SystemService.Instance.get().search(userId, fromValue, tillValue, displayMaxValue, sid, eventValues,
				folder.getFolderId(), new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(List<GUIHistory> result) {
						LD.clearPrompt();

						List<ListGridRecord> records = new ArrayList<>();
						for (GUIHistory hist : result) {
							ListGridRecord rec = new ListGridRecord();
							rec.setAttribute(EVENT, I18N.message(hist.getEvent()));
							rec.setAttribute("date", hist.getDate());
							rec.setAttribute("user", hist.getUsername());
							rec.setAttribute(NAME, hist.getFileName());
							rec.setAttribute(FOLDER_STR, hist.getPath());
							rec.setAttribute("sid", hist.getSessionId());
							rec.setAttribute("key", hist.getKeyLabel());
							rec.setAttribute(DOC_ID, hist.getDocId());
							rec.setAttribute(FOLDER_ID, hist.getFolderId());
							rec.setAttribute(USER_ID, hist.getUserId());
							rec.setAttribute("ip", hist.getIp());
							rec.setAttribute(DEVICE, hist.getDevice());
							rec.setAttribute(GEOLOCATION, hist.getGeolocation());
							rec.setAttribute(USERNAME, hist.getUserLogin());
							rec.setAttribute(COMMENT, hist.getComment());
							rec.setAttribute(REASON, hist.getReason());
							rec.setAttribute("icon", hist.getIcon());
							records.add(rec);
						}
						histories.setData(records.toArray(new ListGridRecord[0]));

						lastchanges.removeMember(infoPanel);
						infoPanel = new InfoPanel("");
						infoPanel.setMessage(I18N.message("showelements", Integer.toString(histories.getTotalRows())));
						lastchanges.addMember(infoPanel, 1);
					}
				});
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		ListGridRecord selection = histories.getSelectedRecord();
		if (selection == null)
			return;

		final Long docId = selection.getAttributeAsLong(DOC_ID) != null && selection.getAttributeAsLong(DOC_ID) != 0L
				? selection.getAttributeAsLong(DOC_ID)
				: null;
		final Long folderId = selection.getAttributeAsLong(FOLDER_ID) != null
				&& selection.getAttributeAsLong(FOLDER_ID) != 0L ? selection.getAttributeAsLong(FOLDER_ID) : null;
		if (docId == null && folderId == null)
			return;

		MenuItem openInFolder = new MenuItem();
		openInFolder.setTitle(I18N.message("openinfolder"));
		openInFolder.addClickHandler(event -> DocumentsPanel.get().openInFolder(docId));

		MenuItem preview = preparePreviewItem(docId);

		MenuItem download = new MenuItem();
		download.setTitle(I18N.message("download"));
		download.addClickHandler(event -> DocUtil.download(docId, null));

		MenuItem openFolder = new MenuItem();
		openFolder.setTitle(I18N.message("openfolder"));
		openFolder.addClickHandler(event -> DocumentsPanel.get().openInFolder(folderId, null));

		if (docId != null)
			contextMenu.setItems(download, preview, openInFolder);
		else
			contextMenu.setItems(openFolder);
		contextMenu.showContextMenu();
	}

	private MenuItem preparePreviewItem(final Long docId) {
		MenuItem preview = new MenuItem();
		preview.setTitle(I18N.message("preview"));
		if (docId != null)
			preview.addClickHandler(event -> DocumentService.Instance.get().getById(docId, new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(GUIDocument doc) {
					new PreviewPopup(doc).show();
				}
			}));
		return preview;
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