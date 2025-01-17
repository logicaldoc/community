package com.logicaldoc.gui.frontend.client.dashboard.reading;

import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIReadingRequest;
import com.logicaldoc.gui.common.client.controllers.ReadingRequestController;
import com.logicaldoc.gui.common.client.controllers.ReadingRequestObserver;
import com.logicaldoc.gui.common.client.data.ReadingRequestsDS;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.grid.FileVersionListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.grid.TypeIconGridField;
import com.logicaldoc.gui.common.client.grid.UserListGridField;
import com.logicaldoc.gui.common.client.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.preview.PreviewPopup;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.ReadingRequestService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;

/**
 * This panel shows the reading requests received or sent by the current user
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
public class ReadingRequestsPanel extends VLayout implements ReadingRequestObserver {

	private static final String DOC_ID = "docId";

	private static final String CONFIRMED = "confirmed";

	private static final String RECEIVED = "received";

	private static ReadingRequestsPanel instance;

	private RefreshableListGrid readingsGrid = new RefreshableListGrid();

	private HLayout content = new HLayout();

	private ReadingRequetPreviewPanel previewPanel = new ReadingRequetPreviewPanel();

	private SelectItem select = ItemFactory.newSelectItem("show");

	public static ReadingRequestsPanel get() {
		if (instance == null)
			instance = new ReadingRequestsPanel();
		return instance;
	}

	public ReadingRequestsPanel() {
		setWidth100();
		setHeight100();
		setMembersMargin(3);

		ReadingRequestController.get().addObserver(this);
	}

	@Override
	public void onDraw() {
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		select.setWidth(250);
		LinkedHashMap<String, String> map = new LinkedHashMap<>();
		map.put(RECEIVED, I18N.message("readingsihavetoconfirm").toLowerCase());
		map.put("sent", I18N.message("readingsihavesent").toLowerCase());
		select.setValueMap(map);
		select.setValue(RECEIVED);
		select.addChangedHandler(event -> refresh());

		toolStrip.addFormItem(select);
		toolStrip.addFill();
		addMember(toolStrip);

		ListGridField id = new ListGridField("id", I18N.getAttributeLabel("id"), 60);
		id.setHidden(true);
		ListGridField recipient = new UserListGridField("user", "userId", "recipient");
		ListGridField requestor = new UserListGridField("requestor", "requestorId", "requestor");
		ListGridField date = new DateListGridField("date", "date", DateCellFormatter.FORMAT_LONG);
		ListGridField confirmed = new DateListGridField(CONFIRMED, "confirmedon", DateCellFormatter.FORMAT_LONG);
		ListGridField message = new ListGridField("message", I18N.message("message"));
		FileNameListGridField fileName = new FileNameListGridField();
		ListGridField fileVersion = new FileVersionListGridField();
		ListGridField icon = new TypeIconGridField();

		readingsGrid = new RefreshableListGrid(new ReadingRequestsDS(true, null)) {
			@Override
			protected String getCellCSSText(ListGridRecord rec, int rowNum, int colNum) {
				return rec.getAttribute(CONFIRMED) == null ? "font-weight: bold;"
						: super.getCellCSSText(rec, rowNum, colNum);
			}
		};
		readingsGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		readingsGrid.setCanFreezeFields(true);
		readingsGrid.setAutoFetchData(true);
		readingsGrid.setShowResizeBar(true);
		readingsGrid.setResizeBarTarget("next");
		readingsGrid.setSelectionType(SelectionStyle.SINGLE);
		readingsGrid.setFields(id, date, icon, fileName, confirmed, fileVersion, recipient, requestor, message);
		readingsGrid.addSelectionChangedHandler(event -> DocumentService.Instance.get()
				.getById(event.getSelectedRecord().getAttributeAsLong(DOC_ID), new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(GUIDocument document) {
						previewPanel.setDocument(document);
					}
				}));
		readingsGrid.addCellContextClickHandler(contextClickEvent -> {
			prepateContextMenu().showContextMenu();
			contextClickEvent.cancel();
		});

		// When the data arrives we may notify the unconfirmed readings
		readingsGrid.addDataArrivedHandler(event -> {
			ListGridRecord[] records = readingsGrid.getRecords();
			List<GUIReadingRequest> requests = new ArrayList<>();
			for (ListGridRecord rcd : records) {
				long recipientId = rcd.getAttributeAsLong("userId");
				if (rcd.getAttribute(CONFIRMED) == null && recipientId == Session.get().getUser().getId()) {
					GUIReadingRequest req = new GUIReadingRequest();
					req.setId(rcd.getAttributeAsLong("id"));
					req.setDocId(rcd.getAttributeAsLong(DOC_ID));
					req.setFileName(rcd.getAttributeAsString("filename"));
					req.setUserId(recipientId);
					req.setRequestorId(rcd.getAttributeAsLong("requestId"));
					requests.add(req);
				}
			}
			ReadingRequestController.get().addUnconfirmedReadings(requests);
		});

		content.setWidth100();
		content.setHeight100();
		addMember(content);

		content.setMembers(readingsGrid, previewPanel);
		refresh();
	}

	private Menu prepateContextMenu() {
		long selectedDocId = readingsGrid.getSelectedRecord().getAttributeAsLong(DOC_ID);
		long selectedReadingId = readingsGrid.getSelectedRecord().getAttributeAsLong("id");

		MenuItem openInFolder = new MenuItem();
		openInFolder.setTitle(I18N.message("openinfolder"));
		openInFolder.addClickHandler(event -> DocumentsPanel.get().openInFolder(selectedDocId));

		MenuItem download = new MenuItem();
		download.setTitle(I18N.message("download"));
		download.addClickHandler(event -> DocUtil.download(selectedDocId, null));

		MenuItem preview = new MenuItem();
		preview.setTitle(I18N.message("preview"));
		preview.addClickHandler(
				event -> DocumentService.Instance.get().getById(selectedDocId, new DefaultAsyncCallback<>() {

					@Override
					public void onSuccess(GUIDocument document) {
						new PreviewPopup(document).show();
					}
				}));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), value -> {
			if (Boolean.TRUE.equals(value)) {
				ReadingRequestService.Instance.get().delete(selectedReadingId, new DefaultAsyncCallback<>() {

					@Override
					public void onSuccess(Void result) {
						readingsGrid.removeSelectedData();
					}
				});
			}
		}));

		MenuItem invite = new MenuItem();
		invite.setTitle(I18N.message("inviteandremind"));
		invite.addClickHandler(event -> ReadingRequestService.Instance.get().notityReadingRequest(selectedReadingId,
				new DefaultAsyncCallback<>() {

					@Override
					public void onSuccess(Void arg) {
						GuiLog.info("invitationsent");
					}
				}));

		delete.setEnabled(Session.get().getUser().getId() == readingsGrid.getSelectedRecord()
				.getAttributeAsLong("requestorId").longValue());

		Menu contextMenu = new Menu();
		contextMenu.setItems(preview, download, openInFolder, invite, delete);

		return contextMenu;
	}

	public void refresh() {
		if (previewPanel != null)
			content.removeMember(previewPanel);
		previewPanel = new ReadingRequetPreviewPanel();
		content.addMember(previewPanel);
		readingsGrid.refresh(new ReadingRequestsDS(RECEIVED.equals(select.getValueAsString()), null));

	}

	@Override
	public void onConfirmReading(long docId) {
		com.smartgwt.client.data.Record[] records = readingsGrid
				.findAll(new AdvancedCriteria(DOC_ID, OperatorId.EQUALS, docId));
		if (records != null)
			for (com.smartgwt.client.data.Record rcd : records) {
				rcd.setAttribute(CONFIRMED, new Date());
			}
	}

	@Override
	public void onNewReadingRequests(List<GUIReadingRequest> requests) {
		refresh();
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