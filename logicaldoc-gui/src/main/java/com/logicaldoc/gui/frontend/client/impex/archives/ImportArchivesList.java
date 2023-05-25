package com.logicaldoc.gui.frontend.client.impex.archives;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIArchive;
import com.logicaldoc.gui.common.client.data.ArchivesDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.services.ImpexService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Panel showing the list of export archives
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ImportArchivesList extends VLayout {

	private static final String DESCRIPTION = "description";

	private Layout detailsContainer;

	private RefreshableListGrid list;

	private Canvas details = SELECT_ELEMENT;

	final static Canvas SELECT_ELEMENT = new HTMLPanel("&nbsp;" + I18N.message("selectarchive"));

	public ImportArchivesList() {
		setWidth100();
	}

	@Override
	public void onDraw() {
		final InfoPanel infoPanel = new InfoPanel("");
		Layout listing = new VLayout();
		detailsContainer = new VLayout();
		details = SELECT_ELEMENT;

		// Initialize the listing panel
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("70%");
		listing.setShowResizeBar(true);

		ListGridField id = new ListGridField("id", 50);
		id.setHidden(true);

		ListGridField name = new ListGridField("name", I18N.message("name"), 130);

		ListGridField desdcription = new ListGridField(DESCRIPTION, I18N.message(DESCRIPTION), 250);
		desdcription.setCanFilter(false);

		ListGridField status = new ListGridField("statusicon", I18N.message("status"), 50);
		status.setType(ListGridFieldType.IMAGE);
		status.setCanSort(false);
		status.setAlign(Alignment.CENTER);
		status.setShowDefaultContextMenu(false);
		status.setImageURLPrefix(Util.imagePrefix());
		status.setImageURLSuffix(".png");
		status.setCanFilter(false);

		ListGridField created = new DateListGridField("created", "createdon");

		ListGridField creator = new ListGridField("creator", I18N.message("creator"), 110);

		ListGridField size = new ListGridField("size", I18N.message("size"), 70);
		size.setAlign(Alignment.CENTER);
		size.setType(ListGridFieldType.INTEGER);
		size.setCanFilter(false);

		ListGridField pathOnServer = new ListGridField("pathonserver", I18N.message("pathonserver"));
		pathOnServer.setCanFilter(false);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		list.setFields(id, created, name, desdcription, size, status, creator, pathOnServer);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setFilterOnKeypress(true);
		list.setShowFilterEditor(true);
		list.setDataSource(new ArchivesDS(GUIArchive.MODE_IMPORT, null, null, null));

		listing.addMember(infoPanel);
		listing.addMember(list);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		toolStrip.addButton(refresh);
		refresh.addClickHandler(event -> {
			list.refresh(new ArchivesDS(GUIArchive.MODE_IMPORT, null, null, null));
			detailsContainer.removeMembers(detailsContainer.getMembers());
			details = SELECT_ELEMENT;
			detailsContainer.setMembers(details);
		});

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		list.addSelectionChangedHandler(event -> {
			ListGridRecord rec = list.getSelectedRecord();
			try {
				showDetails(Long.parseLong(rec.getAttribute("id")));
			} catch (Exception t) {
				// Nothing to do
			}
		});

		list.addDataArrivedHandler(event -> infoPanel.setMessage(I18N.message("showarchives",
				Integer.toString(list.getTotalRows()), Session.get().getConfig("conf.importdir"))));

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		setMembers(toolStrip, listing, detailsContainer);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord rec = list.getSelectedRecord();
		final long id = Long.parseLong(rec.getAttributeAsString("id"));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm)) {
				ImpexService.Instance.get().delete(id, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						list.removeSelectedData();
						list.deselectAllRecords();
						showDetails(null);
					}
				});
			}
		}));

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}

	private void showDetails(Long archiveId) {
		if (details != null)
			detailsContainer.removeMember(details);
		if (archiveId == null) {
			details = SELECT_ELEMENT;
			detailsContainer.addMember(details);
			return;
		}

		ImpexService.Instance.get().load(archiveId, new AsyncCallback<GUIArchive>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIArchive result) {
				details = new ImportDetailsPanel(result, ImportArchivesList.this);
				detailsContainer.addMember(details);
			}
		});
	}

	public ListGrid getList() {
		return list;
	}

	public void updateRecord(GUIArchive result) {
		ListGridRecord rec = list.getSelectedRecord();
		rec.setAttribute(DESCRIPTION, result.getDescription());
		list.refreshRow(list.getRecordIndex(rec));
	}
}