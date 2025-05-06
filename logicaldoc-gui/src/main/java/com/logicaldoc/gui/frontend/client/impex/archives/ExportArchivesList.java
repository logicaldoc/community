package com.logicaldoc.gui.frontend.client.impex.archives;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIArchive;
import com.logicaldoc.gui.common.client.data.ArchivesDS;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.FileSizeListGridField;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.frontend.client.services.ImpexService;
import com.smartgwt.client.types.Alignment;
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
public class ExportArchivesList extends VLayout {

	private static final String STATUS = "status";

	private static final String STATUSICON = "statusicon";

	protected Layout detailsContainer;

	protected RefreshableListGrid list;

	protected Canvas details = SELECT_ELEMENT;

	static final Canvas SELECT_ELEMENT = new HTMLPanel("&nbsp;" + I18N.message("selectarchive"));

	protected int archivesType = GUIArchive.TYPE_DEFAULT;

	protected boolean showHistory = false;

	public ExportArchivesList(int archivesType, boolean showHistory) {
		setWidth100();
		this.archivesType = archivesType;
		this.showHistory = showHistory;
	}

	@Override
	public void onDraw() {
		final InfoPanel infoPanel = new InfoPanel("");

		VLayout listing = new VLayout();
		detailsContainer = new VLayout();
		details = SELECT_ELEMENT;

		// Initialize the listing panel
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("60%");
		listing.setShowResizeBar(true);

		ListGridField id = new IdListGridField();

		ListGridField name = new ListGridField("name", I18N.message("name"), 250);
		name.setCanFilter(true);

		ListGridField type = new ListGridField("type", I18N.message("type"), 130);
		type.setHidden(true);
		ListGridField typeLabel = new ListGridField("typelabel", I18N.message("type"), 130);
		typeLabel.setCanFilter(false);

		ListGridField status = new ArchiveStatusListGridField();

		ListGridField created = new DateListGridField("created", "createdon");

		ListGridField creator = new ListGridField("creator", I18N.message("creator"), 110);
		creator.setCanFilter(true);
		ListGridField closer = new ListGridField("closer", I18N.message("closedby"), 110);
		closer.setCanFilter(true);

		ListGridField size = new FileSizeListGridField("size", I18N.message("size"));
		size.setCanFilter(false);

		ListGridField aosManager = new ListGridField("aosmanager", I18N.message("aosmanager"), 110);
		aosManager.setCanFilter(true);

		ListGridField pathOnServer = new ListGridField("pathonserver", I18N.message("pathonserver"));
		pathOnServer.setCanFilter(false);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		if (this.archivesType == GUIArchive.TYPE_STORE)
			list.setFields(id, created, name, size, status, creator, closer, aosManager, pathOnServer);
		else
			list.setFields(id, created, name, size, status, creator, closer, pathOnServer);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setFilterOnKeypress(true);
		list.setShowFilterEditor(true);
		if (this.archivesType == GUIArchive.TYPE_STORE && this.showHistory)
			list.setDataSource(new ArchivesDS(GUIArchive.MODE_EXPORT, this.archivesType, GUIArchive.STATUS_FINALIZED,
					Session.get().getUser().getId()));
		else
			list.setDataSource(
					new ArchivesDS(GUIArchive.MODE_EXPORT, this.archivesType, null, Session.get().getUser().getId()));

		if (!showHistory)
			listing.addMember(infoPanel);
		listing.addMember(list);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		toolStrip.addButton(refresh);
		refresh.addClickHandler(event -> refresh());

		ToolStripButton addArchive = new ToolStripButton();
		addArchive.setTitle(I18N.message("addarchive"));
		addArchive.addClickHandler(event -> {
			onAddingArchive();
			event.cancel();
		});
		toolStrip.addButton(addArchive);

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		list.addSelectionChangedHandler(event -> {
			ListGridRecord rec = list.getSelectedRecord();
			try {
				showDetails(Long.parseLong(rec.getAttribute("id")),
						!Integer.toString(GUIArchive.STATUS_OPEN).equals(rec.getAttribute(STATUS)));
			} catch (Exception t) {
				// Nothing to do
			}
		});

		list.addDataArrivedHandler(event -> infoPanel.setMessage(I18N.message("showarchives",
				Integer.toString(list.getTotalRows()), Session.get().getConfig("conf.exportdir"))));

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		if (!showHistory)
			setMembers(toolStrip, listing, detailsContainer);
		else
			setMembers(listing, detailsContainer);
	}

	protected void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord rec = list.getSelectedRecord();
		final long id = Long.parseLong(rec.getAttributeAsString("id"));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm)) {
				ImpexService.Instance.get().delete(id, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						list.removeSelectedData();
						list.deselectAllRecords();
						showDetails(null, true);
					}
				});
			}
		}));

		MenuItem open = new MenuItem();
		open.setTitle(I18N.message("open"));
		open.addClickHandler(event -> openArchive(rec));

		MenuItem close = new MenuItem();
		close.setTitle(I18N.message("close"));
		close.addClickHandler(
				event -> LD.ask(I18N.message("question"), I18N.message("confirmarchiveclose"), confirm -> {
					if (Boolean.TRUE.equals(confirm)) {
						onClosingArchive(rec);
					}
				}));

		if (GUIArchive.STATUS_OPEN != Integer.parseInt(rec.getAttributeAsString(STATUS)))
			close.setEnabled(false);

		if (GUIArchive.STATUS_ERROR != Integer.parseInt(rec.getAttributeAsString(STATUS)))
			open.setEnabled(false);

		contextMenu.setItems(close, open, delete);
		addUsefulMenuItem(contextMenu);
		contextMenu.showContextMenu();
	}

	public void showDetails(Long archiveId, boolean readonly) {
		if (details != null)
			detailsContainer.removeMember(details);
		if (archiveId != null)
			details = new ArchiveDetailsPanel(this, archiveId, readonly);
		else
			details = SELECT_ELEMENT;
		detailsContainer.addMember(details);
	}

	public ListGrid getList() {
		return list;
	}

	protected void closeArchive(final ListGridRecord rec) {
		ImpexService.Instance.get().setStatus(rec.getAttributeAsLong("id"), GUIArchive.STATUS_CLOSED,
				new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(STATUS, GUIArchive.STATUS_CLOSED);
						rec.setAttribute(STATUSICON, "lock");
						list.refreshRow(list.getRecordIndex(rec));
						showDetails(Long.parseLong(rec.getAttributeAsString("id")), true);
					}
				});
	}

	public int getArchivesType() {
		return archivesType;
	}

	protected void onAddingArchive() {
		ArchiveDialog dialog = new ArchiveDialog(ExportArchivesList.this);
		dialog.show();
	}

	/**
	 * This method is used only by the classes that extend this class.
	 */
	protected Menu addUsefulMenuItem(Menu contextMenu) {
		return contextMenu;
	}

	protected void onClosingArchive(final ListGridRecord rec) {
		closeArchive(rec);
	}

	protected void openArchive(final ListGridRecord rec) {
		ImpexService.Instance.get().setStatus(Long.parseLong(rec.getAttributeAsString("id")), GUIArchive.STATUS_OPEN,
				new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(STATUS, "0");
						rec.setAttribute(STATUSICON, "lock_open");
						list.refreshRow(list.getRecordIndex(rec));
						showDetails(Long.parseLong(rec.getAttributeAsString("id")), true);
					}
				});
	}

	public void refresh(int archivesType, boolean showHistory) {
		this.archivesType = archivesType;
		this.showHistory = showHistory;
		refresh();
	}

	public void refresh() {
		if (archivesType == GUIArchive.TYPE_STORE && showHistory)
			list.refresh(new ArchivesDS(GUIArchive.MODE_EXPORT, archivesType, GUIArchive.STATUS_FINALIZED,
					Session.get().getUser().getId()));
		else
			list.refresh(new ArchivesDS(GUIArchive.MODE_EXPORT, archivesType, null, Session.get().getUser().getId()));
		detailsContainer.removeMembers(detailsContainer.getMembers());
		details = SELECT_ELEMENT;
		detailsContainer.setMembers(details);
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