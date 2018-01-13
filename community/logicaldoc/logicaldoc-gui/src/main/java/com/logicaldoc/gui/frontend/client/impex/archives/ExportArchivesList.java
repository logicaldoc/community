package com.logicaldoc.gui.frontend.client.impex.archives;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIArchive;
import com.logicaldoc.gui.common.client.data.ArchivesDS;
import com.logicaldoc.gui.common.client.formatters.DateCellFormatter;
import com.logicaldoc.gui.common.client.formatters.FileSizeCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.frontend.client.services.ImpexService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionChangedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Panel showing the list of export archives
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class ExportArchivesList extends VLayout {

	protected Layout listing;

	protected Layout detailsContainer;

	protected ListGrid list;

	protected Canvas details = SELECT_ELEMENT;

	private InfoPanel infoPanel;

	final static Canvas SELECT_ELEMENT = new HTMLPanel("&nbsp;" + I18N.message("selectarchive"));

	protected int archivesType = GUIArchive.TYPE_DEFAULT;

	protected boolean showHistory = false;

	public ExportArchivesList(int archsType, boolean history) {
		setWidth100();
		infoPanel = new InfoPanel("");
		refresh(archsType, history);
	}

	public void refresh(int archivesType, boolean history) {
		Canvas[] members = getMembers();
		for (Canvas canvas : members) {
			removeMember(canvas);
		}

		this.archivesType = archivesType;
		this.showHistory = history;

		listing = new VLayout();
		detailsContainer = new VLayout();
		details = SELECT_ELEMENT;

		// Initialize the listing panel
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("60%");
		listing.setShowResizeBar(true);

		ListGridField id = new ListGridField("id", 50);
		id.setHidden(true);

		ListGridField name = new ListGridField("name", I18N.message("name"), 250);
		name.setCanFilter(true);

		ListGridField type = new ListGridField("type", I18N.message("type"), 130);
		type.setHidden(true);
		ListGridField typeLabel = new ListGridField("typelabel", I18N.message("type"), 130);
		typeLabel.setCanFilter(false);

		ListGridField status = new ListGridField("statusicon", I18N.message("status"), 50);
		status.setType(ListGridFieldType.IMAGE);
		status.setCanSort(false);
		status.setAlign(Alignment.CENTER);
		status.setShowDefaultContextMenu(false);
		status.setImageURLPrefix(Util.imagePrefix());
		status.setImageURLSuffix(".png");
		status.setCanFilter(false);

		ListGridField created = new ListGridField("created", I18N.message("createdon"), 110);
		created.setAlign(Alignment.CENTER);
		created.setType(ListGridFieldType.DATE);
		created.setCellFormatter(new DateCellFormatter(false));
		created.setCanFilter(false);

		ListGridField creator = new ListGridField("creator", I18N.message("creator"), 110);
		creator.setCanFilter(true);
		ListGridField closer = new ListGridField("closer", I18N.message("closedby"), 110);
		closer.setCanFilter(true);

		ListGridField size = new ListGridField("size", I18N.message("size"), 70);
		size.setAlign(Alignment.CENTER);
		size.setType(ListGridFieldType.FLOAT);
		size.setCellFormatter(new FileSizeCellFormatter());
		size.setCanFilter(false);

		ListGridField aosManager = new ListGridField("aosmanager", I18N.message("aosmanager"), 110);
		aosManager.setCanFilter(true);

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		if (this.archivesType == GUIArchive.TYPE_STORAGE)
			list.setFields(id, created, name, size, status, creator, closer, aosManager);
		else
			list.setFields(id, created, name, size, status, creator, closer);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setFilterOnKeypress(true);
		list.setShowFilterEditor(true);
		if (this.archivesType == GUIArchive.TYPE_STORAGE && this.showHistory)
			list.setDataSource(new ArchivesDS(GUIArchive.MODE_EXPORT, this.archivesType, GUIArchive.STATUS_FINALIZED,
					Session.get().getUser().getId()));
		else
			list.setDataSource(new ArchivesDS(GUIArchive.MODE_EXPORT, this.archivesType, null, Session.get().getUser()
					.getId()));

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
		refresh.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				refresh(ExportArchivesList.this.archivesType, ExportArchivesList.this.showHistory);
			}
		});

		ToolStripButton addArchive = new ToolStripButton();
		addArchive.setTitle(I18N.message("addarchive"));
		addArchive.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onAddingArchive();
				event.cancel();
			}
		});
		toolStrip.addButton(addArchive);

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});

		list.addSelectionChangedHandler(new SelectionChangedHandler() {
			@Override
			public void onSelectionChanged(SelectionEvent event) {
				ListGridRecord record = list.getSelectedRecord();
				try {
					showDetails(Long.parseLong(record.getAttribute("id")), !Integer.toString(GUIArchive.STATUS_OPENED)
							.equals(record.getAttribute("status")));
				} catch (Throwable t) {
				}
			}
		});

		list.addDataArrivedHandler(new DataArrivedHandler() {
			@Override
			public void onDataArrived(DataArrivedEvent event) {
				infoPanel.setMessage(I18N.message("showarchives", Integer.toString(list.getTotalRows())));
			}
		});

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		if (!showHistory)
			setMembers(toolStrip, listing, detailsContainer);
		else
			setMembers(listing, detailsContainer);
	}

	protected void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord record = list.getSelectedRecord();
		final long id = Long.parseLong(record.getAttributeAsString("id"));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							ImpexService.Instance.get().delete(id, new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void result) {
									list.removeSelectedData();
									list.deselectAllRecords();
									showDetails(null, true);
								}
							});
						}
					}
				});
			}
		});

		MenuItem open = new MenuItem();
		open.setTitle(I18N.message("open"));
		open.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				openArchive(record);
			}
		});

		MenuItem close = new MenuItem();
		close.setTitle(I18N.message("close"));
		close.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmarchiveclose"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							onClosingArchive(record, id);
						}
					}
				});
			}
		});

		if (GUIArchive.STATUS_OPENED != Integer.parseInt(record.getAttributeAsString("status")))
			close.setEnabled(false);

		if (GUIArchive.STATUS_ERROR != Integer.parseInt(record.getAttributeAsString("status")))
			open.setEnabled(false);

		contextMenu.setItems(close, open, delete);
		addUsefulMenuItem(record, contextMenu);
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

	protected void closeArchive(final ListGridRecord record) {
		ImpexService.Instance.get().setStatus(Long.parseLong(record.getAttributeAsString("id")),
				GUIArchive.STATUS_CLOSED, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						record.setAttribute("status", "1");
						record.setAttribute("statusicon", "lock");
						list.refreshRow(list.getRecordIndex(record));
						showDetails(Long.parseLong(record.getAttributeAsString("id")), true);
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
	protected Menu addUsefulMenuItem(final ListGridRecord record, Menu contextMenu) {
		// DO NOTHING
		return contextMenu;
	}

	protected void onClosingArchive(final ListGridRecord record, final long id) {
		closeArchive(record);
	}

	protected void openArchive(final ListGridRecord record) {
		ImpexService.Instance.get().setStatus(Long.parseLong(record.getAttributeAsString("id")),
				GUIArchive.STATUS_OPENED, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						record.setAttribute("status", "0");
						record.setAttribute("statusicon", "lock_open");
						list.refreshRow(list.getRecordIndex(record));
						showDetails(Long.parseLong(record.getAttributeAsString("id")), true);
					}
				});
	}
}