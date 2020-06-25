package com.logicaldoc.gui.frontend.client.impex.archives;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIArchive;
import com.logicaldoc.gui.common.client.beans.GUIIncrementalArchive;
import com.logicaldoc.gui.common.client.data.IncrementalArchivesDS;
import com.logicaldoc.gui.common.client.formatters.DaysCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.common.client.widgets.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.services.ImpexService;
import com.smartgwt.client.types.Alignment;
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
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class IncrementalArchivesList extends VLayout {

	protected Layout detailsContainer;

	protected RefreshableListGrid list;

	protected Canvas details = SELECT_ELEMENT;

	final static Canvas SELECT_ELEMENT = new HTMLPanel("&nbsp;" + I18N.message("selectconfig"));

	protected int archivesType = GUIArchive.TYPE_DEFAULT;

	public IncrementalArchivesList(int archivesType) {
		setWidth100();
		this.archivesType = archivesType;
	}

	@Override
	public void onDraw() {
		final InfoPanel infoPanel = new InfoPanel("");

		VLayout listing = new VLayout();
		detailsContainer = new VLayout();
		details = SELECT_ELEMENT;

		// Initialize the listing panel
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("65%");
		listing.setShowResizeBar(true);

		ListGridField id = new ListGridField("id", 50);
		id.setHidden(true);

		ListGridField prefix = new ListGridField("prefix", I18N.message("prefix"), 250);

		ListGridField type = new ListGridField("typelabel", I18N.message("type"), 130);
		type.setCanFilter(false);

		ListGridField frequency = new ListGridField("frequency", I18N.message("frequency"), 110);
		frequency.setCellFormatter(new DaysCellFormatter());
		frequency.setCanFilter(false);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		list.setFields(id, prefix, frequency);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setFilterOnKeypress(true);
		list.setShowFilterEditor(true);
		list.setDataSource(new IncrementalArchivesDS(archivesType));

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
				refresh(IncrementalArchivesList.this.archivesType);
			}
		});

		ToolStripButton addIncremental = new ToolStripButton();
		addIncremental.setTitle(I18N.message("addincremental"));
		addIncremental.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onAddingIncrementalArchive();
				event.cancel();
			}
		});
		toolStrip.addButton(addIncremental);

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
				if (record == null)
					return;
				ImpexService.Instance.get().loadIncremental(Long.parseLong(record.getAttributeAsString("id")),
						new AsyncCallback<GUIIncrementalArchive>() {
							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(GUIIncrementalArchive result) {
								showDetails(result);
							}
						});
			}
		});

		list.addDataArrivedHandler(new DataArrivedHandler() {
			@Override
			public void onDataArrived(DataArrivedEvent event) {
				infoPanel.setMessage(I18N.message("showincremental", Integer.toString(list.getTotalRows())));
			}
		});

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		setMembers(toolStrip, listing, detailsContainer);
	}

	public void refresh() {
		list.refresh(new IncrementalArchivesDS(archivesType));
		detailsContainer.removeMembers(detailsContainer.getMembers());
		details = SELECT_ELEMENT;
		detailsContainer.setMembers(details);
	}

	public void refresh(int archivesType) {
		this.archivesType = archivesType;
		refresh();
	}

	private void showContextMenu() {
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
							ImpexService.Instance.get().deleteIncremental(id, new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void result) {
									list.removeSelectedData();
									list.deselectAllRecords();
									showDetails(null);
								}
							});
						}
					}
				});
			}
		});
		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}

	protected void showDetails(GUIIncrementalArchive incremental) {
		if (details != null)
			detailsContainer.removeMember(details);
		if (incremental != null)
			details = new IncrementalDetailsPanel(incremental, this);
		else
			details = SELECT_ELEMENT;
		detailsContainer.addMember(details);
	}

	public ListGrid getList() {
		return list;
	}

	/**
	 * Updates the selected record with new data
	 * 
	 * @param incremental the archive to update
	 */
	public void updateRecord(GUIIncrementalArchive incremental) {
		ListGridRecord record = list.getSelectedRecord();
		if (record == null)
			record = new ListGridRecord();

		record.setAttribute("prefix", incremental.getPrefix());
		record.setAttribute("frequency", incremental.getFrequency());
		record.setAttribute("type", incremental.getType());
		record.setAttribute("typelabel", incremental.getType() == GUIArchive.TYPE_DEFAULT ? I18N.message("default")
				: I18N.message("paperdematerialization"));

		if (record.getAttributeAsString("id") != null
				&& (incremental.getId() == Long.parseLong(record.getAttributeAsString("id")))) {
			list.refreshRow(list.getRecordIndex(record));
		} else {
			// Append a new record
			record.setAttribute("id", incremental.getId());
			list.refreshRow(list.getRecordIndex(record));
			list.selectRecord(record);
		}
	}

	protected void onAddingIncrementalArchive() {
		list.deselectAllRecords();
		GUIIncrementalArchive archive = new GUIIncrementalArchive();
		archive.setType(IncrementalArchivesList.this.archivesType);
		showDetails(archive);
	}
}