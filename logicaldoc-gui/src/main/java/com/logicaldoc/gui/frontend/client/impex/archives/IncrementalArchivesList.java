package com.logicaldoc.gui.frontend.client.impex.archives;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIArchive;
import com.logicaldoc.gui.common.client.beans.GUIIncrementalArchive;
import com.logicaldoc.gui.common.client.data.IncrementalArchivesDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.widgets.grid.formatters.DaysCellFormatter;
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
public class IncrementalArchivesList extends VLayout {

	private static final String FREQUENCY = "frequency";

	private static final String PREFIX = "prefix";

	protected Layout detailsContainer;

	protected RefreshableListGrid list;

	protected Canvas details = SELECT_ELEMENT;

	static final Canvas SELECT_ELEMENT = new HTMLPanel("&nbsp;" + I18N.message("selectconfig"));

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

		ListGridField prefix = new ListGridField(PREFIX, I18N.message(PREFIX), 250);

		ListGridField type = new ListGridField("typelabel", I18N.message("type"), 130);
		type.setCanFilter(false);

		ListGridField frequency = new ListGridField(FREQUENCY, I18N.message(FREQUENCY), 110);
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
		refresh.addClickHandler(event -> refresh(IncrementalArchivesList.this.archivesType));

		ToolStripButton addIncremental = new ToolStripButton();
		addIncremental.setTitle(I18N.message("addincremental"));
		addIncremental.addClickHandler(event -> {
			onAddingIncrementalArchive();
			event.cancel();
		});
		toolStrip.addButton(addIncremental);

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		list.addSelectionChangedHandler(event -> {
			ListGridRecord rec = list.getSelectedRecord();
			if (rec == null)
				return;
			ImpexService.Instance.get().loadIncremental(Long.parseLong(rec.getAttributeAsString("id")),
					new DefaultAsyncCallback<>() {
						@Override
						public void onSuccess(GUIIncrementalArchive result) {
							showDetails(result);
						}
					});
		});

		list.addDataArrivedHandler(
				event -> infoPanel.setMessage(I18N.message("showincremental", Integer.toString(list.getTotalRows()))));

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

		final ListGridRecord rec = list.getSelectedRecord();
		final long id = Long.parseLong(rec.getAttributeAsString("id"));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm)) {
				ImpexService.Instance.get().deleteIncremental(id, new DefaultAsyncCallback<>() {
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
	 * Updates the selected rec with new data
	 * 
	 * @param incremental the archive to update
	 */
	public void updateRecord(GUIIncrementalArchive incremental) {
		ListGridRecord rec = list.getSelectedRecord();
		if (rec == null)
			rec = new ListGridRecord();

		rec.setAttribute(PREFIX, incremental.getPrefix());
		rec.setAttribute(FREQUENCY, incremental.getFrequency());
		rec.setAttribute("type", incremental.getType());
		rec.setAttribute("typelabel", incremental.getType() == GUIArchive.TYPE_DEFAULT ? I18N.message("default")
				: I18N.message("paperdematerialization"));

		if (rec.getAttributeAsString("id") != null
				&& (incremental.getId() == Long.parseLong(rec.getAttributeAsString("id")))) {
			list.refreshRow(list.getRecordIndex(rec));
		} else {
			// Append a new rec
			rec.setAttribute("id", incremental.getId());
			list.refreshRow(list.getRecordIndex(rec));
			list.selectRecord(rec);
		}
	}

	protected void onAddingIncrementalArchive() {
		list.deselectAllRecords();
		GUIIncrementalArchive archive = new GUIIncrementalArchive();
		archive.setType(IncrementalArchivesList.this.archivesType);
		showDetails(archive);
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