package com.logicaldoc.gui.frontend.client.impex.archives;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.data.ImpexFoldersDS;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.frontend.client.services.ImpexService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.SelectionStyle;
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
 * Panel showing the list of archive import folders
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ImportArchiveFoldersList extends VLayout {

	private RefreshableListGrid list;

	private ImportArchivesPanel archivesPanel = null;

	public ImportArchiveFoldersList(ImportArchivesPanel panel) {
		setWidth100();
		this.archivesPanel = panel;
	}

	@Override
	public void onDraw() {
		final InfoPanel infoPanel = new InfoPanel("");

		Layout listing = new VLayout();

		// Initialize the listing panel
		listing.setAlign(Alignment.CENTER);

		ListGridField name = new ListGridField("name", I18N.message("folder"), 250);

		ListGridField date = new DateListGridField("date", "date");

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		list.setFields(date, name);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setFilterOnKeypress(true);
		list.setShowFilterEditor(true);
		list.setDataSource(new ImpexFoldersDS());

		listing.addMember(infoPanel);
		listing.addMember(list);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		toolStrip.addButton(refresh);
		refresh.addClickHandler(event -> list.refresh(new ImpexFoldersDS()));

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		list.addDataArrivedHandler(
				event -> infoPanel.setMessage(I18N.message("showfolders", Integer.toString(list.getTotalRows()))));

		setMembers(toolStrip, listing);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord rec = list.getSelectedRecord();
		final String name = rec.getAttributeAsString("name");

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm)) {
				ImpexService.Instance.get().deleteFolder(name, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						list.removeSelectedData();
						list.deselectAllRecords();
					}
				});
			}
		}));

		MenuItem importBundle = new MenuItem();
		importBundle.setTitle(I18N.message("importbundle"));
		importBundle.addClickHandler(
				event -> LD.ask(I18N.message("question"), I18N.message("confirmimportbundle"), confirm -> {
					if (Boolean.TRUE.equals(confirm)) {
						ImpexService.Instance.get().startImport(name, new AsyncCallback<>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.warn(I18N.message("archiveimportingerror"), null);
							}

							@Override
							public void onSuccess(Void result) {
								list.removeSelectedData();
								list.deselectAllRecords();
								archivesPanel.onConfirmImportBundle();
							}
						});
					}
				}));

		contextMenu.setItems(importBundle, delete);
		contextMenu.showContextMenu();
	}

	public ListGrid getList() {
		return list;
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