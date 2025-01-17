package com.logicaldoc.gui.frontend.client.impex.folders;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIImportFolder;
import com.logicaldoc.gui.common.client.data.ImportFoldersDS;
import com.logicaldoc.gui.common.client.grid.EnabledListGridField;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.IntegerListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.ImportFolderService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Panel showing the details of an import folder
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class ImportFoldersPanel extends AdminPanel {

	private static final String QUESTION = "question";

	private static final String ENABLED = "eenabled";

	private Layout detailsContainer = new VLayout();

	private RefreshableListGrid list;

	private Canvas details = SELECT_FOLDER;

	static final Canvas SELECT_FOLDER = new HTMLPanel("&nbsp;" + I18N.message("selectimportfolder"));

	public ImportFoldersPanel() {
		super("importfolders");
	}

	@Override
	public void onDraw() {
		final InfoPanel infoPanel = new InfoPanel("");

		// Initialize the listing panel
		Layout listing = new VLayout();
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("60%");
		listing.setShowResizeBar(true);

		ListGridField id = new IdListGridField();

		ListGridField targetFolderId = new ListGridField("targetFolderId", I18N.message("targetid"), 50);
		targetFolderId.setHidden(true);

		ListGridField src = new ListGridField("src", I18N.message("source"), 300);
		src.setCanFilter(true);

		ListGridField type = new ListGridField("type", I18N.message("type"));
		type.setCanFilter(false);
		type.setAutoFitWidth(true);

		IntegerListGridField importedDocs = new IntegerListGridField("docs", I18N.message("importeddocuments"));
		importedDocs.setAutoFitWidth(true);

		ListGridField enabled = new EnabledListGridField();

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		list.setFields(enabled, id, targetFolderId, src, type, importedDocs);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setFilterOnKeypress(true);
		list.setDataSource(new ImportFoldersDS());

		listing.addMember(infoPanel);
		listing.addMember(list);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		toolStrip.addButton(refresh);
		refresh.addClickHandler((ClickEvent event) -> refresh());

		ToolStripButton addImportFolder = new ToolStripButton();
		addImportFolder.setTitle(I18N.message("addimportfolder"));
		addImportFolder.addClickHandler((ClickEvent event) -> {
			list.deselectAllRecords();
			GUIImportFolder share = new GUIImportFolder();
			share.setProvider("file");
			showShareDetails(share);
		});
		if (Feature.enabled(Feature.IMPORT_LOCAL_FOLDERS) || Feature.enabled(Feature.IMPORT_REMOTE_FOLDERS))
			toolStrip.addButton(addImportFolder);

		list.addCellContextClickHandler((CellContextClickEvent event) -> {
			showContextMenu();
			event.cancel();
		});

		list.addSelectionChangedHandler((SelectionEvent event) -> {
			Record rec = list.getSelectedRecord();
			if (rec != null)
				ImportFolderService.Instance.get().getImportFolder(Long.parseLong(rec.getAttributeAsString("id")),
						new DefaultAsyncCallback<>() {
							@Override
							public void onSuccess(GUIImportFolder share) {
								showShareDetails(share);
							}
						});
		});

		list.addDataArrivedHandler((DataArrivedEvent event) -> infoPanel
				.setMessage(I18N.message("showimportfolders", Integer.toString(list.getTotalRows()))));

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		body.setMembers(toolStrip, listing, detailsContainer);
	}

	public void refresh() {
		list.refresh(new ImportFoldersDS());
		detailsContainer.removeMembers(detailsContainer.getMembers());
		details = SELECT_FOLDER;
		detailsContainer.setMembers(details);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord rec = list.getSelectedRecord();
		final long id = Long.parseLong(rec.getAttributeAsString("id"));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(
				event -> LD.ask(I18N.message(QUESTION), I18N.message("confirmdelete"), (Boolean value) -> {
					if (Boolean.TRUE.equals(value)) {
						ImportFolderService.Instance.get().delete(id, new DefaultAsyncCallback<>() {
							@Override
							public void onSuccess(Void result) {
								list.removeSelectedData();
								list.deselectAllRecords();
								showShareDetails(null);
							}
						});
					}
				}));

		MenuItem test = new MenuItem();
		test.setTitle(I18N.message("testconnection"));
		test.addClickHandler(event -> ImportFolderService.Instance.get()
				.test(Long.parseLong(rec.getAttributeAsString("id")), new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Boolean result) {
						if (Boolean.TRUE.equals(result))
							SC.say(I18N.message("connectionestablished"));
						else
							SC.warn(I18N.message("connectionfailed"));
					}
				}));

		MenuItem enable = new MenuItem();
		enable.setTitle(I18N.message("enable"));
		enable.setEnabled(!rec.getAttributeAsBoolean(ENABLED));
		enable.addClickHandler(event -> ImportFolderService.Instance.get()
				.changeStatus(Long.parseLong(rec.getAttributeAsString("id")), true, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(ENABLED, true);
						list.refreshRow(list.getRecordIndex(rec));
					}
				}));

		MenuItem disable = new MenuItem();
		disable.setTitle(I18N.message("disable"));
		disable.setEnabled(rec.getAttributeAsBoolean(ENABLED));
		disable.addClickHandler(event -> ImportFolderService.Instance.get()
				.changeStatus(Long.parseLong(rec.getAttributeAsString("id")), false, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(ENABLED, false);
						list.refreshRow(list.getRecordIndex(rec));
					}
				}));

		MenuItem resetCache = new MenuItem();
		resetCache.setTitle(I18N.message("resetcache"));
		resetCache.addClickHandler(event -> LD.ask(I18N.message(QUESTION), I18N.message("confirmresetcache"), value -> {
			if (Boolean.TRUE.equals(value)) {
				ImportFolderService.Instance.get().resetCache(id, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						GuiLog.info(I18N.message("cachedeleted"), null);
					}
				});
			}
		}));

		MenuItem resetCounter = new MenuItem();
		resetCounter.setTitle(I18N.message("resetcounter"));
		resetCounter
				.addClickHandler(event -> LD.ask(I18N.message(QUESTION), I18N.message("confirmresetcounter"), value -> {
					if (Boolean.TRUE.equals(value)) {
						ImportFolderService.Instance.get().resetCounter(id, new DefaultAsyncCallback<>() {
							@Override
							public void onSuccess(Void result) {
								GuiLog.info(I18N.message("counterreseted"), null);
								rec.setAttribute("docs", "0");
								list.refreshRow(list.getRecordIndex(rec));
							}
						});
					}
				}));

		contextMenu.setItems(test, enable, disable, delete, resetCache, resetCounter);
		contextMenu.showContextMenu();
	}

	public void showShareDetails(GUIImportFolder share) {
		if (!(details instanceof ImportFolderDetailsPanel)) {
			detailsContainer.removeMember(details);
			details = new ImportFolderDetailsPanel(this);
			detailsContainer.addMember(details);
		}
		((ImportFolderDetailsPanel) details).setShare(share);
	}

	public ListGrid getList() {
		return list;
	}

	/**
	 * Updates the selected rec with new data
	 * 
	 * @param importFolder the import folder to update
	 */
	public void updateRecord(GUIImportFolder importFolder) {
		Record rec = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, importFolder.getId()));
		if (rec == null) {
			rec = new ListGridRecord();
			// Append a new rec
			rec.setAttribute("id", importFolder.getId());
			list.addData(rec);
			list.selectRecord(rec);
		}

		rec.setAttribute("src", importFolder.getDisplayUrl());

		rec.setAttribute(ENABLED, importFolder.getEnabled() == 1 ? "0" : "2");

		String type = I18N.message("localfolder");
		if (importFolder.getProvider().startsWith("smb"))
			type = I18N.message(importFolder.getProvider() + "share");
		else if ("ftp".equals(importFolder.getProvider()))
			type = I18N.message("fftp");
		else if ("ftps".equals(importFolder.getProvider()))
			type = I18N.message("ftps");
		else if ("sftp".equals(importFolder.getProvider()))
			type = I18N.message("sftp");

		rec.setAttribute("type", type);

		list.refreshRow(list.getRecordIndex(rec));
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