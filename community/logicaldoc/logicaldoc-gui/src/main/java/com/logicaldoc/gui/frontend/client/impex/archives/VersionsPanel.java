package com.logicaldoc.gui.frontend.client.impex.archives;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIArchive;
import com.logicaldoc.gui.common.client.data.VersionsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileSizeListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.VersionListGridField;
import com.logicaldoc.gui.frontend.client.services.ImpexService;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickEvent;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows a list of versions of an archive in a tabular way.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class VersionsPanel extends VLayout {

	private VersionsDS dataSource;

	private ListGrid listGrid;

	private int max = 100;

	private ToolStrip toolbar = new ToolStrip();

	private ExportArchivesList archivesList = null;

	public VersionsPanel(ExportArchivesList list, final Long archiveId, final boolean readonly) {
		this.archivesList = list;

		final IntegerItem maxItem = ItemFactory.newValidateIntegerItem("max", "", null, 1, null);
		maxItem.setHint(I18N.message("elements"));
		maxItem.setShowTitle(false);
		maxItem.setDefaultValue(100);
		maxItem.setWidth(40);

		toolbar.setWidth100();

		ToolStripButton display = new ToolStripButton();
		display.setTitle(I18N.message("display"));
		toolbar.addButton(display);
		toolbar.addFormItem(maxItem);
		display.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (maxItem.validate() && maxItem.getValue() != null) {
					if (maxItem.getValue() instanceof Integer)
						max = (Integer) maxItem.getValue();
					else
						max = Integer.parseInt(maxItem.getValue().toString());
					initListGrid(archiveId, readonly);
				}
			}
		});
		toolbar.addFill();

		addMember(toolbar);
		initListGrid(archiveId, readonly);
	}

	private void initListGrid(final Long archiveId, final boolean readonly) {
		if (listGrid != null)
			removeMember(listGrid);

		ListGridField id = new ListGridField("id", 80);
		id.setHidden(true);

		ListGridField docid = new ListGridField("docid", I18N.message("id"), 80);

		ListGridField customid = new ListGridField("customid", I18N.message("customid"), 100);
		FileNameListGridField fileName = new FileNameListGridField();

		ListGridField version = new VersionListGridField();

		ListGridField date = new DateListGridField("date", "date");

		ListGridField size = new FileSizeListGridField("size", I18N.message("size"));
		size.setCanFilter(false);

		ListGridField template = new ListGridField("template", I18N.message("template"), 200);

		listGrid = new ListGrid();
		listGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		listGrid.setCanFreezeFields(true);
		listGrid.setAutoFetchData(true);
		dataSource = new VersionsDS(null, archiveId, max);
		listGrid.setDataSource(dataSource);
		listGrid.setFields(id, docid, customid, fileName, version, date, size, template);
		addMember(listGrid, 1);

		listGrid.addCellDoubleClickHandler(new CellDoubleClickHandler() {
			@Override
			public void onCellDoubleClick(CellDoubleClickEvent event) {
				ListGridRecord record = event.getRecord();
				DocUtil.download(record.getAttributeAsLong("docid"), null, record.getAttribute("id"));
			}
		});

		listGrid.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				Menu contextMenu = setupContextMenu(archiveId, readonly);
				contextMenu.showContextMenu();
				event.cancel();
			}
		});
	}

	/**
	 * Prepares the context menu.
	 */
	private Menu setupContextMenu(final long archiveId, boolean readonly) {
		final ListGridRecord[] selection = listGrid.getSelectedRecords();

		Menu contextMenu = new Menu();

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				if (selection == null || selection.length == 0)
					return;
				final Long[] ids = new Long[selection.length];
				for (int i = 0; i < selection.length; i++) {
					ids[i] = Long.parseLong(selection[i].getAttribute("id"));
				}

				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							listGrid.removeSelectedData();
							listGrid.deselectAllRecords();

							ImpexService.Instance.get().deleteVersions(archiveId, ids, new AsyncCallback<GUIArchive>() {

								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(GUIArchive archive) {
									ListGridRecord selectedRecord = archivesList.getList().getSelectedRecord();
									if (selectedRecord != null) {
										selectedRecord.setAttribute("size", archive.getSize());
										archivesList.getList()
												.refreshRow(archivesList.getList().getRecordIndex(selectedRecord));
									}
								}
							});
						}
					}
				});
			}
		});

		if (readonly)
			delete.setEnabled(false);

		contextMenu.setItems(delete);
		return contextMenu;
	}

	@Override
	public void destroy() {
		super.destroy();
		if (dataSource != null)
			dataSource.destroy();
	}
}