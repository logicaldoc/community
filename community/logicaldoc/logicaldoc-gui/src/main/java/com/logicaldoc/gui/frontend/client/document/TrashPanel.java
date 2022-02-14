package com.logicaldoc.gui.frontend.client.document;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.data.GarbageDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.observer.FolderController;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.folder.FolderNavigator;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * This panel shows the current user's garbage
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class TrashPanel extends VLayout {

	private RefreshableListGrid list;

	private static TrashPanel instance;

	public static TrashPanel get() {
		if (instance == null)
			instance = new TrashPanel();
		return instance;
	}

	public TrashPanel() {
		setMembersMargin(3);
	}

	@Override
	public void onDraw() {
		ListGridField id = new ListGridField("id");
		id.setHidden(true);

		FileNameListGridField fileName = new FileNameListGridField();
		fileName.setTitle(I18N.message("name"));
		fileName.setCanFilter(true);
		fileName.setWidth("*");

		DateListGridField lastModified = new DateListGridField("lastModified", "lastmodified");
		lastModified.setHidden(true);

		ListGridField customId = new ColoredListGridField("customId", I18N.message("customid"), 110);
		customId.setType(ListGridFieldType.TEXT);
		customId.setCanFilter(true);
		customId.setHidden(true);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setWidth100();
		list.setHeight100();
		list.setAutoFetchData(true);
		list.setFields(fileName, customId, lastModified);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setDataSource(new GarbageDS());
		list.setShowFilterEditor(true);
		list.setFilterOnKeypress(true);
		addMember(list);

		if (FolderController.get().getCurrentFolder() != null && FolderController.get().getCurrentFolder().isWrite())
			list.addCellContextClickHandler(new CellContextClickHandler() {
				@Override
				public void onCellContextClick(CellContextClickEvent event) {
					showContextMenu();
					event.cancel();
				}
			});
	}

	public void refresh() {
		if (list != null)
			list.refresh(new GarbageDS());
	}

	private void restoreDocument(final long id) {
		DocumentService.Instance.get().restore(new Long[] { id }, FolderController.get().getCurrentFolder().getId(),
				new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void ret) {
						list.removeSelectedData();
						GuiLog.info(I18N.message("documentrestored"),
								I18N.message("documentrestoreddetail", Long.toString(id)));

						// Force a refresh
						FolderController.get().selected(FolderController.get().getCurrentFolder());
					}
				});
	}

	private void restoreFolder(final long id) {
		FolderService.Instance.get().restore(new Long[] { id }, FolderController.get().getCurrentFolder().getId(),
				new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void ret) {
						list.removeSelectedData();
						GuiLog.info(I18N.message("folderrestored"),
								I18N.message("folderrestoreddetail", Long.toString(id)));

						// Force a reload
						FolderNavigator.get().reload();
					}
				});
	}

	private void showContextMenu() {
		final ListGridRecord[] records = list.getSelectedRecords();

		Menu contextMenu = new Menu();

		MenuItem restore = new MenuItem();
		restore.setTitle(I18N.message("restore"));
		restore.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {

				if ("document".equals(records[0].getAttribute("type")))
					restoreDocument(Long.parseLong(records[0].getAttribute("id")));
				else
					restoreFolder(Long.parseLong(records[0].getAttribute("id")));
			}
		});

		MenuItem remove = new MenuItem();
		remove.setTitle(I18N.message("remove"));
		remove.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				final List<Long> docIds = new ArrayList<Long>();
				final List<Long> folderIds = new ArrayList<Long>();
				for (ListGridRecord record : records) {
					if ("document".equals(record.getAttribute("type")))
						docIds.add(Long.parseLong(record.getAttribute("id")));
					else
						folderIds.add(Long.parseLong(record.getAttribute("id")));
				}

				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							if (!docIds.isEmpty())
								DocumentService.Instance.get().deleteFromTrash(docIds.toArray(new Long[0]),
										new AsyncCallback<Void>() {

											@Override
											public void onFailure(Throwable caught) {
												GuiLog.serverError(caught);
											}

											@Override
											public void onSuccess(Void arg) {
												refresh();
											}
										});
							if (!folderIds.isEmpty())
								FolderService.Instance.get().deleteFromTrash(folderIds.toArray(new Long[0]),
										new AsyncCallback<Void>() {

											@Override
											public void onFailure(Throwable caught) {
												GuiLog.serverError(caught);
											}

											@Override
											public void onSuccess(Void arg) {
												refresh();
											}
										});
						}
					}
				});
			}
		});

		MenuItem emptyTrash = new MenuItem();
		emptyTrash.setTitle(I18N.message("emptytrash"));
		emptyTrash.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmemptytrash"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							DocumentService.Instance.get().emptyTrash(new AsyncCallback<Void>() {

								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(Void arg) {
									refresh();
								}
							});
						}
					}
				});
			}
		});

		restore.setEnabled(records.length == 1);

		contextMenu.setItems(restore, remove, emptyTrash);
		contextMenu.showContextMenu();
	}
}