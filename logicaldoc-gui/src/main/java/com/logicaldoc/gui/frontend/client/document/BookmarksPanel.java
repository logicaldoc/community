package com.logicaldoc.gui.frontend.client.document;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIBookmark;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.BookmarksDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.ListGridEditEvent;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.validator.LengthRangeValidator;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * This panel shows the current user's bookmarks
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class BookmarksPanel extends VLayout {

	private RefreshableListGrid list;

	private static BookmarksPanel instance;

	public static BookmarksPanel get() {
		if (instance == null)
			instance = new BookmarksPanel();
		return instance;
	}

	public BookmarksPanel() {
		setMembersMargin(3);
	}

	@Override
	public void onDraw() {
		ListGridField id = new ListGridField("id");
		id.setHidden(true);

		LengthRangeValidator validator = new LengthRangeValidator();
		validator.setMin(1);

		FileNameListGridField name = new FileNameListGridField("name", "icon", I18N.message("name"), 200);
		name.setWidth("*");
		name.setValidators(validator);

		ListGridField description = new ColoredListGridField("description", I18N.message("description"));
		description.setValidators(validator);
		description.setHidden(true);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanEdit(false);
		list.setEditEvent(ListGridEditEvent.DOUBLECLICK);
		list.setWidth100();
		list.setHeight100();
		list.setAutoFetchData(true);
		list.setFields(name, description);
		list.setDataSource(new BookmarksDS());
		list.setShowFilterEditor(true);
		list.setFilterOnKeypress(true);
		list.setCanEdit(false);
		addMember(list);

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				final ListGridRecord rec = list.getSelectedRecord();
				FolderService.Instance.get().getFolder(Long.parseLong(rec.getAttributeAsString("folderId")), false,
						false, false, new AsyncCallback<GUIFolder>() {

							@Override
							public void onSuccess(GUIFolder folder) {
								showContextMenu(folder, rec.getAttributeAsString("type").equals("0"));
							}

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}
						});
				event.cancel();
			}
		});

		list.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onBookmarkSelected();
			}
		});
	}

	private void showContextMenu(GUIFolder folder, boolean isDocument) {
		Menu contextMenu = new Menu();

		MenuItem edit = new MenuItem();
		edit.setTitle(I18N.message("edit"));
		edit.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				ListGridRecord rec = list.getSelectedRecord();
				GUIBookmark bookmark = new GUIBookmark();
				bookmark.setId(Long.parseLong(rec.getAttributeAsString("id")));
				bookmark.setName(rec.getAttributeAsString("name"));
				bookmark.setDescription(rec.getAttributeAsString("description"));
				BookmarkDialog dialog = new BookmarkDialog(bookmark);
				dialog.show();
			}
		});

		MenuItem download = new MenuItem();
		download.setTitle(I18N.message("download"));
		download.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				download();
			}
		});

		MenuItem openInFolder = new MenuItem();
		openInFolder.setTitle(I18N.message("openinfolder"));
		openInFolder.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				onBookmarkSelected();
			}
		});

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				final ListGridRecord[] selection = list.getSelectedRecords();
				if (selection == null || selection.length == 0)
					return;
				final long[] ids = new long[selection.length];
				for (int i = 0; i < selection.length; i++) {
					ids[i] = Long.parseLong(selection[i].getAttribute("id"));
				}

				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							DocumentService.Instance.get().deleteBookmarks(ids, new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(Void result) {
									list.removeSelectedData();
								}
							});
						}
					}
				});
			}
		});

		if (!(list.getSelectedRecords() != null && list.getSelectedRecords().length == 1)) {
			download.setEnabled(false);
			openInFolder.setEnabled(false);
		}

		if (folder != null && !folder.isDownload())
			download.setEnabled(false);
		else if (folder == null) {
			download.setEnabled(false);
			openInFolder.setEnabled(false);
		}

		if (isDocument)
			contextMenu.setItems(edit, download, delete, openInFolder);
		else
			contextMenu.setItems(edit, delete, openInFolder);
		contextMenu.showContextMenu();
	}

	public void refresh() {
		if (list != null)
			list.refresh(new BookmarksDS());
	}

	private void download() {
		Long id = list.getSelectedRecord().getAttributeAsLong("targetId");
		DocUtil.download(id, null);
	}

	private void onBookmarkSelected() {
		ListGridRecord rec = list.getSelectedRecord();
		if (rec.getAttributeAsString("type").equals("0"))
			DocumentsPanel.get().openInFolder(rec.getAttributeAsLong("folderId"),
					rec.getAttributeAsLong("targetId"));
		else
			DocumentsPanel.get().openInFolder(rec.getAttributeAsLong("targetId"), null);
	}
}