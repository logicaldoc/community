package com.logicaldoc.gui.frontend.client.document;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIBookmark;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.BookmarksDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridEditEvent;
import com.smartgwt.client.types.ListGridFieldType;
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

		ListGridField name = new ListGridField("name", I18N.message("name"));
		name.setValidators(validator);
		name.setWidth("*");

		ListGridField description = new ListGridField("description", I18N.message("description"));
		description.setValidators(validator);
		description.setHidden(true);

		ListGridField icon = new ListGridField("icon", " ", 24);
		icon.setType(ListGridFieldType.IMAGE);
		icon.setCanSort(false);
		icon.setAlign(Alignment.CENTER);
		icon.setShowDefaultContextMenu(false);
		icon.setImageURLPrefix(Util.imagePrefix());
		icon.setImageURLSuffix(".png");
		icon.setCanEdit(false);
		icon.setCanFilter(false);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanEdit(false);
		list.setEditEvent(ListGridEditEvent.DOUBLECLICK);
		list.setWidth100();
		list.setHeight100();
		list.setAutoFetchData(true);
		list.setFields(icon, name, description);
		list.setDataSource(new BookmarksDS());
		list.setShowFilterEditor(true);
		list.setFilterOnKeypress(true);
		list.setCanEdit(false);
		addMember(list);

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				final ListGridRecord record = list.getSelectedRecord();
				FolderService.Instance.get().getFolder(Long.parseLong(record.getAttributeAsString("folderId")), false,
						false, false, new AsyncCallback<GUIFolder>() {

							@Override
							public void onSuccess(GUIFolder folder) {
								showContextMenu(folder, record.getAttributeAsString("type").equals("0"));
							}

							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
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
				ListGridRecord record = list.getSelectedRecord();
				GUIBookmark bookmark = new GUIBookmark();
				bookmark.setId(Long.parseLong(record.getAttributeAsString("id")));
				bookmark.setName(record.getAttributeAsString("name"));
				bookmark.setDescription(record.getAttributeAsString("description"));
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
									Log.serverError(caught);
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
		ListGridRecord record = list.getSelectedRecord();
		if (record.getAttributeAsString("type").equals("0"))
			DocumentsPanel.get().openInFolder(record.getAttributeAsLong("folderId"),
					record.getAttributeAsLong("targetId"));
		else
			DocumentsPanel.get().openInFolder(record.getAttributeAsLong("targetId"), null);
	}
}