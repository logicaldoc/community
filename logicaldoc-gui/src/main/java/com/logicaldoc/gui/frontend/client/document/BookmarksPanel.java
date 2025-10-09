package com.logicaldoc.gui.frontend.client.document;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIBookmark;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.BookmarksDS;
import com.logicaldoc.gui.common.client.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.validators.MinLengthValidator;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.ListGridEditEvent;
import com.smartgwt.client.widgets.form.validator.LengthRangeValidator;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;

/**
 * This panel shows the current user's bookmarks
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class BookmarksPanel extends VLayout {

	private static final String TARGET_ID = "targetId";

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
		ListGridField id = new IdListGridField();

		LengthRangeValidator validator = new MinLengthValidator(1);

		FileNameListGridField name = new FileNameListGridField("name", "icon", I18N.message("name"), 200);
		name.setWidth("*");
		name.setValidators(validator);

		ListGridField description = new ColoredListGridField("description");
		description.setValidators(validator);
		description.setHidden(true);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanEdit(false);
		list.setEditEvent(ListGridEditEvent.DOUBLECLICK);
		list.setWidth100();
		list.setHeight100();
		list.setAutoFetchData(true);
		list.setFields(id, name, description);
		list.setDataSource(new BookmarksDS());
		list.setShowFilterEditor(true);
		list.setFilterOnKeypress(true);
		list.setCanEdit(false);
		addMember(list);

		list.addCellContextClickHandler(event -> {
			final ListGridRecord rec = list.getSelectedRecord();
			FolderService.Instance.get().getFolder(Long.parseLong(rec.getAttributeAsString("folderId")), false, false,
					false, new DefaultAsyncCallback<>() {
						@Override
						public void handleSuccess(GUIFolder folder) {
							showContextMenu(folder, rec.getAttributeAsString("type").equals("0"));
						}
					});
			event.cancel();
		});

		list.addClickHandler(event -> onBookmarkSelected());
	}

	private void showContextMenu(GUIFolder folder, boolean isDocument) {
		Menu contextMenu = new Menu();

		MenuItem edit = new MenuItem();
		edit.setTitle(I18N.message("edit"));
		edit.addClickHandler(event -> {
			ListGridRecord rec = list.getSelectedRecord();
			GUIBookmark bookmark = new GUIBookmark();
			bookmark.setId(Long.parseLong(rec.getAttributeAsString("id")));
			bookmark.setName(rec.getAttributeAsString("name"));
			bookmark.setDescription(rec.getAttributeAsString("description"));
			BookmarkDialog dialog = new BookmarkDialog(bookmark);
			dialog.show();
		});

		MenuItem download = new MenuItem();
		download.setTitle(I18N.message("download"));
		download.addClickHandler(event -> download());

		MenuItem openInFolder = new MenuItem();
		openInFolder.setTitle(I18N.message("openinfolder"));
		openInFolder.addClickHandler(event -> onBookmarkSelected());

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> {
			final ListGridRecord[] selection = list.getSelectedRecords();
			if (selection == null || selection.length == 0)
				return;

			LD.ask(I18N.message("question"), I18N.message("confirmdelete"), answer -> {
				if (Boolean.TRUE.equals(answer)) {
					DocumentService.Instance.get().deleteBookmarks(GridUtil.getIds(selection),
							new DefaultAsyncCallback<>() {
								@Override
								public void handleSuccess(Void result) {
									list.removeSelectedData();
								}
							});
				}
			});
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
			contextMenu.setItems(edit, download, new MenuItemSeparator(), delete, new MenuItemSeparator(),
					openInFolder);
		else
			contextMenu.setItems(edit, new MenuItemSeparator(), delete, new MenuItemSeparator(), openInFolder);
		contextMenu.showContextMenu();
	}

	public void refresh() {
		if (list != null)
			list.refresh(new BookmarksDS());
	}

	private void download() {
		Long id = list.getSelectedRecord().getAttributeAsLong(TARGET_ID);
		DocUtil.download(id, null);
	}

	private void onBookmarkSelected() {
		ListGridRecord rec = list.getSelectedRecord();
		if (rec.getAttributeAsString("type").equals("0"))
			DocumentsPanel.get().openInFolder(rec.getAttributeAsLong("folderId"), rec.getAttributeAsLong(TARGET_ID));
		else
			DocumentsPanel.get().openInFolder(rec.getAttributeAsLong(TARGET_ID), null);
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