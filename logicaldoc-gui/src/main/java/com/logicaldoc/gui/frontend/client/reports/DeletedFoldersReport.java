package com.logicaldoc.gui.frontend.client.reports;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.DeletedFoldersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.FolderChangeListener;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.common.client.widgets.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FolderListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.folder.RestoreDialog;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows a list of deleted folders
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 */
public class DeletedFoldersReport extends AdminPanel implements FolderChangeListener {

	private RefreshableListGrid list;

	private SelectItem userSelector;

	private FolderSelector folderSelector;

	public DeletedFoldersReport() {
		super("deletedfolders");
	}

	@Override
	public void onDraw() {
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		userSelector = ItemFactory.newUserSelector("user", "deletedby", null, false, false);
		userSelector.setWrapTitle(false);
		userSelector.setWidth(150);
		userSelector.addChangedHandler(event -> refresh());
		toolStrip.addFormItem(userSelector);

		folderSelector = new FolderSelector("folder", null);
		folderSelector.setTitle(I18N.message("parent"));
		folderSelector.setWrapTitle(false);
		folderSelector.setWidth(250);
		folderSelector.addFolderChangeListener(this);
		toolStrip.addFormItem(folderSelector);

		ToolStripButton print = new ToolStripButton();
		print.setIcon(ItemFactory.newImgIcon("printer.png").getSrc());
		print.setTooltip(I18N.message("print"));
		print.setAutoFit(true);
		print.addClickHandler(event -> GridUtil.print(list));
		toolStrip.addSeparator();
		toolStrip.addButton(print);

		if (Feature.visible(Feature.EXPORT_CSV)) {
			toolStrip.addSeparator();
			ToolStripButton export = new ToolStripButton();
			export.setIcon(ItemFactory.newImgIcon("table_row_insert.png").getSrc());
			export.setTooltip(I18N.message("export"));
			export.setAutoFit(true);
			toolStrip.addButton(export);
			export.addClickHandler(event -> GridUtil.exportCSV(list, false));
			if (!Feature.enabled(Feature.EXPORT_CSV)) {
				export.setDisabled(true);
				export.setTooltip(I18N.message("featuredisabled"));
			}
		}

		toolStrip.addFill();

		// Prepare a panel containing a title and the documents list
		final InfoPanel infoPanel = new InfoPanel("");

		ListGridField id = new ColoredListGridField("id");
		id.setHidden(true);
		id.setCanGroupBy(false);

		ListGridField name = new FolderListGridField("name", I18N.message("name"));
		name.setWidth(200);
		name.setCanFilter(true);

		ListGridField lastModified = new DateListGridField("lastModified", "lastmodified");
		lastModified.setCanGroupBy(false);

		ListGridField deleteUser = new UserListGridField("deleteUser", "deleteUserId", "deletedby");
		deleteUser.setCanFilter(true);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(false);
		list.setAutoFetchData(true);
		list.setFilterOnKeypress(true);
		list.setShowFilterEditor(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);

		list.setFields(id, name, lastModified, deleteUser);

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		list.addDataArrivedHandler(
				event -> infoPanel.setMessage(I18N.message("showndocuments", Integer.toString(list.getTotalRows()))));

		body.setMembers(toolStrip, infoPanel, list);

		refresh();
	}

	private void refresh() {
		Long folderId = folderSelector.getFolderId();

		Long userId = null;

		if (userSelector.getValueAsString() != null && !"".equals(userSelector.getValueAsString()))
			userId = Long.parseLong(userSelector.getValueAsString());
		list.refresh(new DeletedFoldersDS(userId, folderId));
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();
		final ListGridRecord[] selection = list.getSelectedRecords();

		MenuItem restore = new MenuItem();
		restore.setTitle(I18N.message("restore"));
		restore.addClickHandler(event -> {
			if (selection == null || selection.length == 0)
				return;
			final Long[] ids = new Long[selection.length];
			for (int i = 0; i < selection.length; i++)
				ids[i] = Long.parseLong(selection[i].getAttribute("id"));

			final RestoreDialog dialog = new RestoreDialog(null, ids, evn -> refresh());
			dialog.show();
		});

		contextMenu.setItems(restore);
		contextMenu.showContextMenu();
	}

	@Override
	public void onChanged(GUIFolder folder) {
		refresh();
	}
}