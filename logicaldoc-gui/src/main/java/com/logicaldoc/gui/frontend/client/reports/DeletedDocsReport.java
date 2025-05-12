package com.logicaldoc.gui.frontend.client.reports;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.DeletedDocsDS;
import com.logicaldoc.gui.common.client.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.grid.FileSizeListGridField;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.UserListGridField;
import com.logicaldoc.gui.common.client.grid.VersionListGridField;
import com.logicaldoc.gui.common.client.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.FolderChangeListener;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.logicaldoc.gui.frontend.client.folder.RestoreDialog;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows a list of deleted documents
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.2.1
 */
public class DeletedDocsReport extends ReportPanel implements FolderChangeListener {

	private SelectItem userSelector;

	private FolderSelector folderSelector;

	private SpinnerItem max;

	public DeletedDocsReport() {
		super("deleteddocs", "showndocuments");
	}

	@Override
	protected void prepareListGrid() {
		ListGridField id = new IdListGridField();

		ListGridField size = new FileSizeListGridField("size", I18N.message("size"), 70);
		size.setCanFilter(false);
		size.setCanGroupBy(false);

		ListGridField version = new VersionListGridField();
		version.setCanFilter(false);
		version.setCanGroupBy(false);

		ListGridField fileVersion = new VersionListGridField("fileVersion", "fileversion");
		fileVersion.setCanFilter(false);
		fileVersion.setCanGroupBy(false);
		fileVersion.setHidden(true);

		ListGridField lastModified = new DateListGridField("lastModified", "lastmodified",
				DateCellFormatter.FORMAT_LONG);
		lastModified.setCanGroupBy(false);

		ListGridField customId = new ColoredListGridField("customId", I18N.message("customid"), 110);
		customId.setType(ListGridFieldType.TEXT);
		customId.setHidden(true);
		customId.setCanGroupBy(false);

		FileNameListGridField filename = new FileNameListGridField();
		filename.setWidth(200);
		filename.setCanFilter(true);

		ListGridField deleteUser = new UserListGridField("deleteUser", "avatar", "deletedby");
		deleteUser.setCanFilter(true);

		ListGridField type = new ColoredListGridField("type", I18N.message("type"), 55);
		type.setType(ListGridFieldType.TEXT);
		type.setAlign(Alignment.CENTER);
		type.setHidden(true);
		type.setCanGroupBy(false);

		list.setFields(id, filename, version, fileVersion, size, lastModified, customId, deleteUser, type);
	}

	@Override
	protected void fillToolBar(ToolStrip toolStrip) {
		max = ItemFactory.newSpinnerItem("max", "", 100, 5, null);
		max.setHint(I18N.message("elements"));
		max.setStep(10);
		max.setShowTitle(false);
		max.addChangedHandler(event -> refresh());

		ToolStripButton display = new ToolStripButton();
		display.setTitle(I18N.message("display"));
		display.addClickHandler(event -> {
			if (Boolean.TRUE.equals(max.validate()))
				refresh();
		});
		toolStrip.addButton(display);
		toolStrip.addFormItem(max);
		toolStrip.addSeparator();

		userSelector = ItemFactory.newUserSelector("user", "deletedby", null, false, false);
		userSelector.setWrapTitle(false);
		userSelector.setWidth(150);
		userSelector.addChangedHandler(event -> refresh());
		toolStrip.addFormItem(userSelector);

		folderSelector = new FolderSelector("folder", null);
		folderSelector.setWrapTitle(false);
		folderSelector.setWidth(250);
		folderSelector.addFolderChangeListener(this);
		toolStrip.addFormItem(folderSelector);
	}

	@Override
	protected void refresh() {
		Long folderId = folderSelector.getFolderId();
		Long userId = null;
		if (userSelector.getValueAsString() != null && !"".equals(userSelector.getValueAsString()))
			userId = Long.parseLong(userSelector.getValueAsString());
		list.refresh(new DeletedDocsDS(userId, folderId, max.getValueAsInteger()));
	}

	@Override
	protected void showContextMenu() {
		Menu contextMenu = new Menu();
		final ListGridRecord[] selection = list.getSelectedRecords();

		MenuItem restore = new MenuItem();
		restore.setTitle(I18N.message("restore"));
		restore.addClickHandler(click -> {
			if (selection == null || selection.length == 0)
				return;
			new RestoreDialog(GridUtil.getIds(selection), null, evt -> refresh()).show();
		});

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("permanentlydelete"));
		delete.addClickHandler(
				click -> LD.ask(I18N.message("permanentlydelete"), I18N.message("permanentlydeletehint"), choice -> {
					if (Boolean.TRUE.equals(choice)) {
						LD.contactingServer();
						DocumentService.Instance.get().destroyDocuments(GridUtil.getIds(selection),
								new DefaultAsyncCallback<Void>() {
									@Override
									public void onFailure(Throwable caught) {
										super.onFailure(caught);
										refresh();
									}

									@Override
									public void onSuccess(Void arg0) {
										LD.clearPrompt();
										refresh();
									}
								});
					}
				}));
		delete.setEnabled(
				com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.DESTROY_DOCUMENTS));

		contextMenu.setItems(restore, new MenuItemSeparator(), delete);
		contextMenu.showContextMenu();
	}

	@Override
	public void onChanged(GUIFolder folder) {
		refresh();
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