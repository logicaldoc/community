package com.logicaldoc.gui.frontend.client.reports;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.ArchivedDocsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.FolderChangeListener;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.logicaldoc.gui.common.client.widgets.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileSizeListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.VersionListGridField;
import com.logicaldoc.gui.common.client.widgets.preview.PreviewPopup;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.document.SendToArchiveDialog;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows a list of archived documents
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.2
 */
public class ArchivedDocsReport extends ReportPanel implements FolderChangeListener {

	private FolderSelector folderSelector;

	private SpinnerItem max;

	public ArchivedDocsReport() {
		super("archiveddocs", "showndocuments");
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
			if (max.validate())
				refresh();
		});
		toolStrip.addButton(display);
		toolStrip.addFormItem(max);
		toolStrip.addSeparator();

		folderSelector = new FolderSelector("folder", true);
		folderSelector.setWrapTitle(false);
		folderSelector.setWidth(250);
		folderSelector.addFolderChangeListener(this);
		toolStrip.addFormItem(folderSelector);
	}

	@Override
	protected void prepareListGrid() {
		ListGridField id = new ColoredListGridField("id", I18N.message("id"));
		id.setHidden(true);
		id.setCanGroupBy(false);

		ListGridField size = new FileSizeListGridField("size", I18N.message("size"));
		size.setCanFilter(false);
		size.setCanGroupBy(false);

		ListGridField version = new VersionListGridField();
		version.setCanFilter(false);
		version.setCanGroupBy(false);

		ListGridField fileVersion = new VersionListGridField("fileVersion", "fileversion");
		fileVersion.setCanFilter(false);
		fileVersion.setCanGroupBy(false);
		fileVersion.setHidden(true);

		ListGridField lastModified = new DateListGridField("lastModified", "lastmodified");
		lastModified.setCanFilter(false);
		lastModified.setCanGroupBy(false);
		lastModified.setHidden(true);

		ListGridField created = new DateListGridField("created", "createdon");

		ListGridField folder = new ColoredListGridField("folder", I18N.message("folder"), 200);
		folder.setAlign(Alignment.CENTER);
		folder.setCanFilter(true);
		folder.setCanGroupBy(true);

		ListGridField customId = new ColoredListGridField("customId", I18N.message("customid"), 110);
		customId.setType(ListGridFieldType.TEXT);
		customId.setHidden(true);
		customId.setCanGroupBy(false);

		FileNameListGridField filename = new FileNameListGridField();
		filename.setWidth(200);
		filename.setCanFilter(true);

		ListGridField type = new ColoredListGridField("type", I18N.message("type"), 55);
		type.setType(ListGridFieldType.TEXT);
		type.setAlign(Alignment.CENTER);
		type.setHidden(true);
		type.setCanGroupBy(false);

		list.setFields(filename, version, fileVersion, size, created, lastModified, folder, id, customId, type);

		list.addDoubleClickHandler(event -> DocUtil.download(list.getSelectedRecord().getAttributeAsLong("id"), null));
	}

	@Override
	protected void refresh() {
		Long folderId = folderSelector.getFolderId();
		int maxElements = max.getValueAsInteger();
		list.refresh(new ArchivedDocsDS(folderId, maxElements));
	}

	@Override
	protected void showContextMenu() {
		Menu contextMenu = new Menu();
		final ListGridRecord[] selection = list.getSelectedRecords();

		MenuItem preview = new MenuItem();
		preview.setTitle(I18N.message("preview"));
		preview.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				long id = Long.parseLong(list.getSelectedRecord().getAttribute("id"));

				DocumentService.Instance.get().getById(id, new AsyncCallback<GUIDocument>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIDocument doc) {
						PreviewPopup iv = new PreviewPopup(doc);
						iv.show();
					}
				});
			}
		});
		preview.setEnabled(
				com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.PREVIEW));

		MenuItem download = new MenuItem();
		download.setTitle(I18N.message("download"));
		download.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				DocUtil.download(list.getSelectedRecord().getAttributeAsLong("id"), null);
			}
		});

		MenuItem openFolder = new MenuItem();
		openFolder.setTitle(I18N.message("openfolder"));
		openFolder.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				ListGridRecord record = list.getSelectedRecord();
				DocumentsPanel.get().openInFolder(Long.parseLong(record.getAttributeAsString("folderId")),
						Long.parseLong(record.getAttributeAsString("id")));
			}
		});

		MenuItem restore = new MenuItem();
		restore.setTitle(I18N.message("restore"));
		restore.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				long[] docIds = new long[selection.length];
				for (int i = 0; i < selection.length; i++)
					docIds[i] = Long.parseLong(selection[i].getAttributeAsString("id"));
				DocumentService.Instance.get().unarchiveDocuments(docIds, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void arg0) {
						list.removeSelectedData();
						GuiLog.info(I18N.message("docsrestored"), null);
					}
				});
			}
		});

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				final long[] docIds = new long[selection.length];
				for (int i = 0; i < selection.length; i++)
					docIds[i] = Long.parseLong(selection[i].getAttributeAsString("id"));

				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							DocumentService.Instance.get().delete(docIds, new AsyncCallback<Void>() {
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

		MenuItem sendToExpArchive = new MenuItem();
		sendToExpArchive.setTitle(I18N.message("sendtoexparchive"));
		sendToExpArchive.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				long[] selectionIds = new long[selection.length];
				for (int i = 0; i < selection.length; i++)
					selectionIds[i] = Long.parseLong(selection[i].getAttributeAsString("id"));
				SendToArchiveDialog archiveDialog = new SendToArchiveDialog(selectionIds, true);
				archiveDialog.show();
			}
		});

		download.setEnabled(list.getSelectedRecords() != null && list.getSelectedRecords().length == 1);
		preview.setEnabled(list.getSelectedRecords() != null && list.getSelectedRecords().length == 1);
		openFolder.setEnabled(list.getSelectedRecords() != null && list.getSelectedRecords().length == 1);
		sendToExpArchive.setEnabled(list.getSelectedRecords() != null && list.getSelectedRecords().length > 0);
		delete.setEnabled(list.getSelectedRecords() != null && list.getSelectedRecords().length > 0);
		restore.setEnabled(list.getSelectedRecords() != null && list.getSelectedRecords().length > 0);

		contextMenu.setItems(download, preview, openFolder, restore, sendToExpArchive, delete);
		contextMenu.showContextMenu();
	}

	@Override
	public void onChanged(GUIFolder folder) {
		refresh();
	}
}