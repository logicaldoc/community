package com.logicaldoc.gui.frontend.client.reports;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.ArchivedDocsDS;
import com.logicaldoc.gui.common.client.formatters.DateCellFormatter;
import com.logicaldoc.gui.common.client.formatters.FileSizeCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.common.client.widgets.FolderChangeListener;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.common.client.widgets.PreviewPopup;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.document.SendToArchiveDialog;
import com.logicaldoc.gui.frontend.client.folder.FolderSelector;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.events.DoubleClickHandler;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows a list of archived documents
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 7.2
 */
public class ArchivedDocsPanel extends AdminPanel implements FolderChangeListener {
	private ListGrid list;

	private InfoPanel infoPanel;

	private FolderSelector folderSelector;

	private IntegerItem max;

	public ArchivedDocsPanel() {
		super("archiveddocs");

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		max = ItemFactory.newValidateIntegerItem("max", "", 100, 1, null);
		max.setHint(I18N.message("elements"));
		max.setShowTitle(false);
		max.setWidth(50);

		ToolStripButton display = new ToolStripButton();
		display.setTitle(I18N.message("display"));
		display.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (max.validate())
					refresh();
			}
		});
		toolStrip.addButton(display);
		toolStrip.addFormItem(max);
		toolStrip.addSeparator();

		folderSelector = new FolderSelector("folder", true);
		folderSelector.setWrapTitle(false);
		folderSelector.setWidth(250);
		folderSelector.addFolderChangeListener(this);
		toolStrip.addFormItem(folderSelector);

		ToolStripButton print = new ToolStripButton();
		print.setIcon(ItemFactory.newImgIcon("printer.png").getSrc());
		print.setTooltip(I18N.message("print"));
		print.setAutoFit(true);
		print.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				Canvas.printComponents(new Object[] { list });
			}
		});
		toolStrip.addSeparator();
		toolStrip.addButton(print);

		if (Feature.visible(Feature.EXPORT_CSV)) {
			toolStrip.addSeparator();
			ToolStripButton export = new ToolStripButton();
			export.setIcon(ItemFactory.newImgIcon("table_row_insert.png").getSrc());
			export.setTooltip(I18N.message("export"));
			export.setAutoFit(true);
			toolStrip.addButton(export);
			export.addClickHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					Util.exportCSV(list, false);
				}
			});
			if (!Feature.enabled(Feature.EXPORT_CSV)) {
				export.setDisabled(true);
				export.setTooltip(I18N.message("featuredisabled"));
			}
		}

		toolStrip.addFill();

		// Prepare a panel containing a title and the documents list
		infoPanel = new InfoPanel("");

		body.setMembers(toolStrip, infoPanel);

		refresh();
	}

	private void refresh() {
		if (list != null) {
			list.destroy();
			body.removeMember(list);
		}

		Long folderId = folderSelector.getFolderId();
		int maxElements = max.getValueAsInteger();

		ListGridField id = new ListGridField("id", I18N.message("id"));
		id.setHidden(true);
		id.setCanGroupBy(false);

		ListGridField size = new ListGridField("size", I18N.message("size"), 70);
		size.setAlign(Alignment.RIGHT);
		size.setType(ListGridFieldType.FLOAT);
		size.setCellFormatter(new FileSizeCellFormatter());
		size.setCanFilter(false);
		size.setCanGroupBy(false);

		ListGridField icon = new ListGridField("icon", " ", 24);
		icon.setType(ListGridFieldType.IMAGE);
		icon.setCanSort(false);
		icon.setAlign(Alignment.CENTER);
		icon.setShowDefaultContextMenu(false);
		icon.setImageURLPrefix(Util.imagePrefix());
		icon.setImageURLSuffix(".png");
		icon.setCanFilter(false);
		icon.setCanGroupBy(false);

		ListGridField version = new ListGridField("version", I18N.message("version"), 55);
		version.setAlign(Alignment.CENTER);
		version.setCanFilter(true);
		version.setCanGroupBy(false);

		ListGridField fileVersion = new ListGridField("fileVersion", I18N.message("fileversion"), 55);
		fileVersion.setAlign(Alignment.CENTER);
		fileVersion.setCanFilter(true);
		fileVersion.setCanGroupBy(false);
		fileVersion.setHidden(true);

		ListGridField lastModified = new ListGridField("lastModified", I18N.message("lastmodified"), 110);
		lastModified.setAlign(Alignment.CENTER);
		lastModified.setType(ListGridFieldType.DATE);
		lastModified.setCellFormatter(new DateCellFormatter(false));
		lastModified.setCanFilter(false);
		lastModified.setCanGroupBy(false);
		lastModified.setHidden(true);

		ListGridField created = new ListGridField("created", I18N.message("createdon"), 110);
		created.setAlign(Alignment.CENTER);
		created.setType(ListGridFieldType.DATE);
		created.setCellFormatter(new DateCellFormatter(false));
		created.setCanFilter(false);

		ListGridField folder = new ListGridField("folder", I18N.message("folder"), 200);
		folder.setAlign(Alignment.CENTER);
		folder.setCanFilter(true);
		folder.setCanGroupBy(true);

		ListGridField customId = new ListGridField("customId", I18N.message("customid"), 110);
		customId.setType(ListGridFieldType.TEXT);
		customId.setHidden(true);
		customId.setCanGroupBy(false);

		ListGridField filename = new ListGridField("filename", I18N.message("filename"), 200);
		filename.setCanFilter(true);

		ListGridField type = new ListGridField("type", I18N.message("type"), 55);
		type.setType(ListGridFieldType.TEXT);
		type.setAlign(Alignment.CENTER);
		type.setHidden(true);
		type.setCanGroupBy(false);

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(false);
		list.setAutoFetchData(true);
		list.setFilterOnKeypress(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setDataSource(new ArchivedDocsDS(folderId, maxElements));

		list.setFields(icon, filename, version, fileVersion, size, created, lastModified, folder, id, customId, type);

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});

		list.addDoubleClickHandler(new DoubleClickHandler() {
			@Override
			public void onDoubleClick(DoubleClickEvent event) {
				Long id = list.getSelectedRecord().getAttributeAsLong("id");
				WindowUtils.openUrl(Util.downloadURL(id));
			}
		});

		list.addDataArrivedHandler(new DataArrivedHandler() {
			@Override
			public void onDataArrived(DataArrivedEvent event) {
				infoPanel.setMessage(I18N.message("showndocuments", Integer.toString(list.getTotalRows())));
			}
		});

		body.addMember(list);
	}

	private void showContextMenu() {
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
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(GUIDocument doc) {
						PreviewPopup iv = new PreviewPopup(doc);
						iv.show();
					}
				});
			}
		});

		MenuItem download = new MenuItem();
		download.setTitle(I18N.message("download"));
		download.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				String id = list.getSelectedRecord().getAttribute("id");
				WindowUtils.openUrl(GWT.getHostPageBaseURL() + "download?docId=" + id);
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
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void arg0) {
						list.removeSelectedData();
						Log.info(I18N.message("docsrestored"), null);
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