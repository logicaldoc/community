package com.logicaldoc.gui.frontend.client.document;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIVersion;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.data.VersionsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.common.client.widgets.preview.PreviewPopup;
import com.logicaldoc.gui.frontend.client.document.note.VersionNotesWindow;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows a list of versions of a document in a tabular way.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class VersionsPanel extends DocumentDetailTab {

	private static final String DOWNLOAD = "download";

	private static final String VERSION = "version";

	private static final String PERMALINK = "permalink";

	private static final String FILE_VERSION = "fileVersion";

	private ListGrid list = null;

	public VersionsPanel(final GUIDocument document) {
		super(document, null);
	}

	@Override
	protected void onDraw() {
		ListGridField id = new ListGridField("id");
		id.setHidden(true);

		ListGridField user = new UserListGridField("user", "userId", "user");
		ListGridField event = new ColoredListGridField("event", 200);
		ListGridField version = new ColoredListGridField(VERSION, 70);
		ListGridField fileVersion = new ColoredListGridField(FILE_VERSION, I18N.message("fileversion"), 70);
		ListGridField date = new DateListGridField("date", "date");
		ListGridField comment = new ColoredListGridField("comment", I18N.message("comment"));
		FileNameListGridField fileName = new FileNameListGridField();

		ListGridField type = new ColoredListGridField("type", 55);
		type.setType(ListGridFieldType.TEXT);
		type.setAlign(Alignment.CENTER);
		type.setHidden(true);

		ListGridField permalink = preparePermalink();

		ListGridField wfStatus = prepareWorkflow();

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setDataSource(new VersionsDS(document.getId(), null, 100));
		if (document.getFolder().isDownload())
			list.setFields(user, event, fileName, type, fileVersion, version, date, permalink, wfStatus, comment);
		else
			list.setFields(user, event, fileName, type, fileVersion, version, date, comment);

		addListHandlers();

		VLayout container = new VLayout();
		container.setMembersMargin(3);
		container.addMember(list);

		ToolStrip buttons = prepareButtons();

		container.addMember(buttons);
		addMember(container);
	}

	private ToolStrip prepareButtons() {
		ToolStrip buttons = new ToolStrip();
		buttons.setWidth100();

		ToolStripButton export = new ToolStripButton(I18N.message("export"));
		export.addClickHandler(clkEvent -> GridUtil.exportCSV(list, true));

		ToolStripButton print = new ToolStripButton(I18N.message("print"));
		print.addClickHandler(printEvent -> GridUtil.print(list));

		buttons.addButton(export);
		buttons.addButton(print);
		return buttons;
	}

	private void addListHandlers() {
		list.addCellDoubleClickHandler(clickEvent -> {
			ListGridRecord rec = clickEvent.getRecord();
			if (FolderController.get().getCurrentFolder().isDownload()
					&& DOWNLOAD.equals(Session.get().getInfo().getConfig("gui.doubleclick")))
				onDownload(document, rec);
			else
				onPreview(document, rec);
		});

		list.addCellContextClickHandler(contextClickEvent -> {
			ListGridField field = list.getField(contextClickEvent.getColNum());
			if (!PERMALINK.equals(field.getName())) {
				prepareContextMenu().showContextMenu();
				contextClickEvent.cancel();
			}
		});
	}

	private ListGridField prepareWorkflow() {
		ListGridField wfStatus = new ListGridField("workflowStatus", I18N.message("workflowstatus"), 150);
		wfStatus.setHidden(false);
		wfStatus.setCanFilter(true);
		wfStatus.setCanSort(true);
		wfStatus.setAlign(Alignment.LEFT);
		wfStatus.setCellFormatter((Object value, ListGridRecord rec, int rowNum, int colNum) -> {
			String display = rec.getAttributeAsString("workflowStatusDisplay");
			if (display != null && !display.isEmpty())
				return "<span style='color: " + display + ";'>" + value + "</span>";
			else
				return value != null ? value.toString() : "";
		});
		return wfStatus;
	}

	private ListGridField preparePermalink() {
		ListGridField permalink = new ColoredListGridField(PERMALINK, I18N.message(PERMALINK), 90);
		permalink.setAlign(Alignment.CENTER);
		permalink.setCellFormatter((Object value, ListGridRecord rec, int rowNum, int colNum) -> {
			long docId = document.getDocRef() != null ? document.getDocRef() : document.getId();
			String fileVer = rec.getAttributeAsString(FILE_VERSION);
			String downloadUrl = Util.downloadURL(docId, fileVer);
			return "<a href='" + downloadUrl + "' target='_blank'>" + I18N.message(DOWNLOAD) + "</a>";
		});
		return permalink;
	}

	protected void onDownload(final GUIDocument document, ListGridRecord rec) {
		if (document.getFolder().isDownload())
			DocUtil.download(document.getDocRef() != null ? document.getDocRef() : document.getId(),
					rec.getAttribute(FILE_VERSION));
	}

	protected void onPreview(final GUIDocument document, ListGridRecord rec) {
		GUIVersion version = new GUIVersion();
		version.setId(document.getId());
		version.setFolder(document.getFolder());
		version.setDocId(document.getId());
		version.setId(document.getId());
		version.setVersion(rec.getAttribute(VERSION));
		version.setFileVersion(rec.getAttribute(FILE_VERSION));
		version.setType(rec.getAttribute("type"));
		version.setFileName(rec.getAttribute("filename"));
		version.setFileSize(document.getFileSize());
		PreviewPopup iv = new PreviewPopup(version);
		iv.show();
	}

	/**
	 * Prepares the context menu.
	 */
	private Menu prepareContextMenu() {
		final ListGridRecord[] selection = list.getSelectedRecords();

		Menu contextMenu = new Menu();

		MenuItem compareMetadata = prepareCompareMetadataItem(selection);

		MenuItem compareContent = compareContentMenuItem(selection);

		MenuItem download = new MenuItem();
		download.setTitle(I18N.message(DOWNLOAD));
		download.addClickHandler((MenuItemClickEvent downloadEvent) -> onDownload(document, selection[0]));
		download.setEnabled(document.getFolder().isDownload());

		MenuItem preview = new MenuItem();
		preview.setTitle(I18N.message("preview"));
		preview.addClickHandler((MenuItemClickEvent previewEvent) -> onPreview(document, selection[0]));
		preview.setEnabled(
				com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.PREVIEW));

		MenuItem promote = comparePromoteMenuItem(selection);

		MenuItem delete = prepareDeleteMenuItem(selection);

		MenuItem replaceFile = new MenuItem();
		replaceFile.setTitle(I18N.message("replacefile"));
		replaceFile.addClickHandler((MenuItemClickEvent replaceFileEvent) -> new ReplaceVersionFile(document,
				selection[0].getAttributeAsString(FILE_VERSION)).show());

		MenuItem notes = new MenuItem();
		notes.setTitle(I18N.message("notes"));
		notes.addClickHandler((MenuItemClickEvent notesEvent) -> new VersionNotesWindow(document,
				selection[0].getAttributeAsString(FILE_VERSION)).show());

		compareMetadata.setEnabled(selection != null && selection.length == 2);
		compareContent.setEnabled(Feature.enabled(Feature.COMPARISON) && (selection != null && selection.length == 2));
		delete.setEnabled(deleteEnabled && selection != null && selection.length > 0);
		replaceFile.setEnabled(updateEnabled && selection != null && selection.length == 1);
		promote.setEnabled(updateEnabled && selection != null && selection.length == 1);

		if (selection == null || selection.length < 1) {
			preview.setEnabled(false);
			download.setEnabled(false);
			delete.setEnabled(false);
			replaceFile.setEnabled(false);
			promote.setEnabled(false);
			compareMetadata.setEnabled(false);
			compareContent.setEnabled(false);
			notes.setEnabled(false);
		}

		if (Feature.visible(Feature.COMPARISON))
			contextMenu.setItems(preview, download, notes, compareMetadata, compareContent, delete, promote,
					replaceFile);
		else
			contextMenu.setItems(preview, download, notes, compareMetadata, delete, promote, replaceFile);

		return contextMenu;
	}

	private MenuItem prepareDeleteMenuItem(final ListGridRecord[] selection) {
		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler((MenuItemClickEvent event) -> LD.ask(I18N.message("question"),
				I18N.message("delversionwarn") + ".\n " + I18N.message("confirmdelete"), (Boolean value) -> {
					if (Boolean.TRUE.equals(value)) {
						DocumentService.Instance.get().deleteVersions(GridUtil.getIds(selection),
								new AsyncCallback<GUIDocument>() {
									@Override
									public void onFailure(Throwable caught) {
										GuiLog.serverError(caught);
									}

									@Override
									public void onSuccess(GUIDocument result) {
										if (result != null) {
											document.setVersion(result.getVersion());
											document.setFileVersion(result.getFileVersion());
											DocumentController.get().modified(result);
											DocumentController.get().selected(result);
											list.removeSelectedData();
										}
									}
								});
					}
				}));
		return delete;
	}

	private MenuItem comparePromoteMenuItem(final ListGridRecord[] selection) {
		MenuItem promote = new MenuItem();
		promote.setTitle(I18N.message("promote"));
		promote.addClickHandler((MenuItemClickEvent promoteEvent) -> LD.ask(I18N.message("question"),
				I18N.message("promotequestion"), (Boolean yes) -> {
					if (Boolean.TRUE.equals(yes)) {
						LD.contactingServer();
						DocumentService.Instance.get().promoteVersion(document.getId(),
								selection[0].getAttributeAsString(VERSION), new AsyncCallback<GUIDocument>() {

									@Override
									public void onFailure(Throwable caught) {
										LD.clearPrompt();
										GuiLog.serverError(caught);
									}

									@Override
									public void onSuccess(GUIDocument document) {
										LD.clearPrompt();
										DocumentController.get().checkedIn(document);
										destroy();
									}
								});
					}
				}));
		return promote;
	}

	private MenuItem compareContentMenuItem(final ListGridRecord[] selection) {
		MenuItem compareContent = new MenuItem();
		compareContent.setTitle(I18N.message("comparecontent"));
		compareContent.addClickHandler((MenuItemClickEvent compareContentEvent) -> DocumentService.Instance.get()
				.getVersionsById(Long.parseLong(selection[0].getAttribute("id")),
						Long.parseLong(selection[1].getAttribute("id")), new AsyncCallback<GUIVersion[]>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIVersion[] versions) {
								ComparisonWindow diffWinfow = new ComparisonWindow(versions[0], versions[1]);
								diffWinfow.show();
							}
						}));
		return compareContent;
	}

	private MenuItem prepareCompareMetadataItem(final ListGridRecord[] selection) {
		MenuItem compareMetadata = new MenuItem();
		compareMetadata.setTitle(I18N.message("comparemetadata"));
		compareMetadata.addClickHandler((MenuItemClickEvent compareMetadataEvent) -> DocumentService.Instance.get()
				.getVersionsById(Long.parseLong(selection[0].getAttribute("id")),
						Long.parseLong(selection[1].getAttribute("id")), new AsyncCallback<GUIVersion[]>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIVersion[] result) {
								MetadataDiff diffWinfow = new MetadataDiff(result[0], result[1]);
								diffWinfow.show();
							}
						}));
		return compareMetadata;
	}
}