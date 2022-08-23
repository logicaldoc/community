package com.logicaldoc.gui.frontend.client.document;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIVersion;
import com.logicaldoc.gui.common.client.data.VersionsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.observer.DocumentController;
import com.logicaldoc.gui.common.client.observer.FolderController;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.common.client.widgets.preview.PreviewPopup;
import com.logicaldoc.gui.frontend.client.document.note.AnnotationsWindow;
import com.logicaldoc.gui.frontend.client.document.note.NoteUpdateDialog;
import com.logicaldoc.gui.frontend.client.document.note.NotesPanel;
import com.logicaldoc.gui.frontend.client.document.note.VersionNotesWindow;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickEvent;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
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

	private ListGrid list = null;

	public VersionsPanel(final GUIDocument document) {
		super(document, null);
	}

	@Override
	protected void onDraw() {
		ListGridField id = new ListGridField("id");
		id.setHidden(true);

		ListGridField user = new UserListGridField("user", "userId", "user");
		ListGridField event = new ColoredListGridField("event", I18N.message("event"), 200);
		ListGridField version = new ColoredListGridField("version", I18N.message("version"), 70);
		ListGridField fileVersion = new ColoredListGridField("fileVersion", I18N.message("fileversion"), 70);
		ListGridField date = new DateListGridField("date", "date");
		ListGridField comment = new ColoredListGridField("comment", I18N.message("comment"));
		FileNameListGridField fileName = new FileNameListGridField();

		ListGridField type = new ColoredListGridField("type", I18N.message("type"), 55);
		type.setType(ListGridFieldType.TEXT);
		type.setAlign(Alignment.CENTER);
		type.setHidden(true);

		ListGridField permalink = new ColoredListGridField("permalink", I18N.message("permalink"), 90);
		permalink.setAlign(Alignment.CENTER);
		permalink.setCellFormatter(new CellFormatter() {

			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				long docId = document.getDocRef() != null ? document.getDocRef() : document.getId();
				String fileVersion = record.getAttributeAsString("fileVersion");
				String downloadUrl = Util.downloadURL(docId, fileVersion);
				String perma = "<a href='" + downloadUrl + "' target='_blank'>" + I18N.message("download") + "</a>";
				return perma;
			}
		});

		ListGridField wfStatus = new ListGridField("workflowStatus", I18N.message("workflowstatus"), 150);
		wfStatus.setHidden(false);
		wfStatus.setCanFilter(true);
		wfStatus.setCanSort(true);
		wfStatus.setAlign(Alignment.LEFT);
		wfStatus.setCellFormatter(new CellFormatter() {

			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				String display = record.getAttributeAsString("workflowStatusDisplay");
				if (display != null && !display.isEmpty())
					return "<span style='color: " + display + ";'>" + value + "</span>";
				else
					return value != null ? value.toString() : "";
			}
		});

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setDataSource(new VersionsDS(document.getId(), null, 100));
		if (document.getFolder().isDownload())
			list.setFields(user, event, fileName, type, fileVersion, version, date, permalink, wfStatus, comment);
		else
			list.setFields(user, event, fileName, type, fileVersion, version, date, comment);
		list.addCellDoubleClickHandler(new CellDoubleClickHandler() {
			@Override
			public void onCellDoubleClick(CellDoubleClickEvent event) {
				ListGridRecord record = event.getRecord();
				if (FolderController.get().getCurrentFolder().isDownload()
						&& "download".equals(Session.get().getInfo().getConfig("gui.doubleclick")))
					onDownload(document, record);
				else
					onPreview(document, record);
			}
		});

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				ListGridField field = list.getField(event.getColNum());
				if (!"permalink".equals(field.getName())) {
					prepareContextMenu().showContextMenu();
					event.cancel();
				}
			}
		});

		VLayout container = new VLayout();
		container.setMembersMargin(3);
		container.addMember(list);

		ToolStrip buttons = new ToolStrip();
		buttons.setWidth100();

		ToolStripButton export = new ToolStripButton(I18N.message("export"));
		export.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				GridUtil.exportCSV(list, true);
			}
		});
		
		ToolStripButton print = new ToolStripButton(I18N.message("print"));
		print.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				GridUtil.print(list);
			}
		});

		buttons.addButton(export);
		buttons.addButton(print);

		container.addMember(buttons);
		addMember(container);
	}

	protected void onDownload(final GUIDocument document, ListGridRecord record) {
		if (document.getFolder().isDownload())
			DocUtil.download(document.getDocRef() != null ? document.getDocRef() : document.getId(),
					record.getAttribute("fileVersion"));
	}

	protected void onPreview(final GUIDocument document, ListGridRecord record) {
		GUIVersion version = new GUIVersion();
		version.setId(document.getId());
		version.setFolder(document.getFolder());
		version.setDocId(document.getId());
		version.setId(document.getId());
		version.setVersion(record.getAttribute("version"));
		version.setFileVersion(record.getAttribute("fileVersion"));
		version.setType(record.getAttribute("type"));
		version.setFileName(record.getAttribute("filename"));
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
		MenuItem compareMetadata = new MenuItem();
		compareMetadata.setTitle(I18N.message("comparemetadata"));
		compareMetadata.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				DocumentService.Instance.get().getVersionsById(Long.parseLong(selection[0].getAttribute("id")),
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
						});
			}
		});

		MenuItem compareContent = new MenuItem();
		compareContent.setTitle(I18N.message("comparecontent"));
		compareContent.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				DocumentService.Instance.get().getVersionsById(Long.parseLong(selection[0].getAttribute("id")),
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
						});
			}
		});

		MenuItem download = new MenuItem();
		download.setTitle(I18N.message("download"));
		download.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				onDownload(document, selection[0]);
			}
		});
		download.setEnabled(document.getFolder().isDownload());

		MenuItem preview = new MenuItem();
		preview.setTitle(I18N.message("preview"));
		preview.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				onPreview(document, selection[0]);
			}
		});
		preview.setEnabled(com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.PREVIEW));

		MenuItem promote = new MenuItem();
		promote.setTitle(I18N.message("promote"));
		promote.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("promotequestion"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							LD.contactingServer();
							DocumentService.Instance.get().promoteVersion(document.getId(),
									selection[0].getAttributeAsString("version"), new AsyncCallback<GUIDocument>() {

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
					}
				});
			}
		});

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"),
						I18N.message("delversionwarn") + ".\n " + I18N.message("confirmdelete"), new BooleanCallback() {
							@Override
							public void execute(Boolean value) {
								if (value) {
									long[] ids = new long[selection.length];
									int i = 0;
									for (ListGridRecord record : selection)
										ids[i++] = Long.parseLong(record.getAttribute("id"));

									DocumentService.Instance.get().deleteVersions(ids,
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
							}
						});
			}
		});

		MenuItem replaceFile = new MenuItem();
		replaceFile.setTitle(I18N.message("replacefile"));
		replaceFile.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				ReplaceVersionFile dialog = new ReplaceVersionFile(document,
						selection[0].getAttributeAsString("fileVersion"));
				dialog.show();
			}
		});

		MenuItem notes = new MenuItem();
		notes.setTitle(I18N.message("notes"));
		notes.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				VersionNotesWindow versionNotes = new VersionNotesWindow(document,
						selection[0].getAttributeAsString("fileVersion"));
				versionNotes.show();
			}
		});

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
}