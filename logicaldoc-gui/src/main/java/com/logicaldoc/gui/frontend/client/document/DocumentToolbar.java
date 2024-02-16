package com.logicaldoc.gui.frontend.client.document;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUICalendarEvent;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIReminder;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.controllers.FolderObserver;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.DropSpotPopup;
import com.logicaldoc.gui.frontend.client.calendar.CalendarEventDialog;
import com.logicaldoc.gui.frontend.client.document.form.AddDocumentUsingForm;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsGrid;
import com.logicaldoc.gui.frontend.client.document.reading.ReadingRequestDialog;
import com.logicaldoc.gui.frontend.client.document.signature.DigitalSignatureDialog;
import com.logicaldoc.gui.frontend.client.document.stamp.StampDialog;
import com.logicaldoc.gui.frontend.client.document.update.UpdateDialog;
import com.logicaldoc.gui.frontend.client.folder.FolderNavigator;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.subscription.SubscriptionDialog;
import com.smartgwt.client.types.SelectionType;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * The toolbar to handle some documents aspects
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentToolbar extends ToolStrip implements FolderObserver {

	protected ToolStripButton refresh = AwesomeFactory.newToolStripButton("sync-alt", "refresh");

	protected ToolStripButton download = AwesomeFactory.newToolStripButton("download", "download");

	protected ToolStripButton saveLayout = AwesomeFactory.newToolStripButton("save", "savelayoutinuserprofile");

	protected ToolStripButton pdf = AwesomeFactory.newToolStripButton("file-pdf", "exportpdf");

	protected ToolStripButton convert = AwesomeFactory.newToolStripButton("copy", "convert");

	protected ToolStripButton add = AwesomeFactory.newToolStripButton("upload", "adddocuments");

	protected ToolStripButton addForm = AwesomeFactory.newToolStripButton("file-alt", "addform");

	protected ToolStripButton subscribe = AwesomeFactory.newToolStripButton("envelope", "subscribe");

	protected ToolStripButton dropSpot = AwesomeFactory.newToolStripButton("eye-dropper", "dropspot");

	protected ToolStripButton scan = AwesomeFactory.newToolStripButton("scanner-image", "scandocument");

	protected ToolStripButton archive = AwesomeFactory.newToolStripButton("archive", "sendtoexparchive");

	protected ToolStripButton startWorkflow = AwesomeFactory.newToolStripButton("cogs", "startworkflow");

	protected ToolStripButton addCalendarEvent = AwesomeFactory.newToolStripButton("calendar-plus", "newcalendarevent");

	protected ToolStripButton list = AwesomeFactory.newToolStripButton("bars", "list");

	protected ToolStripButton gallery = AwesomeFactory.newToolStripButton("images", "gallery");

	protected ToolStripButton office = AwesomeFactory.newToolStripButton("windows", "editwithoffice");

	protected ToolStripButton bulkUpdate = AwesomeFactory.newToolStripButton("edit", "bulkupdate");

	protected ToolStripButton stamp = AwesomeFactory.newToolStripButton("tint", "stamp");

	protected ToolStripButton sign = AwesomeFactory.newToolStripButton("badge-check", "sign");

	protected ToolStripButton readingRequest = AwesomeFactory.newToolStripButton("glasses", "requestreading");

	protected ToolStripButton bulkCheckout = AwesomeFactory.newToolStripButton("check", "bulkcheckout");

	protected ToolStripButton filter = AwesomeFactory.newToolStripButton("filter", "filter");

	protected ToolStripButton print = AwesomeFactory.newToolStripButton("print", "print");

	protected ToolStripButton export = AwesomeFactory.newToolStripButton("angle-double-down", "export");

	protected ToolStripButton togglePreview = AwesomeFactory.newToolStripButton("toggle-on", "closepreview");

	protected GUIDocument document;

	private static DocumentToolbar instance = null;

	public static DocumentToolbar get() {
		if (instance == null)
			instance = new DocumentToolbar();
		return instance;
	}

	private DocumentToolbar() {
		setWidth100();
		setHeight(27);

		GUIFolder folder = FolderController.get().getCurrentFolder();
		prepareButtons();
		update(null, folder);

		FolderController.get().addObserver(this);
	}

	protected void prepareButtons() {

		addRefresh();

		addSeparator();

		addDownload();

		addPdf();

		addConvert();

		addOffice();

		addSeparator();

		addUpload();

		addDropSpot();

		addScan();

		addForm();

		addSubscribe();

		addArchive();

		if (Feature.visible(Feature.BULK_UPDATE) || Feature.visible(Feature.BULK_CHECKOUT))
			addSeparator();

		addBulkUpdate();

		addBulkCheckout();

		addStamp();

		addDigitalSignature();

		addReadingRequest();

		addStartWorkflow();

		addCalendar();

		addSeparator();

		addFilter();

		addPrint();

		addExport();

		addSaveLayout();

		addSeparator();

		addList();

		addGallery();

		addSeparator();

		addTogglePreview();

		int mode = DocumentsGrid.MODE_LIST;
		if (CookiesManager.get(CookiesManager.COOKIE_DOCSLIST_MODE) != null
				&& !CookiesManager.get(CookiesManager.COOKIE_DOCSLIST_MODE).equals(""))
			mode = Integer.parseInt(CookiesManager.get(CookiesManager.COOKIE_DOCSLIST_MODE));
		if (mode == DocumentsGrid.MODE_LIST)
			list.setSelected(true);
		else
			gallery.setSelected(true);

		addFill();
	}

	private void addTogglePreview() {
		try {
			// Retrieve the saved preview width
			String w = CookiesManager.get(CookiesManager.COOKIE_DOCSLIST_PREV_W);
			if (Integer.parseInt(w) <= 0) {
				togglePreview.setTitle(AwesomeFactory.getIconHtml("toggle-off"));
				togglePreview.setTooltip(I18N.message("openpreview"));
			}
		} catch (Exception t) {
			// Nothing to do
		}
		togglePreview.addClickHandler(event -> {
			if (DocumentsPanel.get().getPreviewPanel().isVisible()
					&& DocumentsPanel.get().getPreviewPanel().getWidth() > 1) {
				DocumentsPanel.get().getPreviewPanel().setWidth(0);
				togglePreview.setTitle(AwesomeFactory.getIconHtml("toggle-off"));
				togglePreview.setTooltip(I18N.message("openpreview"));
			} else {
				try {
					String w = CookiesManager.get(CookiesManager.COOKIE_DOCSLIST_PREV_W);
					DocumentsPanel.get().getPreviewPanel().setWidth(Integer.parseInt(w));
				} catch (Exception t) {
					DocumentsPanel.get().getPreviewPanel().setWidth(350);
				}
				DocumentsPanel.get().getPreviewPanel().setDocument(document);
				togglePreview.setTitle(AwesomeFactory.getIconHtml("toggle-on"));
				togglePreview.setTooltip(I18N.message("closepreview"));
			}
		});
		addButton(togglePreview);
	}

	private void addFilter() {
		filter.setActionType(SelectionType.CHECKBOX);
		addButton(filter);
		filter.addClickHandler(event -> DocumentsPanel.get().toggleFilters());
	}

	private void addGallery() {
		gallery.setActionType(SelectionType.RADIO);
		gallery.setRadioGroup("mode");
		gallery.addClickHandler(event -> {
			if (FolderController.get().getCurrentFolder() != null)
				CookiesManager.save(CookiesManager.COOKIE_DOCSLIST_MODE, DocumentsGrid.MODE_GALLERY);
			DocumentsPanel.get().refresh(DocumentsGrid.MODE_GALLERY);
		});
		gallery.setDisabled(FolderController.get().getCurrentFolder() == null
				|| !Session.get().getConfigAsBoolean("gui.galleryenabled"));
		gallery.setVisible(Session.get().getConfigAsBoolean("gui.galleryenabled"));
		addButton(gallery);
	}

	private void addList() {
		list.setActionType(SelectionType.RADIO);
		list.setRadioGroup("mode");
		list.addClickHandler(event -> {
			CookiesManager.save(CookiesManager.COOKIE_DOCSLIST_MODE, DocumentsGrid.MODE_LIST);
			DocumentsPanel.get().refresh(DocumentsGrid.MODE_LIST);
		});
		list.setDisabled(FolderController.get().getCurrentFolder() == null);
		addButton(list);
	}

	private void addSaveLayout() {
		saveLayout.addClickHandler(event -> saveGridState());
		addButton(saveLayout);
	}

	private void addExport() {
		if (Feature.visible(Feature.EXPORT_CSV)) {
			addButton(export);
			export.addClickHandler(event -> DocumentsPanel.get().export());
			if (!Feature.enabled(Feature.EXPORT_CSV)) {
				setFeatureDisabled(export);
			}
		}
	}

	private void addPrint() {
		addButton(print);
		print.addClickHandler(event -> DocumentsPanel.get().printPreview());
	}

	private void addCalendar() {
		if (Feature.visible(Feature.CALENDAR)) {
			addSeparator();
			addButton(addCalendarEvent);
			if (!Feature.enabled(Feature.CALENDAR))
				setFeatureDisabled(addCalendarEvent);

			addCalendarEvent.addClickHandler(
					event -> checkPermissionsAndRun(new String[] { GUIAccessControlEntry.PERMISSION_CALENDAR }, () -> {
						DocumentsGrid grid = DocumentsPanel.get().getDocumentsGrid();
						GUICalendarEvent calEvent = new GUICalendarEvent();
						calEvent.setCreator(Session.get().getUser().getFullName());
						calEvent.setCreatorId(Session.get().getUser().getId());
						GUIUser user = new GUIUser();
						user.setId(Session.get().getUser().getId());
						user.setUsername(Session.get().getUser().getUsername());
						user.setFirstName(Session.get().getUser().getFirstName());
						user.setName(Session.get().getUser().getName());
						calEvent.addParticipant(user);

						List<GUIDocument> docs = grid.getSelectedDocuments();
						if (!docs.isEmpty()) {
							calEvent.setDocuments(docs);
							calEvent.setTitle(Util.getBaseName(docs.get(0).getFileName()));
							calEvent.setType(docs.get(0).getTemplate());
						}

						calEvent.addReminder(new GUIReminder(0, GUIReminder.TIME_UNIT_MINUTE));
						CalendarEventDialog eventDialog = new CalendarEventDialog(calEvent, null);
						eventDialog.show();
					}));
		}
	}

	private void addStartWorkflow() {
		if (Feature.visible(Feature.WORKFLOW)) {
			addSeparator();
			addButton(startWorkflow);
			if (!Feature.enabled(Feature.WORKFLOW))
				setFeatureDisabled(startWorkflow);

			startWorkflow.addClickHandler(event -> checkPermissionsAndRun(
					new String[] { GUIAccessControlEntry.PERMISSION_WORKFLOW },
					() -> new StartWorkflowDialog(DocumentsPanel.get().getDocumentsGrid().getSelectedIds()).show()));
		}
	}

	private void addDigitalSignature() {
		if (Feature.visible(Feature.DIGITAL_SIGNATURE)) {
			addButton(sign);
			sign.setTooltip(I18N.message("sign"));
			if (!Feature.enabled(Feature.DIGITAL_SIGNATURE))
				setFeatureDisabled(sign);

			sign.addClickHandler(event -> checkPermissionsAndRun(new String[] { GUIAccessControlEntry.PERMISSION_SIGN },
					() -> new DigitalSignatureDialog(DocumentsPanel.get().getDocumentsGrid().getSelectedIds()).show()));
		}
	}

	private void addReadingRequest() {
		if (Feature.visible(Feature.READING_CONFIRMATION)) {
			addButton(readingRequest);
			readingRequest.setTooltip(I18N.message("requestreading"));
			if (!Feature.enabled(Feature.READING_CONFIRMATION))
				setFeatureDisabled(readingRequest);

			readingRequest.addClickHandler(event -> checkPermissionsAndRun(
					new String[] { GUIAccessControlEntry.PERMISSION_READINGREQ },
					() -> new ReadingRequestDialog(DocumentsPanel.get().getDocumentsGrid().getSelectedIds()).show()));
		}
	}

	private void addStamp() {
		if (Feature.visible(Feature.STAMP)) {
			addSeparator();
			addButton(stamp);
			stamp.setTooltip(I18N.message("stamp"));
			if (!Feature.enabled(Feature.STAMP))
				setFeatureDisabled(stamp);

			stamp.addClickHandler(event -> checkPermissionsAndRun(
					new String[] { GUIAccessControlEntry.PERMISSION_WRITE },
					() -> new StampDialog(DocumentsPanel.get().getDocumentsGrid().getSelectedDocuments()).show()));
		}
	}

	private void addBulkCheckout() {
		if (Feature.visible(Feature.BULK_CHECKOUT)) {
			addButton(bulkCheckout);
			if (!Feature.enabled(Feature.BULK_CHECKOUT))
				setFeatureDisabled(bulkCheckout);

			bulkCheckout.addClickHandler(event -> checkPermissionsAndRun(
					new String[] { GUIAccessControlEntry.PERMISSION_DOWNLOAD, GUIAccessControlEntry.PERMISSION_WRITE },
					() -> {
						List<GUIDocument> docs = DocumentsPanel.get().getDocumentsGrid().getSelectedDocuments();
						List<Long> unlockedIds = new ArrayList<>();
						for (GUIDocument doc : docs)
							if (doc.getStatus() == 0 && doc.getImmutable() == 0)
								unlockedIds.add(doc.getId());
						Util.openBulkCheckout(unlockedIds);
					}));
		}
	}

	private void addBulkUpdate() {
		if (Feature.visible(Feature.BULK_UPDATE)) {
			addButton(bulkUpdate);
			if (!Feature.enabled(Feature.BULK_UPDATE))
				setFeatureDisabled(bulkUpdate);

			bulkUpdate.addClickHandler(event -> checkPermissionsAndRun(
					new String[] { GUIAccessControlEntry.PERMISSION_WRITE },
					() -> new UpdateDialog(DocumentsPanel.get().getDocumentsGrid().getSelectedIds(),
							FolderController.get().getCurrentFolder().newDocument(), UpdateDialog.BULKUPDATE, false)
									.show()));
		}
	}

	private void addArchive() {
		if (Feature.visible(Feature.IMPEX)) {
			addSeparator();
			addButton(archive);
			if (!Feature.enabled(Feature.IMPEX))
				setFeatureDisabled(archive);

			archive.addClickHandler(event -> checkPermissionsAndRun(
					new String[] { GUIAccessControlEntry.PERMISSION_ARCHIVE },
					() -> new SendToArchiveDialog(DocumentsPanel.get().getDocumentsGrid().getSelectedIds(), true)
							.show()));
		}
	}

	private void addSubscribe() {
		if (Feature.visible(Feature.AUDIT)) {
			addSeparator();
			addButton(subscribe);
			if (!Feature.enabled(Feature.AUDIT))
				setFeatureDisabled(subscribe);

			subscribe.addClickHandler(
					event -> checkPermissionsAndRun(new String[] { GUIAccessControlEntry.PERMISSION_SUBSCRIPTION },
							() -> new SubscriptionDialog(null, DocumentsPanel.get().getDocumentsGrid().getSelectedIds())
									.show()));
		}
	}

	private void addForm() {
		if (Feature.visible(Feature.FORM)) {
			addButton(addForm);
			if (!Feature.enabled(Feature.FORM))
				setFeatureDisabled(addForm);

			addForm.addClickHandler(event -> new AddDocumentUsingForm().show());
		}
	}

	private void addScan() {
		if (Feature.visible(Feature.SCAN) && Menu.enabled(Menu.SCAN)) {
			addButton(scan);
			if (!Feature.enabled(Feature.SCAN))
				setFeatureDisabled(scan);

			scan.addClickHandler(event -> Util.openScan());
		}
	}

	private void addUpload() {
		add.addClickHandler(event -> {
			new DocumentsUploader().show();
			event.cancel();
		});
		addButton(add);
	}

	private void addDropSpot() {
		if (Feature.visible(Feature.DROP_SPOT) && Menu.enabled(Menu.DROP_SPOT)) {
			addButton(dropSpot);
			if (!Feature.enabled(Feature.DROP_SPOT))
				setFeatureDisabled(dropSpot);

			dropSpot.addClickHandler(event -> DropSpotPopup.openDropSpot());
		}
	}

	private void addOffice() {
		if (Feature.visible(Feature.OFFICE)) {
			addButton(office);
			office.setTooltip(I18N.message("editwithoffice"));
			office.setTitle("<i class='fab fa-windows fa-lg fa-lg' aria-hidden='true'></i>");
			if (!Feature.enabled(Feature.OFFICE))
				setFeatureDisabled(office);

			office.addClickHandler(event -> checkPermissionsAndRun(
					new String[] { GUIAccessControlEntry.PERMISSION_DOWNLOAD, GUIAccessControlEntry.PERMISSION_WRITE },
					() -> Util.openEditWithOffice(document.getId())));
		}
	}

	private void addConvert() {
		if (Feature.visible(Feature.FORMAT_CONVERSION)) {
			addButton(convert);
			convert.setTooltip(I18N.message("convert"));
			if (!Feature.enabled(Feature.PDF))
				setFeatureDisabled(convert);

			convert.addClickHandler(event -> {
				new ConversionDialog(document).show();
				event.cancel();
			});
		}
	}

	private void addPdf() {
		if (Feature.visible(Feature.PDF)) {
			addButton(pdf);
			pdf.setTooltip(I18N.message("exportpdf"));
			if (!Feature.enabled(Feature.PDF))
				setFeatureDisabled(pdf);

			pdf.addClickHandler(
					event -> checkPermissionsAndRun(new String[] { GUIAccessControlEntry.PERMISSION_DOWNLOAD }, () -> {
						List<Long> selection = DocumentsPanel.get().getDocumentsGrid().getSelectedIds();
						if (selection.size() == 1) {
							DocUtil.downloadPdfConversion(document.getId(), document.getVersion());
						} else {
							StringBuilder url = new StringBuilder(Util.contextPath() + "convertpdf?open=true&docId=");
							for (long id : selection) {
								url.append(Long.toString(id));
								url.append(",");
							}
							Util.download(url.toString());
						}
					}));
		}
	}

	private void addDownload() {
		addButton(download);
		download.addClickHandler(
				event -> checkPermissionsAndRun(new String[] { GUIAccessControlEntry.PERMISSION_DOWNLOAD }, () -> {
					List<GUIDocument> selection = DocumentsPanel.get().getDocumentsGrid().getSelectedDocuments();
					if (selection.size() == 1) {
						long id = selection.get(0).getId();
						DocUtil.download(id, null);
					} else {
						StringBuilder url = new StringBuilder(GWT.getHostPageBaseURL());
						url.append("zip-export?folderId=");
						url.append(FolderController.get().getCurrentFolder().getId());
						for (GUIDocument rec : selection) {
							if (rec.isPasswordProtected()) {
								SC.warn(I18N.message("somedocsprotected"));
								break;
							}
							url.append("&docId=");
							url.append(rec.getId());
						}
						Util.download(url.toString());
					}
				}));
	}

	/**
	 * Checks if the user has all the specified permissions on the selected
	 * documents and runs the task
	 * 
	 * @param requiredPermissions The permissions required on the documents
	 *        selection
	 * @param task The task to run
	 */
	private void checkPermissionsAndRun(String[] requiredPermissions, Runnable task) {
		DocumentsGrid grid = DocumentsPanel.get().getDocumentsGrid();
		if (grid.getSelectedCount() == 0)
			return;

		DocumentService.Instance.get().getEnabledPermissions(grid.getSelectedIds(),
				new AsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIAccessControlEntry grantedPermissions) {
						for (String permission : requiredPermissions) {
							if (!grantedPermissions.isPermissionAllowed(permission.toLowerCase())) {
								GuiLog.warn(I18N.message("somedocsdonothaveperm", permission.toUpperCase()), null);
								return;
							}
						}
						task.run();
					}
				});
	}

	private void addRefresh() {
		refresh.addClickHandler(event -> {
			if (FolderController.get().getCurrentFolder() != null)
				FolderNavigator.get().selectFolder(FolderController.get().getCurrentFolder().getId());
		});
		refresh.setDisabled(FolderController.get().getCurrentFolder() == null);
		addButton(refresh);
	}

	/**
	 * Updates the toolbar state on the basis of the passed document and/or
	 * folder
	 * 
	 * @param document the currently selected document
	 * @param folder the currently selected folder
	 */
	public void update(GUIDocument document, GUIFolder folder) {
		try {
			if (folder == null)
				folder = FolderController.get().getCurrentFolder();

			this.document = document;

			if (document != null) {
				updateUsingDocument(document);
			} else {
				download.setDisabled(true);
				pdf.setDisabled(true);
				convert.setDisabled(true);
				subscribe.setDisabled(true);
				archive.setDisabled(true);
				startWorkflow.setDisabled(true);
				bulkUpdate.setDisabled(true);
				bulkCheckout.setDisabled(true);
				stamp.setDisabled(true);
				sign.setDisabled(true);
				office.setDisabled(true);
				addForm.setDisabled(true);
				readingRequest.setDisabled(true);
			}

			updateUsingFolder(folder);
		} catch (Exception t) {
			// Nothing to do
		}
	}

	private void updateUsingFolder(GUIFolder folder) {
		if (folder != null) {
			refresh.setDisabled(false);
			add.setDisabled(!folder.hasPermission(GUIAccessControlEntry.PERMISSION_WRITE));
			dropSpot.setDisabled(!folder.hasPermission(GUIAccessControlEntry.PERMISSION_WRITE)
					|| !folder.hasPermission(GUIAccessControlEntry.PERMISSION_IMPORT)
					|| !Feature.enabled(Feature.DROP_SPOT));
			addForm.setDisabled(
					!folder.hasPermission(GUIAccessControlEntry.PERMISSION_WRITE) || !Feature.enabled(Feature.FORM));
			scan.setDisabled(
					!folder.hasPermission(GUIAccessControlEntry.PERMISSION_WRITE) || !Feature.enabled(Feature.SCAN));
			bulkUpdate.setDisabled(!Feature.enabled(Feature.BULK_UPDATE) || !folder.isWrite());
			bulkCheckout
					.setDisabled(!Feature.enabled(Feature.BULK_CHECKOUT) || !folder.isDownload() || !folder.isWrite());
			addCalendarEvent.setDisabled(!folder.hasPermission(GUIAccessControlEntry.PERMISSION_CALENDAR)
					|| !Feature.enabled(Feature.CALENDAR));
			startWorkflow.setDisabled(!folder.hasPermission(GUIAccessControlEntry.PERMISSION_WORKFLOW)
					|| !Feature.enabled(Feature.WORKFLOW));

			list.setDisabled(false);
			gallery.setDisabled(false);
			togglePreview.setDisabled(false);
		} else {
			refresh.setDisabled(true);
			add.setDisabled(true);
			addForm.setDisabled(true);
			office.setDisabled(true);
			scan.setDisabled(true);
			bulkUpdate.setDisabled(true);
			bulkCheckout.setDisabled(true);
			startWorkflow.setDisabled(true);
			addCalendarEvent.setDisabled(true);
			dropSpot.setDisabled(true);
			list.setDisabled(false);
			gallery.setDisabled(false);
			togglePreview.setDisabled(false);
		}
	}

	private void setFeatureDisabled(ToolStripButton button) {
		button.setDisabled(true);
		button.setTooltip(I18N.message("featuredisabled"));
	}

	private void updateUsingDocument(GUIDocument document) {
		download.setDisabled(!document.isDownload());
		pdf.setDisabled(!Feature.enabled(Feature.PDF) || !document.isDownload());
		convert.setDisabled(!Feature.enabled(Feature.FORMAT_CONVERSION));
		subscribe.setDisabled(!Feature.enabled(Feature.AUDIT));
		bulkUpdate.setDisabled(!Feature.enabled(Feature.BULK_UPDATE) || !document.isWrite());
		bulkCheckout
				.setDisabled(!Feature.enabled(Feature.BULK_CHECKOUT) || !document.isDownload() || !document.isWrite());
		stamp.setDisabled(!Feature.enabled(Feature.STAMP) || !document.isWrite());
		sign.setDisabled(!Feature.enabled(Feature.DIGITAL_SIGNATURE) || !document.isWrite()
				|| !document.hasPermission(GUIAccessControlEntry.PERMISSION_SIGN));

		archive.setDisabled(
				!document.hasPermission(GUIAccessControlEntry.PERMISSION_ARCHIVE) || !Feature.enabled(Feature.IMPEX));
		startWorkflow.setDisabled(!document.hasPermission(GUIAccessControlEntry.PERMISSION_WORKFLOW)
				|| !Feature.enabled(Feature.WORKFLOW));
		readingRequest.setDisabled(!document.hasPermission(GUIAccessControlEntry.PERMISSION_READINGREQ)
				|| !Feature.enabled(Feature.READING_CONFIRMATION));
		addCalendarEvent.setDisabled(!document.hasPermission(GUIAccessControlEntry.PERMISSION_CALENDAR)
				|| !Feature.enabled(Feature.CALENDAR));

		boolean isOfficeFile = false;
		if (document.getFileName() != null)
			isOfficeFile = Util.isOfficeFile(document.getFileName());
		else if (document.getType() != null)
			isOfficeFile = Util.isOfficeFileType(document.getType());

		office.setDisabled(
				!Feature.enabled(Feature.OFFICE) || !isOfficeFile || !document.isDownload() || !document.isWrite());
		if (document.getStatus() != Constants.DOC_UNLOCKED && !Session.get().getUser().isMemberOf(Constants.GROUP_ADMIN)
				&& document.getLockUserId() != null
				&& Session.get().getUser().getId() != document.getLockUserId().longValue())
			office.setDisabled(true);
	}

	@Override
	public void onFolderSelected(GUIFolder folder) {
		update(null, folder);
	}

	@Override
	public void onFolderChanged(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderDeleted(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderCreated(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderMoved(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderBeginEditing(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderCancelEditing(GUIFolder folder) {
		// Nothing to do
	}

	private void saveGridState() {
		Session.get().getUser().setDocsGrid(DocumentsPanel.get().getDocsGridViewState());
		SecurityService.Instance.get().saveInterfaceSettings(Session.get().getUser(), new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable e) {
				GuiLog.serverError(e);
			}

			@Override
			public void onSuccess(GUIUser usr) {
				GuiLog.info(I18N.message("settingssaved"));
			}
		});
	}
}