package com.logicaldoc.gui.frontend.client.document;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.Window;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUICalendarEvent;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.observer.FolderController;
import com.logicaldoc.gui.common.client.observer.FolderObserver;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.frontend.client.calendar.CalendarEventDialog;
import com.logicaldoc.gui.frontend.client.document.form.AddForm;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsGrid;
import com.logicaldoc.gui.frontend.client.document.update.UpdateDialog;
import com.logicaldoc.gui.frontend.client.folder.FolderNavigator;
import com.logicaldoc.gui.frontend.client.subscription.SubscriptionDialog;
import com.smartgwt.client.types.SelectionType;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * The toolbar to handle some documents aspects
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class DocumentToolbar extends ToolStrip implements FolderObserver {
	protected ToolStripButton refresh = new ToolStripButton();

	protected ToolStripButton download = new ToolStripButton();

	protected ToolStripButton rss = new ToolStripButton();

	protected ToolStripButton pdf = new ToolStripButton();

	protected ToolStripButton add = new ToolStripButton();

	protected ToolStripButton addForm = new ToolStripButton();

	protected ToolStripButton dropSpot = new ToolStripButton();

	protected ToolStripButton subscribe = new ToolStripButton();

	protected ToolStripButton scan = new ToolStripButton();

	protected ToolStripButton convert = new ToolStripButton();

	protected ToolStripButton bulkUpdate = new ToolStripButton();

	protected ToolStripButton bulkCheckout = new ToolStripButton();

	protected ToolStripButton stamp = new ToolStripButton();

	protected ToolStripButton sign = new ToolStripButton();

	protected ToolStripButton archive = new ToolStripButton();

	protected ToolStripButton startWorkflow = new ToolStripButton();

	protected ToolStripButton addCalendarEvent = new ToolStripButton();

	protected ToolStripButton print = new ToolStripButton();

	protected ToolStripButton export = new ToolStripButton();

	protected ToolStripButton office = new ToolStripButton();

	protected ToolStripButton list = new ToolStripButton();

	protected ToolStripButton gallery = new ToolStripButton();

	protected ToolStripButton togglePreview = new ToolStripButton();

	protected GUIDocument document;

	public DocumentToolbar() {
		GUIFolder folder = Session.get().getCurrentFolder();
		boolean downloadEnabled = folder != null && folder.isDownload();
		boolean writeEnabled = folder != null && folder.isWrite();
		boolean signEnabled = folder != null && folder.hasPermission(Constants.PERMISSION_SIGN);

		prepareButtons(downloadEnabled, writeEnabled, signEnabled);

		update(null);
		FolderController.get().addObserver(this);
	}

	protected void prepareButtons(boolean downloadEnabled, boolean writeEnabled, boolean signEnabled) {
		refresh.setTooltip(I18N.message("refresh"));
		refresh.setIcon(ItemFactory.newImgIcon("arrow_refresh_small.png").getSrc());
		refresh.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (Session.get().getCurrentFolder() != null)
					FolderNavigator.get().selectFolder(Session.get().getCurrentFolder().getId());
			}
		});
		refresh.setDisabled(Session.get().getCurrentFolder() == null);
		addButton(refresh);
		addSeparator();

		download.setTooltip(I18N.message("download"));
		download.setIcon(ItemFactory.newImgIcon("download.png").getSrc());
		download.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				DocumentsGrid grid = DocumentsPanel.get().getDocumentsGrid();
				if (grid.getSelectedCount() == 0)
					return;

				long[] selection = grid.getSelectedIds();
				if (selection.length == 1) {
					WindowUtils.openUrl(Util.downloadURL(selection[0]));
				} else {
					String url = Util.contextPath() + "zip-export?folderId=" + Session.get().getCurrentFolder().getId();
					for (long id : selection)
						url += "&docId=" + Long.toString(id);
					WindowUtils.openUrl(url);
				}
			}
		});

		rss.setIcon(ItemFactory.newImgIcon("feed_add.png").getSrc());
		rss.setTooltip(I18N.message("rssfeed"));
		rss.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				Window.open(Util.contextPath() + "doc_rss?docId=" + document.getId() + "&locale=" + I18N.getLocale(),
						"_blank", "");
			}
		});

		pdf.setIcon(ItemFactory.newImgIcon("pdf.png").getSrc());
		pdf.setTooltip(I18N.message("exportpdf"));
		pdf.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				DocumentsGrid grid = DocumentsPanel.get().getDocumentsGrid();
				if (grid.getSelectedCount() == 0)
					return;

				long[] selection = grid.getSelectedIds();
				if (selection.length == 1) {
					DocUtil.downloadPdfConversion(document.getId(), document.getVersion());
				} else {
					String url = Util.contextPath() + "convertpdf?open=true&docId=";
					for (long id : selection)
						url += Long.toString(id) + "|";
					WindowUtils.openUrl(url, "_blank");
				}
			}
		});

		convert.setIcon(ItemFactory.newImgIcon("page_white_gear.png").getSrc());
		convert.setTooltip(I18N.message("convert"));
		convert.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				ConversionDialog dialog = new ConversionDialog(document);
				dialog.show();
				event.cancel();
			}
		});

		add.setIcon(ItemFactory.newImgIcon("page_white_add.png").getSrc());
		add.setTooltip(I18N.message("adddocuments"));
		add.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				DocumentsUploader uploader = new DocumentsUploader();
				uploader.show();
				event.cancel();
			}
		});

		addForm.setIcon(ItemFactory.newImgIcon("application_form_add.png").getSrc());
		addForm.setTooltip(I18N.message("addform"));
		addForm.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AddForm dialog = new AddForm();
				dialog.show();
				event.cancel();
			}
		});

		subscribe.setIcon(ItemFactory.newImgIcon("subscription_add.png").getSrc());
		subscribe.setTooltip(I18N.message("subscribe"));
		subscribe.setDisabled(true);
		subscribe.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				DocumentsGrid grid = DocumentsPanel.get().getDocumentsGrid();
				if (grid.getSelectedCount() == 0)
					return;

				SubscriptionDialog dialog = new SubscriptionDialog(null, grid.getSelectedIds());
				dialog.show();
			}
		});

		dropSpot.setIcon(ItemFactory.newImgIcon("drive_add.png").getSrc());
		dropSpot.setTooltip("Drop Spot");
		dropSpot.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				Util.openDropSpot();
			}
		});

		scan.setIcon(ItemFactory.newImgIcon("image_add.png").getSrc());
		scan.setTooltip(I18N.message("scandocument"));
		scan.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				Map<String, String> params = new HashMap<String, String>();
				params.put("targetFolderId", "" + Session.get().getCurrentFolder().getId());

				WindowUtils.openUrl(Util.webstartURL("scan", params), "_self");
			}
		});

		archive.setIcon(ItemFactory.newImgIcon("server_add.png").getSrc());
		archive.setTooltip(I18N.message("sendtoexparchive"));
		archive.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				DocumentsGrid grid = DocumentsPanel.get().getDocumentsGrid();
				if (grid.getSelectedCount() == 0)
					return;

				SendToArchiveDialog archiveDialog = new SendToArchiveDialog(grid.getSelectedIds(), true);
				archiveDialog.show();
			}
		});

		startWorkflow.setIcon(ItemFactory.newImgIcon("cog_go.png").getSrc());
		startWorkflow.setTooltip(I18N.message("startworkflow"));
		startWorkflow.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				DocumentsGrid grid = DocumentsPanel.get().getDocumentsGrid();
				if (grid.getSelectedCount() == 0)
					return;

				WorkflowDialog workflowDialog = new WorkflowDialog(grid.getSelectedIds());
				workflowDialog.show();
			}
		});

		addCalendarEvent.setIcon(ItemFactory.newImgIcon("calendar_add.png").getSrc());
		addCalendarEvent.setTooltip(I18N.message("newcalendarevent"));
		addCalendarEvent.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				DocumentsGrid grid = DocumentsPanel.get().getDocumentsGrid();
				if (grid.getSelectedCount() == 0)
					return;

				GUIDocument[] docs = grid.getSelectedDocuments();

				GUICalendarEvent calEvent = new GUICalendarEvent();
				calEvent.setCreator(Session.get().getUser().getFullName());
				calEvent.setCreatorId(Session.get().getUser().getId());
				GUIUser user = new GUIUser();
				user.setId(Session.get().getUser().getId());
				user.setUserName(Session.get().getUser().getUserName());
				user.setFirstName(Session.get().getUser().getFirstName());
				user.setName(Session.get().getUser().getName());
				calEvent.addParticipant(user);
				calEvent.setDocuments(docs);
				calEvent.setTitle(Util.getBaseName(docs[0].getFileName()));
				calEvent.setType(docs[0].getTemplate());
				CalendarEventDialog eventDialog = new CalendarEventDialog(calEvent, null);
				eventDialog.show();
			}
		});

		list.setTooltip(I18N.message("list"));
		list.setIcon(ItemFactory.newImgIcon("application_view_list.png").getSrc());
		list.setActionType(SelectionType.RADIO);
		list.setRadioGroup("mode");
		list.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				CookiesManager.save(CookiesManager.COOKIE_DOCSLIST_MODE, DocumentsGrid.MODE_LIST);
				DocumentsPanel.get().refresh(null, 1, DocumentsGrid.MODE_LIST);
			}
		});
		list.setDisabled(Session.get().getCurrentFolder() == null);

		gallery.setTooltip(I18N.message("gallery"));
		gallery.setIcon(ItemFactory.newImgIcon("application_view_tile.png").getSrc());
		gallery.setActionType(SelectionType.RADIO);
		gallery.setRadioGroup("mode");
		gallery.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (Session.get().getCurrentFolder() != null)
					CookiesManager.save(CookiesManager.COOKIE_DOCSLIST_MODE, DocumentsGrid.MODE_GALLERY);
				DocumentsPanel.get().refresh(null, 1, DocumentsGrid.MODE_GALLERY);
			}
		});
		gallery.setDisabled(Session.get().getCurrentFolder() == null);

		int mode = DocumentsGrid.MODE_LIST;
		if (CookiesManager.get(CookiesManager.COOKIE_DOCSLIST_MODE) != null
				&& !CookiesManager.get(CookiesManager.COOKIE_DOCSLIST_MODE).equals(""))
			mode = Integer.parseInt(CookiesManager.get(CookiesManager.COOKIE_DOCSLIST_MODE));
		if (mode == DocumentsGrid.MODE_LIST)
			list.setSelected(true);
		else
			gallery.setSelected(true);

		setHeight(27);
		addButton(download);

		if (Feature.visible(Feature.PDF)) {
			addButton(pdf);
			if (!Feature.enabled(Feature.PDF) || !downloadEnabled) {
				pdf.setDisabled(true);
				pdf.setTooltip(I18N.message("featuredisabled"));
			}
		}

		if (Feature.visible(Feature.FORMAT_CONVERSION)) {
			addButton(convert);
			if (!Feature.enabled(Feature.PDF)) {
				convert.setDisabled(true);
				convert.setTooltip(I18N.message("featuredisabled"));
			}
		}

		if (Feature.visible(Feature.OFFICE)) {
			addButton(office);

			if (!Feature.enabled(Feature.OFFICE) || (document != null && !Util.isOfficeFile(document.getFileName()))
					|| !downloadEnabled || !writeEnabled)
				office.setDisabled(true);
			else
				office.setDisabled(false);

			if (!Feature.enabled(Feature.OFFICE)) {
				office.setDisabled(true);
				office.setTooltip(I18N.message("featuredisabled"));
			}
		}

		addSeparator();
		addButton(add);

		if (Feature.visible(Feature.DROP_SPOT)
				&& !"embedded".equals(Session.get().getInfo().getConfig("gui.dropspot.mode"))) {
			addButton(dropSpot);
			if (!Feature.enabled(Feature.DROP_SPOT)) {
				dropSpot.setDisabled(true);
				dropSpot.setTooltip(I18N.message("featuredisabled"));
			}
		}

		if (Feature.visible(Feature.SCAN)) {
			addButton(scan);
			if (!Feature.enabled(Feature.SCAN)) {
				scan.setDisabled(true);
				scan.setTooltip(I18N.message("featuredisabled"));
			}
		}

		if (Feature.visible(Feature.FORM)) {
			addButton(addForm);
			if (!Feature.enabled(Feature.FORM)) {
				addForm.setDisabled(true);
				addForm.setTooltip(I18N.message("featuredisabled"));
			}
		}

		office.setTooltip(I18N.message("editwithoffice"));
		office.setIcon(ItemFactory.newImgIcon("page_white_office.png").getSrc());
		office.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (document == null)
					return;

				WindowUtils.openUrl("ldedit:" + GWT.getHostPageBaseURL() + "ldedit?action=edit&sid="
						+ Session.get().getSid() + "&docId=" + document.getId());
			}
		});

		bulkUpdate.setIcon(ItemFactory.newImgIcon("application_form_edit.png").getSrc());
		bulkUpdate.setTooltip(I18N.message("bulkupdate"));
		bulkUpdate.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				DocumentsGrid grid = DocumentsPanel.get().getDocumentsGrid();
				if (grid.getSelectedCount() == 0)
					return;

				UpdateDialog dialog = new UpdateDialog(grid.getSelectedIds(), null, UpdateDialog.CONTEXT_UPDATE, false);
				dialog.show();
			}
		});

		stamp.setIcon(ItemFactory.newImgIcon("stamp.png").getSrc());
		stamp.setTooltip(I18N.message("stamp"));
		stamp.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				DocumentsGrid grid = DocumentsPanel.get().getDocumentsGrid();
				if (grid.getSelectedCount() == 0)
					return;

				StampDialog dialog = new StampDialog(grid.getSelectedIds());
				dialog.show();
			}
		});

		sign.setIcon(ItemFactory.newImgIcon("rosette.png").getSrc());
		sign.setTooltip(I18N.message("sign"));
		sign.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				DocumentsGrid grid = DocumentsPanel.get().getDocumentsGrid();
				if (grid.getSelectedCount() == 0)
					return;

				SignatureDialog dialog = new SignatureDialog(grid.getSelectedIds());
				dialog.show();
			}
		});

		bulkCheckout.setIcon(ItemFactory.newImgIcon("page_edit.png").getSrc());
		bulkCheckout.setTooltip(I18N.message("bulkcheckout"));
		bulkCheckout.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				DocumentsGrid grid = DocumentsPanel.get().getDocumentsGrid();
				if (grid.getSelectedCount() == 0)
					return;

				GUIDocument docs[] = grid.getSelectedDocuments();
				List<Long> unlockedIds = new ArrayList<Long>();
				for (GUIDocument doc : docs)
					if (doc.getStatus() == 0 && doc.getImmutable() == 0)
						unlockedIds.add(doc.getId());

				Map<String, String> params = new HashMap<String, String>();
				params.put("folderId", "" + Session.get().getCurrentFolder().getId());
				params.put("docIds", unlockedIds.toString().replace('[', ' ').replace(']', ' ').trim());

				WindowUtils.openUrl(Util.webstartURL("bulk-checkout", params), "_self");
			}
		});

		if (Feature.visible(Feature.AUDIT)) {
			addSeparator();
			addButton(subscribe);
			if (!Feature.enabled(Feature.AUDIT)) {
				subscribe.setDisabled(true);
				subscribe.setTooltip(I18N.message("featuredisabled"));
			}
		}

		if (Feature.visible(Feature.RSS)) {
			addButton(rss);
			if (!Feature.enabled(Feature.RSS)) {
				rss.setDisabled(true);
				rss.setTooltip(I18N.message("featuredisabled"));
			}
		}

		if (Feature.visible(Feature.IMPEX)) {
			addSeparator();
			addButton(archive);
			if (!Feature.enabled(Feature.IMPEX)) {
				archive.setDisabled(true);
				archive.setTooltip(I18N.message("featuredisabled"));
			}
		}

		if (Feature.visible(Feature.BULK_UPDATE) || Feature.visible(Feature.BULK_CHECKOUT))
			addSeparator();

		if (Feature.visible(Feature.BULK_UPDATE)) {
			addButton(bulkUpdate);
			if (!Feature.enabled(Feature.BULK_UPDATE)) {
				bulkUpdate.setDisabled(true);
				bulkUpdate.setTooltip(I18N.message("featuredisabled"));
			}
		}

		if (Feature.visible(Feature.BULK_CHECKOUT)) {
			addButton(bulkCheckout);
			if (!Feature.enabled(Feature.BULK_CHECKOUT)) {
				bulkCheckout.setDisabled(true);
				bulkCheckout.setTooltip(I18N.message("featuredisabled"));
			}
		}

		if (Feature.visible(Feature.STAMP)) {
			addSeparator();
			addButton(stamp);
			if (!Feature.enabled(Feature.STAMP)) {
				stamp.setDisabled(true);
				stamp.setTooltip(I18N.message("featuredisabled"));
			}
		}

		if (Feature.visible(Feature.DIGITAL_SIGNATURE)) {
			addButton(sign);
			if (!Feature.enabled(Feature.DIGITAL_SIGNATURE) || !signEnabled || !writeEnabled) {
				sign.setDisabled(true);
				sign.setTooltip(I18N.message("featuredisabled"));
			}
		}

		if (Feature.visible(Feature.WORKFLOW)) {
			addSeparator();
			addButton(startWorkflow);
			if (!Feature.enabled(Feature.WORKFLOW)) {
				startWorkflow.setDisabled(true);
				startWorkflow.setTooltip(I18N.message("featuredisabled"));
			}
		}

		if (Feature.visible(Feature.CALENDAR)) {
			addSeparator();
			addButton(addCalendarEvent);
			if (!Feature.enabled(Feature.CALENDAR)) {
				addCalendarEvent.setDisabled(true);
				addCalendarEvent.setTooltip(I18N.message("featuredisabled"));
			}
		}

		addSeparator();
		ToolStripButton filter = new ToolStripButton();
		filter.setIcon(ItemFactory.newImgIcon("filter.png").getSrc());
		filter.setTooltip(I18N.message("filter"));
		filter.setActionType(SelectionType.CHECKBOX);
		addButton(filter);
		filter.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				DocumentsPanel.get().toggleFilters();
			}
		});

		addSeparator();
		print.setIcon(ItemFactory.newImgIcon("printer.png").getSrc());
		print.setTooltip(I18N.message("print"));
		print.setAutoFit(true);
		addButton(print);
		print.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				DocumentsPanel.get().printPreview();
			}
		});

		if (Feature.visible(Feature.EXPORT_CSV)) {
			addSeparator();
			export.setIcon(ItemFactory.newImgIcon("table_row_insert.png").getSrc());
			export.setTooltip(I18N.message("export"));
			export.setAutoFit(true);
			addButton(export);
			export.addClickHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					DocumentsPanel.get().export();
				}
			});
			if (!Feature.enabled(Feature.EXPORT_CSV)) {
				export.setDisabled(true);
				export.setTooltip(I18N.message("featuredisabled"));
			}
		}

		togglePreview.setIcon(ItemFactory.newImgIcon("application_side_expand.png").getSrc());
		togglePreview.setTooltip(I18N.message("closepreview"));
		try {
			// Retrieve the saved preview width
			String w = CookiesManager.get(CookiesManager.COOKIE_DOCSLIST_PREV_W);
			if (Integer.parseInt(w) <= 0) {
				togglePreview.setIcon(ItemFactory.newImgIcon("application_side_contract.png").getSrc());
				togglePreview.setTooltip(I18N.message("openpreview"));
			}
		} catch (Throwable t) {
		}
		togglePreview.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (DocumentsPanel.get().getPreviewPanel().isVisible()
						&& DocumentsPanel.get().getPreviewPanel().getWidth() > 0) {
					DocumentsPanel.get().getPreviewPanel().setWidth(0);
					togglePreview.setIcon(ItemFactory.newImgIcon("application_side_contract.png").getSrc());
					togglePreview.setTooltip(I18N.message("openpreview"));
				} else {
					DocumentsPanel.get().getPreviewPanel().setWidth(350);
					DocumentsPanel.get().getPreviewPanel().setDocument(document);
					togglePreview.setIcon(ItemFactory.newImgIcon("application_side_expand.png").getSrc());
					togglePreview.setTooltip(I18N.message("closepreview"));
				}
			}
		});

		addSeparator();
		addButton(list);
		addButton(gallery);
		addButton(togglePreview);
	}

	/**
	 * Updates the toolbar state on the basis of the passed document
	 */
	public void update(final GUIDocument document) {
		try {
			GUIFolder folder = Session.get().getCurrentFolder();
			boolean downloadEnabled = folder != null && folder.isDownload();
			boolean writeEnabled = folder != null && folder.isWrite();
			boolean signEnabled = folder != null && folder.hasPermission(Constants.PERMISSION_SIGN);

			this.document = document;

			if (document != null) {
				download.setDisabled(!downloadEnabled);
				office.setDisabled(!downloadEnabled);
				rss.setDisabled(!Feature.enabled(Feature.RSS) || !downloadEnabled);
				pdf.setDisabled(!Feature.enabled(Feature.PDF) || !downloadEnabled);
				if (!pdf.isDisabled())
					pdf.setTooltip(I18N.message("exportpdf"));
				convert.setDisabled(!Feature.enabled(Feature.FORMAT_CONVERSION));
				if (!convert.isDisabled())
					convert.setTooltip(I18N.message("convert"));
				subscribe.setDisabled(!Feature.enabled(Feature.AUDIT));
				bulkUpdate.setDisabled(!Feature.enabled(Feature.BULK_UPDATE) || !writeEnabled);
				bulkCheckout.setDisabled(!Feature.enabled(Feature.BULK_CHECKOUT) || !downloadEnabled || !writeEnabled);
				stamp.setDisabled(!Feature.enabled(Feature.STAMP) || !writeEnabled);
				if (!stamp.isDisabled())
					stamp.setTooltip(I18N.message("stamp"));
				sign.setDisabled(!Feature.enabled(Feature.DIGITAL_SIGNATURE) || !writeEnabled || !signEnabled);
				if (!sign.isDisabled())
					sign.setTooltip(I18N.message("sign"));
				addCalendarEvent.setDisabled(!Feature.enabled(Feature.CALENDAR));

				boolean isOfficeFile = false;
				if (document.getFileName() != null)
					isOfficeFile = Util.isOfficeFile(document.getFileName());
				else if (document.getType() != null)
					isOfficeFile = Util.isOfficeFileType(document.getType());

				office.setDisabled(!Feature.enabled(Feature.OFFICE) || !isOfficeFile || !downloadEnabled
						|| (folder != null && !folder.hasPermission(Constants.PERMISSION_WRITE)));
				if (document.getStatus() != Constants.DOC_UNLOCKED
						&& !Session.get().getUser().isMemberOf(Constants.GROUP_ADMIN)) {
					if (document.getLockUserId() != null
							&& Session.get().getUser().getId() != document.getLockUserId().longValue())
						office.setDisabled(true);
				}
				office.setTooltip(I18N.message("editwithoffice"));
			} else {
				download.setDisabled(true);
				rss.setDisabled(true);
				pdf.setDisabled(true);
				convert.setDisabled(true);
				subscribe.setDisabled(true);
				archive.setDisabled(true);
				startWorkflow.setDisabled(true);
				addCalendarEvent.setDisabled(true);
				bulkUpdate.setDisabled(true);
				bulkCheckout.setDisabled(true);
				stamp.setDisabled(true);
				sign.setDisabled(true);
				office.setDisabled(true);
				addForm.setDisabled(true);
			}

			if (folder != null) {
				refresh.setDisabled(false);
				add.setDisabled(!folder.hasPermission(Constants.PERMISSION_WRITE));
				dropSpot.setDisabled(!folder.hasPermission(Constants.PERMISSION_WRITE)
						|| !Feature.enabled(Feature.DROP_SPOT));
				addForm.setDisabled(!folder.hasPermission(Constants.PERMISSION_WRITE) || !Feature.enabled(Feature.FORM));
				scan.setDisabled(!folder.hasPermission(Constants.PERMISSION_WRITE) || !Feature.enabled(Feature.SCAN));
				archive.setDisabled(document == null || !folder.hasPermission(Constants.PERMISSION_ARCHIVE)
						|| !Feature.enabled(Feature.IMPEX));
				startWorkflow.setDisabled(document == null || !folder.hasPermission(Constants.PERMISSION_WORKFLOW)
						|| !Feature.enabled(Feature.WORKFLOW));
				addCalendarEvent.setDisabled(document == null || !folder.hasPermission(Constants.PERMISSION_CALENDAR)
						|| !Feature.enabled(Feature.CALENDAR));
				list.setDisabled(false);
				gallery.setDisabled(false);
				togglePreview.setDisabled(false);
			} else {
				refresh.setDisabled(true);
				add.setDisabled(true);
				addForm.setDisabled(true);
				office.setDisabled(true);
				scan.setDisabled(true);
				archive.setDisabled(true);
				startWorkflow.setDisabled(true);
				bulkUpdate.setDisabled(true);
				bulkCheckout.setDisabled(true);
				dropSpot.setDisabled(true);
				addCalendarEvent.setDisabled(true);
				list.setDisabled(false);
				gallery.setDisabled(false);
				togglePreview.setDisabled(false);
			}
		} catch (Throwable t) {

		}
	}

	@Override
	public void onFolderSelected(GUIFolder folder) {
		update(null);
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
}