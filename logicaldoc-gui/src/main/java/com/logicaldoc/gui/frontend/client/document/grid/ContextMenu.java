package com.logicaldoc.gui.frontend.client.document.grid;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAutomationRoutine;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIMenu;
import com.logicaldoc.gui.common.client.beans.GUIVersion;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.preview.PreviewPopup;
import com.logicaldoc.gui.frontend.client.clipboard.Clipboard;
import com.logicaldoc.gui.frontend.client.document.ComparisonWindow;
import com.logicaldoc.gui.frontend.client.document.ConversionDialog;
import com.logicaldoc.gui.frontend.client.document.DocumentCheckin;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.document.EmailDialog;
import com.logicaldoc.gui.frontend.client.document.SendToArchiveDialog;
import com.logicaldoc.gui.frontend.client.document.StartWorkflowDialog;
import com.logicaldoc.gui.frontend.client.document.TicketDialog;
import com.logicaldoc.gui.frontend.client.document.reading.ReadingRequestDialog;
import com.logicaldoc.gui.frontend.client.document.signature.DigitalSignatureDialog;
import com.logicaldoc.gui.frontend.client.document.split.SplitDialog;
import com.logicaldoc.gui.frontend.client.document.stamp.StampDialog;
import com.logicaldoc.gui.frontend.client.folder.AutomationDialog;
import com.logicaldoc.gui.frontend.client.services.AutomationService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.AutoComplete;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * This context menu is used for grids containing document records.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ContextMenu extends Menu {

	private static final String FILENAME = "filename";

	private DocumentsGrid grid;

	private MenuItem download;

	private MenuItem cut;

	private MenuItem copy;

	private MenuItem rename;

	private MenuItem delete;

	private MenuItem sendMail;

	private MenuItem readingRequest;

	private MenuItem links;

	private MenuItem immutable;

	private MenuItem setPassword;

	private MenuItem unsetPassword;

	private MenuItem lock;

	private MenuItem unlock;

	private MenuItem checkout;

	private MenuItem checkin;

	private MenuItem archive;

	private MenuItem bookmark;

	private MenuItem markUnindexable;

	private MenuItem markIndexable;

	private MenuItem markIndexableMetadataOnly;

	private MenuItem index;

	private MenuItem sign;

	private MenuItem stamp;

	private MenuItem office;

	private MenuItem sendToExpArchive;

	private MenuItem workflow;

	private MenuItem automation;

	private MenuItem preview;

	private MenuItem ticket;

	private MenuItem convert;

	private MenuItem compare;

	private MenuItem replaceAlias;

	private MenuItem split;

	private MenuItem merge;

	private MenuItem customActionsItem;

	private MenuItem more;

	private Menu indexingMenu;

	private MenuItem indexing;

	private Menu moreMenu;

	public ContextMenu(final GUIFolder folder, final DocumentsGrid xxgrid) {
		this.grid = xxgrid;
		final GUIDocument[] selection = grid.getSelectedDocuments();
		final Long[] selectionIds = grid.getSelectedIds();

		download = new MenuItem();
		download.setTitle(I18N.message("download"));
		download.addClickHandler(event -> onDownload(folder, selection));

		cut = prepareCutItem(selection);

		copy = prepareCopyItem(selection);

		rename = prepareRenameItem(selection);

		delete = prepareDeleteItem(selection);

		sendMail = new MenuItem();
		sendMail.setTitle(I18N.message("sendmail"));
		sendMail.addClickHandler(event -> new EmailDialog(grid.getSelectedIds(), selection[0].getFileName()).show());

		links = prepareLinksItem(selection, selectionIds);

		immutable = prepareImmutableItem(selection, selectionIds);

		setPassword = prepareSetPasswordItem(selection);

		unsetPassword = prepareUnsetPasswordItem(selection);

		lock = prepareLockItem(selection, selectionIds);

		unlock = prepareUnlockItem(selection, selectionIds);

		checkout = prepareCheckoutItem(folder, selection, selectionIds);

		checkin = prepareCheckinItem(selection);

		archive = prepareArchiveItem(selectionIds);

		bookmark = prepareBookmarkItem(selection, selectionIds);

		markUnindexable = prepareMarkUnindexableItem(selection, selectionIds);

		markIndexable = prepareMarkIndexableItem(selection, selectionIds);

		markIndexableMetadataOnly = markIndexableMetadataOnlyItem(selection, selectionIds);

		index = prepareIndexItem(selection, selectionIds);

		sign = new MenuItem();
		sign.setTitle(I18N.message("sign"));
		sign.addClickHandler(event -> new DigitalSignatureDialog(selectionIds).show());

		stamp = new MenuItem();
		stamp.setTitle(I18N.message("stamp"));
		stamp.addClickHandler(event -> new StampDialog(grid).show());

		office = new MenuItem(I18N.message("editwithoffice"));
		office.addClickHandler(event -> Util.openEditWithOffice(selection[0].getId()));

		sendToExpArchive = new MenuItem(I18N.message("sendtoexparchive"));
		sendToExpArchive.addClickHandler(event -> new SendToArchiveDialog(selectionIds, true).show());

		workflow = new MenuItem(I18N.message("startworkflow"));
		workflow.addClickHandler(event -> new StartWorkflowDialog(selectionIds).show());

		automation = new MenuItem(I18N.message("executeautomation"));
		automation.addClickHandler(event -> new AutomationDialog(folder.getId(), selectionIds).show());

		preview = preparePreview();

		ticket = new MenuItem(I18N.message("ticket"));
		ticket.addClickHandler(event -> new TicketDialog(selection[0]).show());

		convert = new MenuItem(I18N.message("convert"));
		convert.addClickHandler(event -> new ConversionDialog(selection[0]).show());

		compare = prepareCompareItem(selection);

		replaceAlias = prepareReplaceAlias();

		split = new MenuItem(I18N.message("split"));
		split.addClickHandler(event -> new SplitDialog(selection[0]).show());

		merge = prepareMergeItem(folder, selectionIds);

		customActionsItem = prepareCustomActionsItem(folder.getId(), selectionIds);

		readingRequest = new MenuItem();
		readingRequest.setTitle(I18N.message("requestreading"));
		readingRequest.addClickHandler(event -> new ReadingRequestDialog(grid.getSelectedIds()).show());

		setItems(download, preview, cut, copy, rename, delete, bookmark, sendMail, links, office, checkout, checkin,
				lock, unlock);

		more = new MenuItem(I18N.message("more"));
		addItem(more);

		indexingMenu = new Menu();
		indexingMenu.setItems(index, markIndexable, markIndexableMetadataOnly, markUnindexable);

		indexing = new MenuItem(I18N.message("indexing"));
		indexing.setSubmenu(indexingMenu);

		moreMenu = new Menu();
		moreMenu.setItems(indexing, immutable, setPassword, unsetPassword, ticket, replaceAlias);

		removeOfficeItem(office);

		addArchiveItem(archive);
		addCustomActionsItem(customActionsItem);

		addConversionItem(convert, moreMenu);
		addCompareItem(compare, moreMenu);
		addSignItem(sign, moreMenu);
		addStampItem(stamp, moreMenu);
		addSplitItem(split, moreMenu);
		addReadingRequestItem(readingRequest, moreMenu);

		moreMenu.addItem(merge);

		addSendToArchiveItem(sendToExpArchive, moreMenu);
		addWorkflowItem(workflow, moreMenu);
		addAutomationItem(automation, moreMenu);

		more.setSubmenu(moreMenu);

		/**
		 * Now implement the security policies
		 */
		boolean someSelection = selection != null && selection.length > 0;
		boolean moreSelected = selection != null && selection.length > 1;
		boolean justOneSelected = someSelection && selection.length == 1;
		boolean immutablesInSelection = someSelection && checkImmutablesInSelection(selection);

		applySecurityPolicies(folder, selection, someSelection, moreSelected, justOneSelected, immutablesInSelection);
	}

	private void applySecurityPolicies(final GUIFolder folder, final GUIDocument[] selection, boolean someSelection,
			boolean moreSelected, boolean justOneSelected, boolean immutablesInSelection) {
		preview.setEnabled(someSelection
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.PREVIEW));
		cut.setEnabled(someSelection && !immutablesInSelection
				&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection) && folder.isMove());

		applyLockingSecurity(folder, selection, someSelection, immutablesInSelection, justOneSelected);

		immutable.setEnabled(
				someSelection && !immutablesInSelection && checkStatusInSelection(Constants.DOC_UNLOCKED, selection)
						&& folder.hasPermission(Constants.PERMISSION_IMMUTABLE));
		applySignSecurity(folder, selection, someSelection, immutablesInSelection);

		stamp.setEnabled(
				someSelection && !immutablesInSelection && checkStatusInSelection(Constants.DOC_UNLOCKED, selection)
						&& folder.isWrite() && Feature.enabled(Feature.STAMP));
		delete.setEnabled(someSelection && !immutablesInSelection
				&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection) && folder.isDelete());
		links.setEnabled(!Clipboard.getInstance().isEmpty() && folder.isWrite());

		applyIndexableSecurity(folder, selection, someSelection, immutablesInSelection);

		applyIndexSecurity(selection, someSelection, immutablesInSelection);

		applyPasswordSecurity(folder, selection, justOneSelected, immutablesInSelection);

		sendMail.setEnabled(someSelection && folder.hasPermission(Constants.PERMISSION_EMAIL));

		copy.setEnabled(someSelection);
		rename.setEnabled(
				justOneSelected && !immutablesInSelection && checkStatusInSelection(Constants.DOC_UNLOCKED, selection)
						&& folder.hasPermission(Constants.PERMISSION_RENAME));

		applyDownloadSecurity(folder, someSelection, justOneSelected);

		applyOfficeSecurity(folder, justOneSelected);

		convert.setEnabled(justOneSelected && Feature.enabled(Feature.FORMAT_CONVERSION));
		archive.setEnabled(someSelection && folder.hasPermission(Constants.PERMISSION_ARCHIVE)
				&& Feature.enabled(Feature.ARCHIVING));
		sendToExpArchive.setEnabled(
				someSelection && folder.hasPermission(Constants.PERMISSION_EXPORT) && Feature.enabled(Feature.IMPEX));
		workflow.setEnabled(someSelection && folder.hasPermission(Constants.PERMISSION_WORKFLOW)
				&& Feature.enabled(Feature.WORKFLOW));
		replaceAlias.setEnabled(justOneSelected && folder.isWrite() && selection[0].getDocRef() != null);
		readingRequest.setEnabled(someSelection && folder.hasPermission(Constants.PERMISSION_READINGREQ)
				&& Feature.enabled(Feature.READING_CONFIRMATION));

		applySplitSecurity(folder, selection, moreSelected, justOneSelected);

		applyCompareSecurity(selection);

		automation.setEnabled(
				Feature.enabled(Feature.AUTOMATION) && folder.hasPermission(Constants.PERMISSION_AUTOMATION));
	}

	private void applySplitSecurity(final GUIFolder folder, final GUIDocument[] selection, boolean moreSelected,
			boolean justOneSelected) {
		split.setEnabled(
				justOneSelected && selection[0].getFileName().toLowerCase().endsWith(".pdf") && folder.isWrite());
		merge.setEnabled(moreSelected && folder.isWrite());
	}

	private void applyOfficeSecurity(final GUIFolder folder, boolean justOneSelected) {
		office.setEnabled(
				justOneSelected && Feature.enabled(Feature.OFFICE) && folder.hasPermission(Constants.PERMISSION_WRITE)
						&& folder.hasPermission(Constants.PERMISSION_DOWNLOAD)
						&& Util.isOfficeFile(grid.getSelectedDocument().getFileName()));
	}

	private void applyDownloadSecurity(final GUIFolder folder, boolean someSelection, boolean justOneSelected) {
		ticket.setEnabled(justOneSelected && folder.isDownload());
		download.setEnabled(someSelection && folder.isDownload());
	}

	private void applyLockingSecurity(final GUIFolder folder, final GUIDocument[] selection, boolean someSelection,
			boolean immutablesInSelection, boolean justOneSelected) {
		unlock.setEnabled(
				someSelection && !immutablesInSelection && (checkStatusInSelection(Constants.DOC_LOCKED, selection)
						|| checkStatusInSelection(Constants.DOC_CHECKED_OUT, selection)));
		lock.setEnabled(someSelection && !immutablesInSelection && folder.isWrite()
				&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection));
		checkout.setEnabled(someSelection && !immutablesInSelection && folder.isDownload() && folder.isWrite()
				&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection));
		checkin.setEnabled(justOneSelected && !immutablesInSelection && folder.isWrite()
				&& checkStatusInSelection(Constants.DOC_CHECKED_OUT, selection));
	}

	private void applyIndexableSecurity(final GUIFolder folder, final GUIDocument[] selection, boolean someSelection,
			boolean immutablesInSelection) {
		markIndexable.setEnabled(someSelection && !immutablesInSelection
				&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection) && folder.isWrite());
		markUnindexable.setEnabled(someSelection && !immutablesInSelection
				&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection) && folder.isWrite());
	}

	private void applyPasswordSecurity(final GUIFolder folder, final GUIDocument[] selection, boolean justOneSelected,
			boolean immutablesInSelection) {
		setPassword.setEnabled(
				justOneSelected && !immutablesInSelection && checkStatusInSelection(Constants.DOC_UNLOCKED, selection)
						&& folder.hasPermission(Constants.PERMISSION_PASSWORD) && !selection[0].isPasswordProtected());
		unsetPassword.setEnabled(
				justOneSelected && !immutablesInSelection && checkStatusInSelection(Constants.DOC_UNLOCKED, selection)
						&& folder.hasPermission(Constants.PERMISSION_PASSWORD) && selection[0].isPasswordProtected());
	}

	private void applySignSecurity(final GUIFolder folder, final GUIDocument[] selection, boolean someSelection,
			boolean immutablesInSelection) {
		sign.setEnabled(
				someSelection && !immutablesInSelection && checkStatusInSelection(Constants.DOC_UNLOCKED, selection)
						&& folder.hasPermission(Constants.PERMISSION_SIGN) && Feature.enabled(Feature.DIGITAL_SIGNATURE)
						&& Session.get().getUser().getCertDN() != null);
	}

	private void applyIndexSecurity(final GUIDocument[] selection, boolean someSelection,
			boolean immutablesInSelection) {
		index.setEnabled(someSelection && !immutablesInSelection
				&& (checkIndexedStatusInSelection(Constants.INDEX_INDEXED, selection)
						|| checkIndexedStatusInSelection(Constants.INDEX_TO_INDEX, selection)
						|| checkIndexedStatusInSelection(Constants.INDEX_TO_INDEX_METADATA, selection))
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.INDEX));
	}

	private void applyCompareSecurity(final GUIDocument[] selection) {
		if ((selection != null && selection.length == 2) && Feature.enabled(Feature.COMPARISON)) {
			String fileName1 = selection[0].getFileName().toLowerCase();
			String fileName2 = selection[1].getFileName().toLowerCase();
			compare.setEnabled(Util.getExtension(fileName1).equalsIgnoreCase(Util.getExtension(fileName2)));
		} else
			compare.setEnabled(false);
	}

	private void addAutomationItem(MenuItem automation, Menu moreMenu) {
		if (Feature.visible(Feature.AUTOMATION))
			moreMenu.addItem(automation);
	}

	private void addWorkflowItem(MenuItem startWorkflow, Menu moreMenu) {
		if (Feature.visible(Feature.WORKFLOW))
			moreMenu.addItem(startWorkflow);
	}

	private void addReadingRequestItem(MenuItem readingRequest, Menu moreMenu) {
		if (Feature.visible(Feature.READING_CONFIRMATION))
			moreMenu.addItem(readingRequest);
	}

	private void addSendToArchiveItem(MenuItem sendToExpArchive, Menu moreMenu) {
		if (Feature.visible(Feature.IMPEX))
			moreMenu.addItem(sendToExpArchive);
	}

	private void addSplitItem(MenuItem split, Menu moreMenu) {
		if (Feature.visible(Feature.SPLIT))
			moreMenu.addItem(split);
	}

	private void addStampItem(MenuItem stamp, Menu moreMenu) {
		if (Feature.visible(Feature.STAMP))
			moreMenu.addItem(stamp);
	}

	private void addSignItem(MenuItem sign, Menu moreMenu) {
		if (Feature.visible(Feature.DIGITAL_SIGNATURE))
			moreMenu.addItem(sign);
	}

	private void addCompareItem(MenuItem compare, Menu moreMenu) {
		if (Feature.visible(Feature.COMPARISON))
			moreMenu.addItem(compare);
	}

	private void addConversionItem(MenuItem convert, Menu moreMenu) {
		if (Feature.visible(Feature.FORMAT_CONVERSION))
			moreMenu.addItem(convert);
	}

	private void addCustomActionsItem(MenuItem customActionsItem) {
		if (Feature.enabled(Feature.CUSTOM_ACTIONS)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.CUSTOM_ACTIONS)
				&& Session.get().getUser().getCustomActions() != null
				&& Session.get().getUser().getCustomActions().length > 0)
			addItem(customActionsItem);
	}

	private void addArchiveItem(MenuItem archive) {
		if (Feature.visible(Feature.ARCHIVING))
			addItem(archive);
	}

	private void removeOfficeItem(MenuItem office) {
		if (!Feature.visible(Feature.OFFICE))
			removeItem(office);
	}

	private MenuItem prepareMergeItem(final GUIFolder folder, Long[] selectionIds) {
		MenuItem item = new MenuItem(I18N.message("merge"));
		item.addClickHandler(
				event -> LD.askForStringMandatory(I18N.message("merge"), I18N.message(FILENAME), null, value -> {
					LD.contactingServer();
					DocumentService.Instance.get().merge(selectionIds, folder.getId(), value,
							new AsyncCallback<GUIDocument>() {

								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
									LD.clearPrompt();
								}

								@Override
								public void onSuccess(GUIDocument mergedDoc) {
									LD.clearPrompt();
									DocumentController.get().stored(mergedDoc);
								}
							});
				}));
		return item;
	}

	private MenuItem prepareReplaceAlias() {
		MenuItem item = new MenuItem(I18N.message("replacealias"));
		item.addClickHandler(
				event -> LD.ask(I18N.message("replacealias"), I18N.message("replacealiasquestion"), value -> {
					if (Boolean.TRUE.equals(value)) {
						GUIDocument alias = grid.getSelectedDocument();
						DocumentService.Instance.get().replaceAlias(alias.getId(), new AsyncCallback<GUIDocument>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIDocument newDoc) {
								DocumentController.get().deleted(new GUIDocument[] { alias });
								DocumentController.get().stored(newDoc);
							}
						});
					}
				}));
		return item;
	}

	private MenuItem prepareCompareItem(final GUIDocument[] selection) {
		MenuItem item = new MenuItem(I18N.message("compare"));
		item.addClickHandler(event -> {
			GUIVersion version1 = new GUIVersion();
			version1.setDocId(selection[0].getId());
			version1.setFileVersion(selection[0].getFileVersion());
			version1.setFileName(selection[0].getFileName());

			GUIVersion version2 = new GUIVersion();
			version2.setDocId(selection[1].getId());
			version2.setFileVersion(selection[1].getFileVersion());
			version2.setFileName(selection[1].getFileName());

			ComparisonWindow diff = new ComparisonWindow(version1, version2);
			diff.show();
		});
		return item;
	}

	private MenuItem preparePreview() {
		MenuItem item = new MenuItem();
		item.setTitle(I18N.message("preview"));
		item.addClickHandler(event -> {
			PreviewPopup iv = null;

			if (grid.getSelectedCount() == 1) {
				GUIDocument doc = grid.getSelectedDocument();
				if (doc.getDocRef() != null) {
					/*
					 * in case of alias the data servlet inverts the docId and
					 * the docRef so in order to have the preview to do the
					 * right security checks we have to restore the correct ids
					 */
					long aliasId = doc.getDocRef();
					doc.setDocRef(doc.getId());
					doc.setId(aliasId);
				}
				iv = new PreviewPopup(doc);
			} else {
				GUIDocument[] docs = grid.getSelectedDocuments();
				for (GUIDocument doc : docs) {
					/*
					 * in case of alias the data servlet inverts the docId and
					 * the docRef so in order to have the preview to do the
					 * right security checks we have to restore the correct ids
					 */
					Long aliasId = doc.getDocRef();
					if (aliasId != null) {
						doc.setDocRef(doc.getId());
						doc.setId(aliasId);
					}
				}
				iv = new PreviewPopup(docs, 0);
			}
			iv.show();
		});
		return item;
	}

	private MenuItem prepareIndexItem(final GUIDocument[] selection, Long[] selectionIds) {
		MenuItem item = new MenuItem();
		item.setTitle(I18N.message("index"));
		item.addClickHandler(event -> {
			LD.contactingServer();

			DocumentService.Instance.get().indexDocuments(selectionIds, new AsyncCallback<Void>() {
				@Override
				public void onFailure(Throwable caught) {
					LD.clearPrompt();
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void result) {
					LD.clearPrompt();
					for (GUIDocument doc : selection) {
						doc.setIndexed(Constants.INDEX_INDEXED);
						if (DocumentController.get().getCurrentDocument() != null
								&& DocumentController.get().getCurrentDocument().getId() == doc.getId()) {
							DocumentController.get().getCurrentDocument().setIndexed(Constants.INDEX_INDEXED);
							DocumentController.get().modified(DocumentController.get().getCurrentDocument());
						} else {
							DocumentController.get().modified(doc);
						}
					}
				}
			});
		});
		return item;
	}

	private MenuItem markIndexableMetadataOnlyItem(GUIDocument[] selection, Long[] selectionIds) {
		MenuItem item = new MenuItem();
		item.setTitle(I18N.message("markindexablemetadataonly"));
		item.addClickHandler(event -> {
			if (selection == null || selection.length == 0)
				return;

			DocumentService.Instance.get().markIndexable(selectionIds, Constants.INDEX_TO_INDEX_METADATA,
					new AsyncCallback<Void>() {
						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Void result) {
							for (GUIDocument doc : selection) {
								doc.setIndexed(Constants.INDEX_TO_INDEX_METADATA);
								if (DocumentController.get().getCurrentDocument() != null
										&& DocumentController.get().getCurrentDocument().getId() == doc.getId()) {
									DocumentController.get().getCurrentDocument()
											.setIndexed(Constants.INDEX_TO_INDEX_METADATA);
									DocumentController.get().modified(DocumentController.get().getCurrentDocument());
								} else {
									DocumentController.get().modified(doc);
								}
							}
						}
					});
		});
		return item;
	}

	private MenuItem prepareMarkIndexableItem(GUIDocument[] selection, Long[] selectionIds) {
		MenuItem item = new MenuItem();
		item.setTitle(I18N.message("markindexable"));
		item.addClickHandler(event -> {
			if (selection == null || selection.length == 0)
				return;

			DocumentService.Instance.get().markIndexable(selectionIds, Constants.INDEX_TO_INDEX,
					new AsyncCallback<Void>() {
						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Void result) {
							for (GUIDocument doc : selection) {
								doc.setIndexed(Constants.INDEX_TO_INDEX);
								if (DocumentController.get().getCurrentDocument() != null
										&& DocumentController.get().getCurrentDocument().getId() == doc.getId()) {
									DocumentController.get().getCurrentDocument().setIndexed(Constants.INDEX_TO_INDEX);
									DocumentController.get().modified(DocumentController.get().getCurrentDocument());
								} else {
									DocumentController.get().modified(doc);
								}
							}
						}
					});
		});
		return item;
	}

	private MenuItem prepareMarkUnindexableItem(final GUIDocument[] selection, Long[] selectionIds) {
		MenuItem item = new MenuItem();
		item.setTitle(I18N.message("markunindexable"));
		item.addClickHandler(event -> {
			if (selection == null || selection.length == 0)
				return;

			DocumentService.Instance.get().markUnindexable(selectionIds, new AsyncCallback<Void>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void result) {
					for (GUIDocument doc : selection) {
						doc.setIndexed(Constants.INDEX_SKIP);
						if (DocumentController.get().getCurrentDocument() != null
								&& DocumentController.get().getCurrentDocument().getId() == doc.getId()) {
							DocumentController.get().getCurrentDocument().setIndexed(Constants.INDEX_SKIP);
							DocumentController.get().modified(DocumentController.get().getCurrentDocument());
						} else {
							DocumentController.get().modified(doc);
						}
					}
				}
			});
		});
		return item;
	}

	private MenuItem prepareBookmarkItem(final GUIDocument[] selection, Long[] selectionIds) {
		MenuItem item = new MenuItem();
		item.setTitle(I18N.message("addbookmark"));
		item.addClickHandler(event -> {
			if (selection == null || selection.length == 0)
				return;
			DocumentService.Instance.get().addBookmarks(selectionIds, 0, new AsyncCallback<Void>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void result) {
					GUIDocument[] selection = grid.getSelectedDocuments();
					for (GUIDocument doc : selection) {
						doc.setBookmarked(true);
						if (DocumentController.get().getCurrentDocument() != null
								&& DocumentController.get().getCurrentDocument().getId() == doc.getId()) {
							DocumentController.get().getCurrentDocument().setBookmarked(true);
							DocumentController.get().modified(DocumentController.get().getCurrentDocument());
						} else {
							DocumentController.get().modified(doc);
						}
					}
					DocumentsPanel.get().getDocumentsMenu().refresh("bookmarks");
				}
			});
		});
		return item;
	}

	private MenuItem prepareArchiveItem(Long[] selectionIds) {
		MenuItem item = new MenuItem();
		item.setTitle(I18N.message("archive"));
		item.addClickHandler(
				event -> LD.askForValue(I18N.message("warning"), I18N.message("archiveadvice"), "", 600, value -> {
					if (value == null)
						return;

					if (value.isEmpty())
						SC.warn(I18N.message("commentrequired"));
					else
						DocumentService.Instance.get().archiveDocuments(selectionIds, value, new AsyncCallback<Void>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void result) {
								grid.removeSelectedDocuments();
								GuiLog.info(I18N.message("documentswerearchived", "" + selectionIds.length), null);
							}
						});
				}));
		return item;
	}

	private MenuItem prepareCheckinItem(final GUIDocument[] selection) {
		MenuItem item = new MenuItem();
		item.setTitle(I18N.message("checkin"));
		item.addClickHandler(event -> {
			if (selection == null || selection.length == 0)
				return;

			long id = selection[0].getId();
			final String filename = selection[0].getFileName();

			// Just to clean the upload folder
			DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<Void>() {

				@Override
				public void onFailure(Throwable caught) {
					// Nothing to do
				}

				@Override
				public void onSuccess(Void result) {
					DocumentService.Instance.get().getById(id, new AsyncCallback<GUIDocument>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(GUIDocument document) {
							new DocumentCheckin(document, filename).show();
						}
					});
				}
			});
		});
		return item;
	}

	private MenuItem prepareCheckoutItem(GUIFolder folder, GUIDocument[] selection, Long[] selectionIds) {
		MenuItem item = new MenuItem();
		item.setTitle(I18N.message("checkout"));
		item.addClickHandler(event -> DocumentService.Instance.get().checkout(selectionIds, new AsyncCallback<Void>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void result) {
				GuiLog.info(I18N.message("documentcheckedout"), null);
				GUIDocument[] docs = grid.getSelectedDocuments();
				for (GUIDocument doc : docs)
					DocUtil.markCheckedOut(doc);
				grid.selectDocument(selectionIds[0]);

				Timer timer = new Timer() {
					public void run() {
						onDownload(folder, selection);
					}
				};
				timer.schedule(100);
			}
		}));
		return item;
	}

	private MenuItem prepareUnlockItem(final GUIDocument[] selection, Long[] selectionIds) {
		MenuItem item = new MenuItem();
		item.setTitle(I18N.message("unlock"));
		item.addClickHandler(event -> {
			if (selection == null)
				return;

			DocumentService.Instance.get().unlock(selectionIds, new AsyncCallback<Void>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void result) {
					GUIDocument[] docs = grid.getSelectedDocuments();
					for (GUIDocument doc : docs)
						DocUtil.markUnlocked(doc);
					grid.selectDocument(selectionIds[0]);
				}
			});
		});
		return item;
	}

	private MenuItem prepareLockItem(final GUIDocument[] selection, final Long[] selectionIds) {
		MenuItem item = new MenuItem();
		item.setTitle(I18N.message("lock"));
		item.addClickHandler(event -> LD.askForValue(I18N.message("info"), I18N.message("lockadvice"), "", value -> {
			if (value != null)
				DocumentService.Instance.get().lock(selectionIds, value, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						for (GUIDocument doc : selection)
							DocUtil.markLocked(doc);
						grid.selectDocument(selectionIds[0]);
					}
				});
		}));
		return item;
	}

	private MenuItem prepareUnsetPasswordItem(final GUIDocument[] selection) {
		MenuItem item = new MenuItem();
		item.setTitle(I18N.message("unsetpassword"));
		item.addClickHandler(event -> {
			if (Session.get().isAdmin()) {
				DocumentService.Instance.get().unsetPassword(selection[0].getId(), "", new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						selection[0].setPasswordProtected(false);
						grid.updateDocument(selection[0]);
					}
				});
			} else
				LD.askForValue(I18N.message("unsetpassword"), I18N.message("currentpassword"), "", 300, value -> {
					if (value == null)
						return;

					if (value.isEmpty())
						SC.warn(I18N.message("passwordrequired"));
					else
						DocumentService.Instance.get().unsetPassword(selection[0].getId(), value,
								new AsyncCallback<Void>() {
									@Override
									public void onFailure(Throwable caught) {
										GuiLog.serverError(caught);
									}

									@Override
									public void onSuccess(Void result) {
										selection[0].setPasswordProtected(false);
										grid.updateDocument(selection[0]);
									}
								});
				});
		});
		return item;
	}

	private MenuItem prepareSetPasswordItem(final GUIDocument[] selection) {
		MenuItem item = new MenuItem();
		item.setTitle(I18N.message("setpassword"));
		item.addClickHandler(event -> {
			TextItem password = ItemFactory.newPasswordItem("password", I18N.message("setpasswordwarning"), null);
			password.setAutoComplete(AutoComplete.NONE);
			LD.askForValue(I18N.message("setpassword"), I18N.message("setpasswordwarning"), "", password, 520,
					value -> {
						if (value == null)
							return;

						if (value.isEmpty())
							SC.warn(I18N.message("passwordrequired"));
						else
							DocumentService.Instance.get().setPassword(selection[0].getId(), value,
									new AsyncCallback<Void>() {
										@Override
										public void onFailure(Throwable caught) {
											GuiLog.serverError(caught);
										}

										@Override
										public void onSuccess(Void result) {
											selection[0].setPasswordProtected(true);
											grid.updateDocument(selection[0]);
											GuiLog.info(I18N.message("passwordapplied"), null);
										}
									});
					});
		});
		return item;
	}

	private MenuItem prepareImmutableItem(final GUIDocument[] selection, Long[] selectionIds) {
		MenuItem item = new MenuItem();
		item.setTitle(I18N.message("makeimmutable"));
		item.addClickHandler(
				event -> LD.askForValue(I18N.message("warning"), I18N.message("immutableadvice"), "", 600, value -> {
					if (value == null)
						return;

					if (value.isEmpty())
						SC.warn(I18N.message("commentrequired"));
					else
						DocumentService.Instance.get().makeImmutable(selectionIds, value, new AsyncCallback<Void>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void result) {
								for (GUIDocument doc : selection) {
									doc.setImmutable(1);
									grid.updateDocument(doc);
								}

								grid.selectDocument(selection[0].getId());
							}
						});
				}));
		return item;
	}

	private MenuItem prepareLinksItem(GUIDocument[] selection, Long[] selectionIds) {
		MenuItem item = new MenuItem();
		item.setTitle(I18N.message("pasteaslinks"));
		item.addClickHandler(event -> {
			if (selection == null || selection.length == 0 || Clipboard.getInstance().isEmpty())
				return;

			final Long[] inIds = new Long[Clipboard.getInstance().size()];
			int i = 0;
			for (GUIDocument doc : Clipboard.getInstance())
				inIds[i++] = doc.getId();

			DocumentService.Instance.get().linkDocuments(inIds, selectionIds, new AsyncCallback<Void>() {

				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void result) {
					for (GUIDocument doc : selection) {
						doc.setLinks(doc.getLinks() + 1);
						DocumentController.get().modified(doc);
					}

					for (GUIDocument doc : Clipboard.getInstance()) {
						doc.setLinks(doc.getLinks() + 1);
						DocumentController.get().modified(doc);
					}

					Clipboard.getInstance().clear();

					/**
					 * For some reason if the link target is already shown in
					 * the details panel it must be reloaded or further inputs
					 * will be lost.
					 */
					DocumentService.Instance.get().getById(grid.getSelectedDocument().getId(),
							new AsyncCallback<GUIDocument>() {
								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(GUIDocument doc) {
									DocumentController.get().setCurrentDocument(doc);
								}
							});
				}
			});
		});
		return item;
	}

	private MenuItem prepareDeleteItem(final GUIDocument[] selection) {
		MenuItem item = new MenuItem();
		item.setTitle(I18N.message("ddelete"));
		item.addClickHandler(event -> {
			final Long[] ids = new Long[selection.length];
			for (int i = 0; i < selection.length; i++)
				ids[i] = selection[i].getId();

			LD.ask(I18N.message("question"), I18N.message("confirmdelete"), value -> {
				if (Boolean.TRUE.equals(value)) {
					DocumentService.Instance.get().delete(ids, new AsyncCallback<Void>() {
						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Void result) {
							// If the data grid is big the records
							// deletion doesn't work, so refresh the
							// screen.
							if (grid.getFolder() != null
									&& grid.getFolder().getId() == FolderController.get().getCurrentFolder().getId())
								DocumentsPanel.get().refresh();
							DocumentController.get().deleted(grid.getSelectedDocuments());
						}
					});
				}
			});
		});
		return item;
	}

	private MenuItem prepareRenameItem(final GUIDocument[] selection) {
		MenuItem item = new MenuItem();
		item.setTitle(I18N.message("rename"));
		item.addClickHandler(event -> {
			final String originalExtension = Util.getExtension(selection[0].getFileName());
			LD.askForValue(I18N.message("rename"), I18N.message(FILENAME), selection[0].getFileName(), val -> {
				if (val == null || "".equals(val.trim()))
					return;
				final String newFilename = val.trim().replace("/", "").replace("\\\\", "");

				// Check if the user has changed the extension
				// and warn him
				if (!originalExtension.equalsIgnoreCase(Util.getExtension(newFilename))) {
					LD.ask(I18N.message(FILENAME), I18N.message("extchangewarn"), response -> {
						if (Boolean.TRUE.equals(response))
							onRename(selection[0].getId(), newFilename);
					});
				} else {
					onRename(selection[0].getId(), newFilename);
				}

			});
		});
		return item;
	}

	private MenuItem prepareCopyItem(final GUIDocument[] selection) {
		MenuItem item = new MenuItem();
		item.setTitle(I18N.message("copy"));
		item.addClickHandler(event -> {
			if (selection == null)
				return;
			Clipboard.getInstance().clear();
			for (int i = 0; i < selection.length; i++) {
				Clipboard.getInstance().add(selection[i]);
				Clipboard.getInstance().setLastAction(Clipboard.COPY);
			}
		});
		return item;
	}

	private MenuItem prepareCutItem(final GUIDocument[] selection) {
		MenuItem item = new MenuItem();
		item.setTitle(I18N.message("cut"));
		item.addClickHandler(event -> {
			if (selection == null)
				return;
			Clipboard.getInstance().clear();
			for (int i = 0; i < selection.length; i++) {
				long id = selection[i].getId();

				GUIDocument document = new GUIDocument();
				document.setId(id);
				document.setIcon(selection[i].getIcon());
				document.setLastModified(selection[i].getLastModified());
				document.setDate(selection[i].getDate());
				document.setVersion(selection[i].getVersion());
				document.setFileVersion(selection[i].getFileVersion());
				document.setFileName(selection[i].getFileName());

				Clipboard.getInstance().add(document);
				Clipboard.getInstance().setLastAction(Clipboard.CUT);
			}
		});
		return item;
	}

	private MenuItem prepareCustomActionsItem(long folderId, Long[] selectedDocIds) {
		MenuItem item = new MenuItem(I18N.message("customactions"));
		Menu menu = new Menu();
		item.setSubmenu(menu);

		if (Session.get().getUser().getCustomActions() == null || Session.get().getUser().getCustomActions().length < 1)
			return item;

		for (GUIMenu menuAction : Session.get().getUser().getCustomActions()) {
			MenuItem actionItem = new MenuItem(I18N.message(menuAction.getName()));
			menu.addItem(actionItem);

			/**
			 * Check on the server if the action has been modified
			 */
			actionItem.addClickHandler(event -> onClickCustomAction(folderId, selectedDocIds, menuAction));
		}

		return item;
	}

	private void onClickCustomAction(long folderId, Long[] selectedDocIds, GUIMenu menuAction) {
		SecurityService.Instance.get().getMenu(menuAction.getId(), I18N.getLocale(), new AsyncCallback<GUIMenu>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIMenu action) {
				Session.get().getUser().updateCustomAction(action);

				if ((action.getRoutineId() == null || action.getRoutineId().longValue() == 0L)
						&& action.getAutomation() != null && !action.getAutomation().trim().isEmpty()) {
					/*
					 * An automation script is specified directly, so launch
					 * it's execution
					 */
					GUIAutomationRoutine routine = new GUIAutomationRoutine();
					routine.setAutomation(action.getAutomation());
					executeRoutine(folderId, selectedDocIds, routine);
				} else if (action.getRoutineId() != null && action.getRoutineId().longValue() != 0L) {
					AutomationService.Instance.get().getRoutine(action.getRoutineId(),
							new AsyncCallback<GUIAutomationRoutine>() {

								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(GUIAutomationRoutine routine) {
									if (routine.getTemplateId() != null && routine.getTemplateId().longValue() != 0L) {
										/*
										 * A routine with parameters is
										 * referenced, so open the input popup
										 */
										FillRoutineParams dialog = new FillRoutineParams(action.getName(), routine,
												folderId, selectedDocIds);
										dialog.show();
									} else {
										/*
										 * A routine without parameters is
										 * referenced, so launch directly
										 */
										executeRoutine(folderId, selectedDocIds, routine);
									}
								}
							});
				}
			}
		});
	}

	private boolean checkStatusInSelection(int status, GUIDocument[] selection) {
		for (GUIDocument doc : selection) {
			if (doc.getStatus() != status || (status == Constants.DOC_CHECKED_OUT
					&& doc.getLockUserId() != Session.get().getUser().getId() && !Session.get().isAdmin()))
				return false;
		}
		return true;
	}

	private boolean checkIndexedStatusInSelection(int status, GUIDocument[] selection) {
		for (GUIDocument doc : selection) {
			if (doc.getIndexed() != status)
				return false;
		}
		return true;
	}

	private boolean checkImmutablesInSelection(GUIDocument[] selection) {
		for (GUIDocument doc : selection)
			if (doc.getImmutable() != 0)
				return true;
		return false;
	}

	private void onDownload(final GUIFolder folder, final GUIDocument[] selection) {
		if (selection.length == 1) {
			long id = selection[0].getId();
			DocUtil.download(id, null);
		} else {
			StringBuilder url = new StringBuilder(GWT.getHostPageBaseURL());
			url.append("zip-export?folderId=");
			url.append(folder.getId());
			for (GUIDocument doc : selection) {
				if (doc.isPasswordProtected()) {
					SC.warn(I18N.message("somedocsprotected"));
					break;
				}
				url.append("&docId=");
				url.append(doc.getId());
			}
			Util.download(url.toString());
		}
	}

	private void executeRoutine(long folderId, Long[] docIds, GUIAutomationRoutine routine) {
		AutomationService.Instance.get().execute(routine, docIds, folderId, new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg0) {
				// Nothing to do
			}
		});
	}

	private void onRename(long docId, String newFilename) {
		DocumentService.Instance.get().rename(docId, newFilename, new AsyncCallback<GUIDocument>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIDocument doc) {
				DocumentController.get().modified(doc);
			}
		});
	}
}