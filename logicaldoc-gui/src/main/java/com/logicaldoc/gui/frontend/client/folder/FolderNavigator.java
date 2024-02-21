package com.logicaldoc.gui.frontend.client.folder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.google.gwt.http.client.RequestTimeoutException;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIAutomationRoutine;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIMenu;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.controllers.FolderObserver;
import com.logicaldoc.gui.common.client.data.FoldersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.RequestInfo;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.ValuesCallback;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.common.client.widgets.grid.FolderListGridField;
import com.logicaldoc.gui.frontend.client.clipboard.Clipboard;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.document.SendToArchiveDialog;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsGrid;
import com.logicaldoc.gui.frontend.client.document.grid.FillRoutineParams;
import com.logicaldoc.gui.frontend.client.folder.browser.FolderCursor;
import com.logicaldoc.gui.frontend.client.folder.browser.FolderTree;
import com.logicaldoc.gui.frontend.client.folder.copy.FolderCopyDialog;
import com.logicaldoc.gui.frontend.client.panels.MainPanel;
import com.logicaldoc.gui.frontend.client.search.Search;
import com.logicaldoc.gui.frontend.client.services.AutomationService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.logicaldoc.gui.frontend.client.subscription.SubscriptionDialog;
import com.smartgwt.client.util.EventHandler;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.tree.TreeNode;

/**
 * The panel that shows the workspaces/folders navigation tree
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class FolderNavigator extends FolderTree implements FolderObserver {

	private static final String POSITION = "position";

	private static final String COLOR = "color";

	private static final String FOLD_REF = "foldRef";

	private static final String OPERATIONNOTALLOWED = "operationnotallowed";

	private static FolderNavigator instance = new FolderNavigator();

	private boolean firstTime = true;

	// Indicates if the Navigator is in the process of opening this path
	GUIFolder[] pathToOpen = null;

	int currentIndexInPathToOpen = 0;

	public FolderNavigator() {
		super(FolderCursor.get());
		setWidth100();
		setBorder("0px");
		setBodyStyleName("normal");
		setShowHeader(false);
		setLeaveScrollbarGap(false);
		setCanReorderRecords(false);
		setCanDragRecordsOut(false);
		setAutoFetchData(true);
		setLoadDataOnDemand(true);
		setDataSource(FoldersDS.get());
		setCanSelectAll(false);
		setShowConnectors(true);
		setShowRoot(false);
		setCanAcceptDrop(true);
		setCanAcceptDroppedRecords(true);
		setSaveLocally(true);
		setIconSize(1);

		ListGridField name = new FolderListGridField();
		setFields(name);

		FolderController.get().addObserver(this);

		addDragStartHandler(event -> {
			// Workspaces cannot be moved
			if (EventHandler.getDragTarget() instanceof FolderNavigator
					&& "1".equals(getDragData()[0].getAttributeAsString("type"))) {
				event.cancel();
				return;
			}
		});

		addDropHandler();

		addCellContextHandler();

		addCellClickHandler();

		addFolderOpenedHandler();

		/*
		 * To refresh the folder's decoration
		 */
		addFolderClosedHandler(event -> {
			event.getNode().setAttribute(OPENED, false);
			updateData(event.getNode());
		});

		// Used to expand root folder after login or to open in folder
		addDataArrivedHandler((DataArrivedEvent dataArrivedEvent) -> FolderNavigator.this.handleDataArrived());

		cursor.registerMaxChangedHandler(event -> reloadChildren());

		cursor.registerPageChangedHandler(event -> reloadChildren());

		/*
		 * A listener of the keypress to automatically collect a string typed by
		 * the user to quickly select the folder
		 */
		addKeyDownHandler(keyDown -> {
			String key = EventHandler.getKey();
			if ("space".equalsIgnoreCase(key)) {
				selectFolder(getSelectedFolderId());
				typed = null;
				lastTyped = null;
			} else {
				if (typed == null) {
					typed = key;
				} else {
					typed += key;
				}
				lastTyped = new Date();
			}
		});

		installTimer();
	}

	private void addDropHandler() {
		addDropHandler(dropEvent -> {
			if (EventHandler.getDragTarget() instanceof FolderNavigator) {
				// Workspaces cannot be moved
				if ("1".equals(getDragData()[0].getAttributeAsString("type"))) {
					dropEvent.cancel();
					return;
				}

				doMoveFolderOnDrop();
			} else if (EventHandler.getDragTarget() instanceof DocumentsGrid) {
				/*
				 * In this case we are moving documents
				 */
				DocumentsGrid grid = (DocumentsGrid) EventHandler.getDragTarget();
				final List<GUIDocument> selection = grid.getSelectedDocuments();
				if (selection.isEmpty())
					return;

				final TreeNode selectedNode = getDropFolder();
				final long folderId = Long.parseLong(selectedNode.getAttribute(FOLDER_ID));

				if (FolderController.get().getCurrentFolder().getId() == folderId)
					return;

				doMoveDocumentOnDrop(selection, selectedNode, folderId);
			}
		});
	}

	/**
	 * Handles the body click on folder name to create the context menu
	 */
	private void addCellContextHandler() {
		addCellContextClickHandler(contextClickEvent -> {
			if (getSelectedRecord() != null && getSelectedRecords().length > 1) {
				Menu contextMenu = prepateContextMenu();
				contextMenu.showContextMenu();
			} else {
				FolderService.Instance.get().getFolder(getSelectedRecord().getAttributeAsLong(FOLDER_ID), false, true,
						false, new AsyncCallback<>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIFolder folder) {
								Menu contextMenu = prepateContextMenu(folder);
								contextMenu.showContextMenu();
							}
						});
			}
			if (contextClickEvent != null)
				contextClickEvent.cancel();
		});
	}

	private void doMoveDocumentOnDrop(final List<GUIDocument> selection, final TreeNode selectedNode,
			final long folderId) {
		final String sourceName = selection.size() == 1 ? selection.get(0).getFileName()
				: (selection.size() + " " + I18N.message("documents").toLowerCase());
		final String targetName = selectedNode.getAttributeAsString("name");

		LD.ask(I18N.message("move"), I18N.message("moveask", sourceName, targetName), yes -> {
			if (Boolean.TRUE.equals(yes)) {
				FolderService.Instance.get().paste(selection.stream().map(d -> d.getId()).collect(Collectors.toList()),
						folderId, "cut", false, false, false, new AsyncCallback<>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
								GuiLog.warn(I18N.message(OPERATIONNOTALLOWED), null);
							}

							@Override
							public void onSuccess(Void result) {
								DocumentsPanel.get().onFolderSelected(FolderController.get().getCurrentFolder());
								GuiLog.debug("Drag&Drop operation completed.");
							}
						});
			}

			TreeNode node = getTree().find(FOLDER_ID, (Object) Long.valueOf(folderId));
			if (node != null)
				getTree().reloadChildren(node);
		});
	}

	private void doMoveFolderOnDrop() {
		final List<Long> source = getSelectedIds();
		final long target = Long.parseLong(getDropFolder().getAttributeAsString(FOLDER_ID));

		final String sourceName = getDragData()[0].getAttributeAsString("name");
		final String targetName = getDropFolder().getAttributeAsString("name");

		LD.ask(I18N.message("move"), I18N.message("moveask", sourceName, targetName), yes -> {
			if (Boolean.TRUE.equals(yes)) {
				for (Long id : source) {
					TreeNode node = getTree().find(FOLDER_ID, (Object) id);
					getTree().remove(node);
				}

				FolderService.Instance.get().move(source, target, new AsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
						GuiLog.warn(I18N.message(OPERATIONNOTALLOWED), null);
					}

					@Override
					public void onSuccess(Void ret) {
						TreeNode targetNode = getTree().find(FOLDER_ID, (Object) Long.valueOf(target));
						if (targetNode != null)
							getTree().reloadChildren(targetNode);
					}
				});
			}
		});
	}

	/**
	 * Select the specified folder
	 * 
	 * @param folderId the folder's identifier
	 */
	@Override
	public void selectFolder(final long folderId) {
		FolderService.Instance.get().getFolder(folderId, false, true, Session.get().isFolderPagination(),
				new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIFolder result) {
						if (result != null) {
							result.setPathExtended(getPath(folderId));
							FolderController.get().selected(result);
						}
					}
				});
	}

	/**
	 * Prepares the context menu for multiple selection
	 * 
	 * @return the prepared context menu
	 */
	private Menu prepateContextMenu() {
		MenuItem move = new MenuItem();
		move.setTitle(I18N.message("move"));
		move.addClickHandler(event -> {
			MoveDialog dialog = new MoveDialog();
			dialog.show();
		});

		MenuItem copy = new MenuItem();
		copy.setTitle(I18N.message("copy"));
		copy.addClickHandler(event -> new FolderCopyDialog().show());

		MenuItem merge = new MenuItem();
		merge.setTitle(I18N.message("merge"));
		merge.addClickHandler(event -> new MergeDialog().show());

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> onDelete());

		Menu contextMenu = new Menu();
		contextMenu.setItems(move, copy, merge, delete);

		return contextMenu;
	}

	/**
	 * Prepares the context menu for single selection
	 * 
	 * @param selectedFolder the folder to use
	 */
	private Menu prepateContextMenu(final GUIFolder selectedFolder) {

		Menu contextMenu = new Menu();

		MenuItem search = prepareSearchMenuItem(selectedFolder);

		MenuItem delete = prepareDeleteMenuItem(selectedFolder);

		MenuItem create = prepareCreateMenuItem(selectedFolder);

		MenuItem createAlias = prepareCreateAliasMenuItem();

		MenuItem rename = prepareRenameMenuItem(selectedFolder);

		MenuItem createWorkspace = new MenuItem();
		createWorkspace.setTitle(I18N.message("newworkspace"));
		createWorkspace.addClickHandler(click -> onCreateWorkspace());

		MenuItem reload = new MenuItem();
		reload.setTitle(I18N.message("reload"));
		reload.addClickHandler(click -> reload());

		MenuItem move = prepareMoveMenuItem(selectedFolder);

		MenuItem copy = prepareCopyMenuItem();

		MenuItem merge = prepareMergeMenuItem(selectedFolder);

		MenuItem paste = new MenuItem();
		paste.setTitle(I18N.message("paste"));
		paste.addClickHandler(click -> onPaste());

		MenuItem pasteAsAlias = new MenuItem();
		pasteAsAlias.setTitle(I18N.message("pasteasalias"));
		pasteAsAlias.addClickHandler(click -> onPasteAsAlias());

		MenuItem exportZip = prepareExportZipMenuItem(selectedFolder);

		MenuItem addBookmark = new MenuItem();
		addBookmark.setTitle(I18N.message("addbookmark"));
		addBookmark.addClickHandler(click -> onAddBookmark());

		if (!selectedFolder.hasPermission(GUIAccessControlEntry.PERMISSION_WRITE)
				|| Clipboard.getInstance().isEmpty()) {
			paste.setEnabled(false);
			pasteAsAlias.setEnabled(false);
		}

		if (!selectedFolder.hasPermission(GUIAccessControlEntry.PERMISSION_ADD)) {
			createAlias.setEnabled(false);
		}

		if (Clipboard.getInstance().getLastAction().equals(Clipboard.CUT))
			pasteAsAlias.setEnabled(false);

		if (Session.get().getUser().isMemberOf(Constants.GROUP_ADMIN) && selectedFolder.isWorkspace()
				&& Feature.visible(Feature.MULTI_WORKSPACE)) {
			delete.setEnabled(!selectedFolder.isDefaultWorkspace());
			move.setEnabled(false);
			merge.setEnabled(false);
			rename.setEnabled(!selectedFolder.isDefaultWorkspace());
			createWorkspace.setEnabled(Feature.enabled(Feature.MULTI_WORKSPACE));
			contextMenu.setItems(reload, search, create, createAlias, rename, createWorkspace, delete, addBookmark,
					paste, pasteAsAlias, move, copy, merge, exportZip);
		} else {
			contextMenu.setItems(reload, search, create, createAlias, rename, delete, addBookmark, paste, pasteAsAlias,
					move, copy, merge, exportZip);
		}

		addSubscribeMenuItem(selectedFolder, contextMenu);

		addApplyTemplateMenuItem(selectedFolder, contextMenu);

		addArchiveMenuItem(selectedFolder, contextMenu);

		addSendToExportArchiveMenuItem(selectedFolder, contextMenu);

		addAutomationMenuItem(selectedFolder, contextMenu);

		addCustomActionsMenuItem(selectedFolder, contextMenu);

		return contextMenu;
	}

	private void addCustomActionsMenuItem(final GUIFolder selectedFolder, Menu contextMenu) {
		if (Feature.enabled(Feature.CUSTOM_ACTIONS)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.CUSTOM_ACTIONS)
				&& !Session.get().getUser().getCustomActions().isEmpty()) {
			MenuItem customActionsItem = prepareCustomActionsMenu(selectedFolder.getId());
			contextMenu.addItem(customActionsItem);
		}
	}

	private void addAutomationMenuItem(final GUIFolder selectedFolder, Menu contextMenu) {
		if (Feature.visible(Feature.AUTOMATION)) {
			MenuItem automation = new MenuItem();
			automation.setTitle(I18N.message("executeautomation"));
			automation.addClickHandler(eaClick -> onAutomation(selectedFolder.getId()));

			contextMenu.addItem(automation);
			if (!Feature.enabled(Feature.AUTOMATION)
					|| !selectedFolder.hasPermission(GUIAccessControlEntry.PERMISSION_AUTOMATION)) {
				automation.setEnabled(false);
			}
		}
	}

	private void addSendToExportArchiveMenuItem(final GUIFolder folder, Menu contextMenu) {
		if (Feature.visible(Feature.IMPEX)) {
			MenuItem sendToExpArchive = new MenuItem();
			sendToExpArchive.setTitle(I18N.message("sendtoexparchive"));
			sendToExpArchive.addClickHandler(
					event -> LD.ask(I18N.message("question"), I18N.message("confirmputinexparchive"), yes -> {
						if (Boolean.TRUE.equals(yes)) {
							SendToArchiveDialog archiveDialog = new SendToArchiveDialog(Arrays.asList(folder.getId()),
									false);
							archiveDialog.show();
						}
					}));
			contextMenu.addItem(sendToExpArchive);
			if (!Feature.enabled(Feature.IMPEX) || !folder.hasPermission(GUIAccessControlEntry.PERMISSION_EXPORT))
				sendToExpArchive.setEnabled(false);
		}
	}

	private void addArchiveMenuItem(final GUIFolder folder, Menu contextMenu) {
		if (Feature.visible(Feature.ARCHIVING)) {
			MenuItem archive = new MenuItem();
			archive.setTitle(I18N.message("archive"));
			archive.addClickHandler(archiveClick -> onArchive(folder.getId()));
			contextMenu.addItem(archive);
			if (!Feature.enabled(Feature.ARCHIVING) || !folder.hasPermission(GUIAccessControlEntry.PERMISSION_ARCHIVE))
				archive.setEnabled(false);
		}
	}

	private void addApplyTemplateMenuItem(final GUIFolder folder, Menu contextMenu) {
		if (Feature.visible(Feature.FOLDER_TEMPLATE)) {
			MenuItem applyTemplate = new MenuItem();
			applyTemplate.setTitle(I18N.message("applytemplate"));
			applyTemplate.addClickHandler(applyTemplateClick -> onApplyTemplate());
			contextMenu.addItem(applyTemplate);
			if (!Feature.enabled(Feature.FOLDER_TEMPLATE)
					|| !folder.hasPermission(GUIAccessControlEntry.PERMISSION_ADD))
				applyTemplate.setEnabled(false);
		}
	}

	private void addSubscribeMenuItem(GUIFolder folder, Menu contextMenu) {
		if (Feature.visible(Feature.AUDIT)) {
			MenuItem subscribe = new MenuItem();
			subscribe.setTitle(I18N.message("subscribe"));
			subscribe.addClickHandler(click -> new SubscriptionDialog(folder.getId(), null).show());
			subscribe.setEnabled(Feature.enabled(Feature.AUDIT));
			contextMenu.addItem(subscribe);
		}
	}

	private MenuItem prepareCopyMenuItem() {
		MenuItem copy = new MenuItem();
		copy.setTitle(I18N.message("copy"));
		copy.addClickHandler(copyClick -> new FolderCopyDialog().show());
		return copy;
	}

	private MenuItem prepareCreateAliasMenuItem() {
		MenuItem createAlias = new MenuItem();
		createAlias.setTitle(I18N.message("createalias"));
		createAlias.addClickHandler(caClick -> new CreateAliasDialog().show());
		createAlias.setEnabled(getSelectedRecord().getAttributeAsString(FOLD_REF) == null);
		return createAlias;
	}

	private MenuItem prepareExportZipMenuItem(final GUIFolder folder) {
		MenuItem exportZip = new MenuItem();
		exportZip.setTitle(I18N.message("exportzip"));
		exportZip.addClickHandler(
				event -> Window.open(Util.contextPath() + "zip-export?folderId=" + folder.getId(), "_blank", ""));
		exportZip.setEnabled(folder.hasPermission(GUIAccessControlEntry.PERMISSION_EXPORT)
				&& folder.hasPermission(GUIAccessControlEntry.PERMISSION_DOWNLOAD));
		return exportZip;
	}

	private MenuItem prepareMoveMenuItem(final GUIFolder folder) {
		MenuItem move = new MenuItem();
		move.setTitle(I18N.message("move"));
		move.addClickHandler(event -> new MoveDialog().show());
		move.setEnabled(folder.hasPermission(GUIAccessControlEntry.PERMISSION_MOVE) && !folder.isDefaultWorkspace()
				&& GUIFolder.TYPE_ALIAS != getSelectedRecord().getAttributeAsInt("type"));
		return move;
	}

	private MenuItem prepareMergeMenuItem(final GUIFolder folder) {
		MenuItem merge = new MenuItem();
		merge.setTitle(I18N.message("merge"));
		merge.addClickHandler(event -> new MergeDialog().show());
		merge.setEnabled(folder.hasPermission(GUIAccessControlEntry.PERMISSION_DELETE) && !folder.isDefaultWorkspace()
				&& GUIFolder.TYPE_ALIAS != getSelectedRecord().getAttributeAsInt("type"));
		return merge;
	}

	private MenuItem prepareDeleteMenuItem(final GUIFolder folder) {
		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> onDelete());
		delete.setEnabled(
				folder.hasPermission(GUIAccessControlEntry.PERMISSION_DELETE) && !folder.isDefaultWorkspace());
		return delete;
	}

	private MenuItem prepareSearchMenuItem(GUIFolder folder) {
		MenuItem search = new MenuItem();
		search.setTitle(I18N.message("search"));
		search.addClickHandler((MenuItemClickEvent event) -> {
			Search.get().getOptions().setFolder(folder.getId());
			Search.get().getOptions().setFolderName(folder.getName());
			Search.get().getOptions().setSearchInSubPath(false);
			Search.get().setOptions(Search.get().getOptions());
			MainPanel.get().selectSearchTab();
		});
		return search;
	}

	private MenuItem prepareRenameMenuItem(final GUIFolder folder) {
		MenuItem rename = new MenuItem();
		rename.setTitle(I18N.message("rename"));
		rename.addClickHandler(event -> onRename());
		rename.setEnabled(
				folder.hasPermission(GUIAccessControlEntry.PERMISSION_RENAME) && !folder.isDefaultWorkspace());
		return rename;
	}

	private MenuItem prepareCreateMenuItem(final GUIFolder folder) {
		MenuItem create = new MenuItem();
		create.setTitle(I18N.message("newfolder"));
		create.addClickHandler(event -> onCreate(folder.getId()));
		create.setEnabled(folder.hasPermission(GUIAccessControlEntry.PERMISSION_ADD));
		return create;
	}

	private void onDelete() {
		final List<Long> selectedIds = getSelectedIds();
		LD.contactingServer();
		DocumentService.Instance.get().countDocuments(selectedIds, Constants.DOC_ARCHIVED, new AsyncCallback<Long>() {

			@Override
			public void onFailure(Throwable caught) {
				LD.clearPrompt();
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Long count) {
				LD.clearPrompt();
				final String folderMessage = selectedIds.size() == 1 ? "confirmdeletefolder" : "confirmdeletefolders";
				final String documentMessage = selectedIds.size() == 1 ? "confirmdeletefolderarchdocs"
						: "confirmdeletefoldersarchdocs";
				LD.ask(I18N.message("question"),
						count.longValue() == 0L ? (I18N.message(folderMessage)) : (I18N.message(documentMessage)),
						yes -> {
							if (Boolean.TRUE.equals(yes)) {
								LD.contactingServer();
								doDelete(selectedIds);
							}
						});
			}
		});
	}

	private void doDelete(final List<Long> selectedIds) {
		TreeNode parentNode = getTree().getParent(getSelectedRecord());
		TreeNode firstNode = getTree().getChildren(parentNode)[0];

		FolderService.Instance.get().delete(selectedIds, new AsyncCallback<>() {
			@Override
			public void onFailure(Throwable caught) {
				LD.clearPrompt();

				if (caught instanceof RequestTimeoutException)
					SC.say("timeout");

				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void result) {
				LD.clearPrompt();

				for (long id : selectedIds) {
					TreeNode node = getTree().find(FOLDER_ID, Long.toString(id));
					if (node != null)
						getTree().remove(node);
				}

				if (parentNode == null || "/".equals(getTree().getPath(parentNode))) {
					// In case of a workspace we close the whole tree and select
					// first workspace
					getTree().closeAll();
					selectFolder(Long.parseLong(firstNode.getAttributeAsString(FOLDER_ID)));
					getTree().openFolder(firstNode);
					selectRecord(0);
				} else {
					selectFolder(Long.parseLong(parentNode.getAttributeAsString(FOLDER_ID)));
					reloadParentsOfSelection();
				}
			}
		});
	}

	/**
	 * Allows the selection of a folders template to apply to the current node
	 */
	private void onApplyTemplate() {
		new ApplyTemplateDialog().show();
	}

	/**
	 * Adds a bookmark to the currently selected folder.
	 */
	private void onAddBookmark() {
		final TreeNode selectedNode = getSelectedRecord();
		final long folderId = Long.parseLong(selectedNode.getAttributeAsString(FOLDER_ID));

		DocumentService.Instance.get().addBookmarks(Arrays.asList(folderId), 1, new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void v) {
				// Nothing to do
			}
		});
	}

	public static FolderNavigator get() {
		return instance;
	}

	void openAllParents(TreeNode node) {
		TreeNode parent = getTree().getParent(node);
		while (parent != null && !node.equals(parent)) {
			getTree().openFolder(parent);
			parent = getTree().getParent(parent);
		}
	}

	void onDataArrivedFirstTime() {
		/*
		 * Redirect the user to the correct folder and/or document
		 */
		RequestInfo loc = WindowUtils.getRequestInfo();
		if (loc.getParameter(FOLDER_ID) != null) {
			DocumentsPanel.get().openInFolder(Long.parseLong(loc.getParameter(FOLDER_ID)), null);
		} else if (loc.getParameter("docId") != null) {
			DocumentsPanel.get().openInFolder(Long.parseLong(loc.getParameter("docId")));
		} else {
			openDefaultNode();
		}

		FolderNavigator.this.firstTime = false;
	}

	private void openDefaultNode() {
		if (Session.get().getConfigAsBoolean("gui.folder.opentree")) {
			long folderId = 0;

			TreeNode rootNode = getTree().getRoot();
			TreeNode[] children = getTree().getChildren(rootNode);
			TreeNode nodeToOpen = null;

			if (children == null || children.length < 1)
				return;

			/*
			 * Get the first workspace and use it as the node to open
			 */
			folderId = Long.parseLong(children[0].getAttributeAsString(FOLDER_ID));
			nodeToOpen = children[0];

			/*
			 * Check if the user has specified a different default workspace
			 */
			if (Session.get().getUser().getDefaultWorkspace() != null) {
				folderId = Session.get().getUser().getDefaultWorkspace();
				for (TreeNode child : children) {
					long val = Long.parseLong(child.getAttributeAsString(FOLDER_ID));
					if (Session.get().getUser().getDefaultWorkspace() == val) {
						nodeToOpen = child;
						break;
					}
				}
			}

			getTree().openFolder(nodeToOpen);
			selectRecord(nodeToOpen);

			FolderService.Instance.get().getFolder(folderId, true, true, Session.get().isFolderPagination(),
					new AsyncCallback<>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(GUIFolder folder) {
							selectFolder(folder.getId());
						}
					});
		}
	}

	void resetPathToOpen(GUIFolder[] path) {
		pathToOpen = path;
		currentIndexInPathToOpen = 0;
	}

	boolean isPaginationEnabled() {
		return Session.get().getConfigAsBoolean("gui.folder.pagination");
	}

	/**
	 * Opens the tree to show the specified folder
	 * 
	 * @param folderId identifier of the folder
	 * @param documentId identifier of the document
	 */
	public void openFolder(final long folderId, final Long documentId) {
		getTree().closeAll();
		Session.get().setHiliteDocId(documentId);
		openFolder(folderId);
	}

	/**
	 * Opens the branch to show the specified folder, it cycles the path
	 * creating minimal tree nodes
	 * 
	 * @param folderId identifier of the folder to open
	 */
	public void openFolder(final long folderId) {
		getTree().closeAll();

		FolderService.Instance.get().getFolder(folderId, true, true, isPaginationEnabled(),
				new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIFolder folder) {
						long folderId = folder.getId();
						Long folderRef = folder.getFoldRef();
						if (folder.getFoldRef() != null) {
							folderId = folder.getFoldRef();
							folderRef = folder.getId();
						}

						TreeNode parent = getParentNode(folder);

						TreeNode node = new TreeNode(folder.getName());
						node.setAttribute("id", parent.getAttributeAsString("id") + "-" + Long.toString(folderId));
						node.setAttribute(FOLDER_ID, Long.toString(folderId));
						node.setAttribute("type", Integer.toString(folder.getType()));
						node.setAttribute(FOLD_REF, folderRef != null ? Long.toString(folderRef) : null);
						if (folder.getColor() != null)
							node.setAttribute(COLOR, folder.getColor());
						node.setAttribute(GUIAccessControlEntry.PERMISSION_ADD,
								Boolean.toString(folder.hasPermission(GUIAccessControlEntry.PERMISSION_ADD)));
						node.setAttribute(GUIAccessControlEntry.PERMISSION_DELETE,
								Boolean.toString(folder.hasPermission(GUIAccessControlEntry.PERMISSION_DELETE)));
						node.setAttribute(GUIAccessControlEntry.PERMISSION_RENAME,
								Boolean.toString(folder.hasPermission(GUIAccessControlEntry.PERMISSION_RENAME)));
						getTree().add(node, parent);
						parent = node;

						getTree().openFolders(getTree().getParents(parent));
						getTree().openFolder(parent);
						scrollToCell(getRowNum(parent), 0);
						selectRecord(parent);

						folder.setPathExtended(getPath(folderId));
						FolderController.get().selected(folder);
					}
				});
	}

	private TreeNode getParentNode(GUIFolder folder) {
		TreeNode parent = getTree().getRoot();
		for (GUIFolder fld : folder.getPath()) {
			if (fld.getId() == Constants.DOCUMENTS_FOLDERID)
				continue;

			long fldId = fld.getId();
			Long fldRef = fld.getFoldRef();
			if (fld.getFoldRef() != null) {
				fldId = fld.getFoldRef();
				fldRef = fld.getId();
			}

			String parentId = parent.getAttributeAsString("id");
			if ("/".equals(parentId))
				parentId = "" + Constants.DOCUMENTS_FOLDERID;

			TreeNode node = new TreeNode(fld.getName());
			node.setAttribute("id", parentId + "-" + Long.toString(fldId));
			node.setAttribute(FOLDER_ID, Long.toString(fldId));
			node.setAttribute("type", Integer.toString(fld.getType()));
			node.setAttribute(FOLD_REF, fldRef != null ? Long.toString(fldRef) : null);
			if (fld.getColor() != null)
				node.setAttribute(COLOR, fld.getColor());
			node.setAttribute(GUIAccessControlEntry.PERMISSION_ADD,
					fld.hasPermission(GUIAccessControlEntry.PERMISSION_ADD));
			node.setAttribute(GUIAccessControlEntry.PERMISSION_DELETE,
					fld.hasPermission(GUIAccessControlEntry.PERMISSION_DELETE));
			node.setAttribute(GUIAccessControlEntry.PERMISSION_RENAME,
					fld.hasPermission(GUIAccessControlEntry.PERMISSION_RENAME));

			getTree().add(node, parent);
			parent = node;
		}
		return parent;
	}

	public String getCurrentPath() {
		return getNodePath(getSelectedRecord());
	}

	void onCreate(long parentId) {
		GUIFolder folder = new GUIFolder();
		folder.setParentId(parentId);
		CreateDialog dialog = new CreateDialog(folder);
		dialog.show();
	}

	private void onRename() {
		final TreeNode selectedNode = getSelectedRecord();
		LD.askForValue(I18N.message("rename"), I18N.message("name"), selectedNode.getAttributeAsString("name"),
				value -> {
					if (value == null || "".equals(value.trim()))
						return;
					final String val = value.trim().replace("/", "").replace("\\\\", "");
					final long folderId = Long.parseLong(selectedNode.getAttributeAsString(FOLDER_ID));
					FolderService.Instance.get().rename(folderId, val, new AsyncCallback<>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Void v) {
							selectedNode.setAttribute("name", val);
							refreshRow(getRecordIndex(selectedNode));
						}
					});
				});
	}

	void onCreateWorkspace() {
		GUIFolder folder = new GUIFolder();
		folder.setType(1);
		CreateDialog dialog = new CreateDialog(folder);
		dialog.show();
	}

	private void onPaste() {
		TreeNode selectedNode = getSelectedRecord();
		final long folderId = Long.parseLong(selectedNode.getAttribute(FOLDER_ID));

		List<FormItem> items = new ArrayList<>();
		CheckboxItem copyDocuments = ItemFactory.newCheckbox("copydocuments");
		copyDocuments.setValue(true);
		copyDocuments.setDisabled(true);
		items.add(copyDocuments);

		CheckboxItem copyLinks = ItemFactory.newCheckbox("copylinks");
		copyLinks.setValue(true);
		items.add(copyLinks);

		CheckboxItem copyNotes = ItemFactory.newCheckbox("copynotes");
		copyNotes.setValue(true);
		items.add(copyNotes);

		CheckboxItem copySecuerity = ItemFactory.newCheckbox("copysecurity");
		copySecuerity.setValue(true);
		items.add(copySecuerity);

		LD.askForValues(I18N.message("copyoptions"), null, items, null, new ValuesCallback() {

			@Override
			public void execute(Map<String, Object> values) {
				FolderService.Instance.get().paste(
						Clipboard.getInstance().stream().map(doc -> doc.getId()).collect(Collectors.toList()), folderId,
						Clipboard.getInstance().getLastAction(), Boolean.TRUE.equals(values.get("copylinks")),
						Boolean.TRUE.equals(values.get("copynotes")), Boolean.TRUE.equals(values.get("copysecurity")),
						new AsyncCallback<>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void result) {
								DocumentsPanel.get().onFolderSelected(FolderController.get().getCurrentFolder());
								Clipboard.getInstance().clear();
							}
						});
			}

			@Override
			public void execute(String value) {
				// Not used
			}
		});
	}

	private void onPasteAsAlias() {
		TreeNode selectedNode = getSelectedRecord();
		final long folderId = Long.parseLong(selectedNode.getAttribute(FOLDER_ID));
		final List<Long> docIds = Clipboard.getInstance().stream().map(d -> d.getId()).collect(Collectors.toList());

		if (Feature.enabled(Feature.PDF))
			LD.askForValue(I18N.message("pasteasalias"), "type", "", ItemFactory.newAliasTypeSelector(),
					type -> pasteAsAlias(folderId, docIds, type));
		else
			pasteAsAlias(folderId, docIds, null);
	}

	private void pasteAsAlias(final long folderId, final List<Long> docIds, String type) {
		FolderService.Instance.get().pasteAsAlias(docIds, folderId, type, new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void result) {
				DocumentsPanel.get().onFolderSelected(FolderController.get().getCurrentFolder());
				Clipboard.getInstance().clear();
				GuiLog.debug("Paste as Alias operation completed.");
			}
		});
	}

	@Override
	public void enable() {
		super.enable();
		getTree().setReportCollisions(false);
	}

	/**
	 * Gets all the IDs of the selected folders
	 * 
	 * @return identifiers of folders
	 */
	public List<Long> getSelectedIds() {
		ListGridRecord[] selection = getSelectedRecords();
		List<Long> ids = new ArrayList<>();
		for (ListGridRecord rec : selection)
			ids.add(rec.getAttributeAsLong(FOLDER_ID));
		return ids;
	}

	/**
	 * Moves the currently selected folder to the new parent folder
	 * 
	 * @param targetFolderId The parent folder
	 */
	public void moveTo(final long targetFolderId) {
		List<Long> ids = getSelectedIds();
		for (Long id : ids) {
			TreeNode node = getTree().find(FOLDER_ID, (Object) id);
			getTree().remove(node);
		}

		LD.contactingServer();
		FolderService.Instance.get().move(ids, targetFolderId, new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				LD.clearPrompt();
				GuiLog.serverError(caught);
				GuiLog.warn(I18N.message(OPERATIONNOTALLOWED), null);
			}

			@Override
			public void onSuccess(Void ret) {
				LD.clearPrompt();
				TreeNode target = getTree().find(FOLDER_ID, Long.toString(targetFolderId));
				if (target != null)
					getTree().reloadChildren(target);
			}
		});
	}

	/**
	 * Merges the currently selected folders to a target folder
	 * 
	 * @param targetFolderId The target folder
	 */
	public void mergeTo(final long targetFolderId) {
		List<Long> ids = getSelectedIds();
		for (Long id : ids) {
			TreeNode node = getTree().find(FOLDER_ID, (Object) id);
			getTree().remove(node);
		}

		LD.contactingServer();
		FolderService.Instance.get().merge(ids, targetFolderId, new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				LD.clearPrompt();
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void ret) {
				LD.clearPrompt();
				TreeNode target = getTree().find(FOLDER_ID, Long.toString(targetFolderId));
				if (target != null)
					getTree().reloadChildren(target);
			}
		});
	}

	/**
	 * Copies the currently selected folders to the new parent folder
	 * 
	 * @param targetFolderId identifier of the parent folder
	 * @param foldersOnly to create just the folders
	 * @param securityOption how to setup the security for the new folder'none',
	 *        'inherit' or 'replicate'
	 */
	public void copyTo(long targetFolderId, boolean foldersOnly, String securityOption) {
		final TreeNode target = getTree().findById(Long.toString(targetFolderId));

		LD.contactingServer();
		FolderService.Instance.get().copyFolders(getSelectedIds(), targetFolderId, foldersOnly, securityOption, null,
				new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						LD.clearPrompt();
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void ret) {
						LD.clearPrompt();
						if (target != null)
							getTree().reloadChildren(target);
					}
				});
	}

	/**
	 * Creates an alias in the currently selected folder
	 * 
	 * @param referencedFolderId The original folder to reference
	 */
	public void createAlias(long referencedFolderId) {
		final TreeNode parent = getSelectedRecord();

		FolderService.Instance.get().createAlias(parent.getAttributeAsLong(FOLDER_ID), referencedFolderId,
				new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIFolder ret) {
						if (parent != null)
							getTree().reloadChildren(parent);
					}
				});
	}

	public boolean isFirstTime() {
		return firstTime;
	}

	@Override
	public void onFolderChanged(GUIFolder folder) {
		TreeNode folderNode = getTree().find(FOLDER_ID, Long.toString(folder.getId()));

		if (folderNode != null) {
			folderNode.setTitle(folder.getName());
			folderNode.setName(folder.getName());
			folderNode.setAttribute(COLOR, folder.getColor());

			getTree().reloadChildren(folderNode);

			boolean positionChanged = folderNode.getAttributeAsInt(POSITION) != folder.getPosition();

			if (positionChanged) {
				folderNode.setAttribute(POSITION, folder.getPosition());
				TreeNode parentNode = getTree().find(FOLDER_ID, Long.toString(folder.getParentId()));
				if (parentNode != null)
					getTree().reloadChildren(parentNode);
				else
					getTree().reloadChildren(getTree().getRoot());
			}
		}
	}

	@Override
	public void onFolderBeginEditing(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderCancelEditing(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderSelected(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderCreated(GUIFolder folder) {
		TreeNode parent = getTree().find(FOLDER_ID, (Object) folder.getParentId());
		if (parent == null)
			return;

		TreeNode node = new TreeNode();
		node.setTitle(folder.getName());
		node.setName(folder.getName());
		node.setAttribute(COLOR, folder.getColor());
		node.setAttribute(FOLDER_ID, folder.getId());
		node.setAttribute("id", parent.getAttribute("id") + "-" + folder.getId());
		node.setAttribute("type", folder.getType());
		node.setAttribute("customIcon", "folder");
		node.setAttribute("status", 0);
		node.setAttribute("publishedStatus", "yes");
		node.setAttribute(POSITION, folder.getPosition());
		if (folder.getFoldRef() != null) {
			node.setAttribute(FOLD_REF, folder.getFoldRef());
			node.setAttribute("customIcon", "folder_alias");
		}

		getTree().add(node, parent);
	}

	@Override
	public void onFolderDeleted(GUIFolder folder) {
		TreeNode node = getTree().find(FOLDER_ID, (Object) folder.getId());
		if (node == null)
			return;

		TreeNode parent = getTree().getParent(node);
		if (parent.getAttributeAsLong(FOLDER_ID) != null
				&& folder.getId() == FolderController.get().getCurrentFolder().getId())
			selectFolder(parent.getAttributeAsLong(FOLDER_ID));

		getTree().remove(node);
		getTree().reloadChildren(parent);
	}

	@Override
	public void onFolderMoved(GUIFolder folder) {
		TreeNode node = getTree().find(FOLDER_ID, (Object) folder.getId());
		if (node != null)
			getTree().remove(node);

		TreeNode parent = getTree().getParent(node);
		if (parent != null)
			getTree().reloadChildren(parent);

		onFolderCreated(folder);
	}

	public void onArchive(final long folderId) {
		LD.askForValue(I18N.message("warning"), I18N.message("archiveadvice"), "", 400, value -> {
			if (value == null)
				return;

			if (value.isEmpty())
				SC.warn(I18N.message("commentrequired"));
			else
				DocumentService.Instance.get().archiveFolder(folderId, value, new AsyncCallback<Long>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Long result) {
						GuiLog.info(I18N.message("documentswerearchived", "" + result), null);
						reload();
					}
				});
		});
	}

	private void onAutomation(final long folderId) {
		new AutomationDialog(folderId, null).show();
	}

	public TreeNode getNode(long folderId) {
		return getTree().find(FOLDER_ID, Long.toString(folderId));
	}

	@Override
	public long getSelectedFolderId() {
		return getSelectedRecord().getAttributeAsLong(FOLDER_ID);
	}

	public TreeNode getRootNode() {
		return getTree().getRoot();
	}

	private void reloadParentsOfSelection() {
		ListGridRecord[] selection = getSelectedRecords();
		for (ListGridRecord rec : selection) {
			try {
				TreeNode node = getTree().find("id", rec.getAttributeAsString("id"));
				TreeNode parentNode = getTree().getParent(node);
				if (parentNode != null) {
					getTree().reloadChildren(parentNode);
				} else {
					getTree().reloadChildren(getRootNode());
				}
			} catch (Exception t) {
				// Nothing to do
			}
		}
	}

	@Override
	public void destroy() {
		FolderController.get().removeObserver(this);
	}

	private MenuItem prepareCustomActionsMenu(final long folderId) {
		Menu customActionsMenu = new Menu();
		for (GUIMenu menuAction : Session.get().getUser().getCustomActions())
			prepareCustomActionMenuItem(folderId, menuAction, customActionsMenu);

		MenuItem customActionsItem = new MenuItem(I18N.message("customactions"));
		customActionsItem.setSubmenu(customActionsMenu);
		return customActionsItem;
	}

	private void prepareCustomActionMenuItem(final long folderId, GUIMenu menuAction, Menu customActionsMenu) {
		MenuItem actionItem = new MenuItem(I18N.message(menuAction.getName()));
		customActionsMenu.addItem(actionItem);

		actionItem.addClickHandler(event ->
		/**
		 * Check on the server if the action has been modified
		 */
		SecurityService.Instance.get().getMenu(menuAction.getId(), I18N.getLocale(), new AsyncCallback<>() {

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
					 * An automation cript is specified directly, so launch it's
					 * execution
					 */
					GUIAutomationRoutine routine = new GUIAutomationRoutine();
					routine.setAutomation(action.getAutomation());
					executeRoutine(folderId, null, routine);
				} else if (action.getRoutineId() != null && action.getRoutineId().longValue() != 0L) {
					AutomationService.Instance.get().getRoutine(action.getRoutineId(),
							new AsyncCallback<>() {

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
												folderId, null);
										dialog.show();
									} else {
										/*
										 * A routine without parameters is
										 * referenced, so launch directly
										 */
										executeRoutine(folderId, null, routine);
									}
								}
							});
				}
			}
		}));
	}

	private void executeRoutine(long folderId, List<Long> docIds, GUIAutomationRoutine routine) {
		AutomationService.Instance.get().execute(routine, docIds, folderId, new AsyncCallback<>() {

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

	protected void handleDataArrived() {
		if (isFirstTime()) {
			onDataArrivedFirstTime();
		} else {
			onDataArrived();
		}
	}

	private void onDataArrived() {
		/*
		 * A folder has been opened, check if we have to handle an openInFolder
		 * task
		 */
		if (pathToOpen == null)
			return;

		GUIFolder fld = pathToOpen[currentIndexInPathToOpen];
		if (isPaginationEnabled())
			cursor.onFolderSelected(fld);

		if (fld.getName().equals("/"))
			fld = pathToOpen[++currentIndexInPathToOpen];

		while (fld.getName().isEmpty())
			fld = pathToOpen[++currentIndexInPathToOpen];

		TreeNode node = getTree().find(FOLDER_ID, "" + fld.getId());

		// Perhaps the node is in another page
		if (node == null) {
			if (isPaginationEnabled()) {
				GUIFolder parentFolder = pathToOpen[currentIndexInPathToOpen - 1];
				cursor.onFolderSelected(parentFolder);
				cursor.next();

				FolderService.Instance.get().setFolderPagination(cursor.getCurrentPagination().getFolderId(),
						cursor.getCurrentPagination().getStartRow(), cursor.getCurrentPagination().getPageSize(),
						new AsyncCallback<>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void arg) {
								TreeNode parentNode = getTree().find(FOLDER_ID, "" + cursor.getFolderId());
								getTree().reloadChildren(parentNode);
							}
						});
			}
			return;
		}

		if (Boolean.FALSE.equals(getTree().isOpen(node))) {
			getTree().openFolder(node);
			currentIndexInPathToOpen++;
			getTree().reloadChildren(node);
		}

		// Open all the parents
		openAllParents(node);

		if (fld.getId() == pathToOpen[pathToOpen.length - 1].getId()) {
			resetPathToOpen(null);
			scrollToCell(getRowNum(node), 0);

			FolderService.Instance.get().getFolder(fld.getId(), true, true, Session.get().isFolderPagination(),
					new AsyncCallback<>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(GUIFolder folder) {
							getTree().openFolder(node);
							scrollToCell(getRowNum(node), 0);
							selectRecord(node);
							FolderController.get().selected(folder);
						}
					});
		}
	}

	/**
	 * Reloads the children of the current node and also re-select the current
	 * folder
	 */
	public void reload() {
		TreeNode selectedNode = getSelectedRecord();
		getTree().reloadChildren(selectedNode);
		selectFolder(selectedNode.getAttributeAsLong(FOLDER_ID));
	}

	/**
	 * Handles the click on a folder to show the contained documents
	 */
	protected void addCellClickHandler() {
		addCellClickHandler(event -> {
			long selectedFolderId = event.getRecord().getAttributeAsLong(FOLDER_ID);

			selectFolder(selectedFolderId);

			if (Session.get().getConfigAsBoolean("gui.folder.openonselect")) {
				// Expand the selected node if it is not already expanded
				TreeNode selectedNode = getSelectedRecord();
				openFolder(selectedNode);
			}
		});
	}
}