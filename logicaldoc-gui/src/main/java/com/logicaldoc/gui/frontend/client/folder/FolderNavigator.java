package com.logicaldoc.gui.frontend.client.folder;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.google.gwt.core.client.Scheduler;
import com.google.gwt.http.client.RequestTimeoutException;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAutomationRoutine;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIMenu;
import com.logicaldoc.gui.common.client.data.FoldersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.observer.FolderController;
import com.logicaldoc.gui.common.client.observer.FolderObserver;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.RequestInfo;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.common.client.widgets.grid.FolderListGridField;
import com.logicaldoc.gui.frontend.client.clipboard.Clipboard;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.document.SendToArchiveDialog;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentGridUtil;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsGrid;
import com.logicaldoc.gui.frontend.client.document.grid.FillRoutineParams;
import com.logicaldoc.gui.frontend.client.folder.copy.FolderCopyDialog;
import com.logicaldoc.gui.frontend.client.panels.MainPanel;
import com.logicaldoc.gui.frontend.client.search.Search;
import com.logicaldoc.gui.frontend.client.services.AutomationService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.logicaldoc.gui.frontend.client.subscription.SubscriptionDialog;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.util.EventHandler;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.events.DragStartEvent;
import com.smartgwt.client.widgets.events.DropEvent;
import com.smartgwt.client.widgets.events.KeyDownEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellClickEvent;
import com.smartgwt.client.widgets.grid.events.CellClickHandler;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.tree.TreeGrid;
import com.smartgwt.client.widgets.tree.TreeNode;
import com.smartgwt.client.widgets.tree.events.FolderClosedEvent;
import com.smartgwt.client.widgets.tree.events.FolderOpenedEvent;
import com.smartgwt.client.widgets.tree.events.FolderOpenedHandler;

/**
 * The panel that shows the workspaces/folders navigation tree
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class FolderNavigator extends TreeGrid implements FolderObserver {

	private static final String POSITION = "position";

	private static final String COLOR = "color";

	private static final String FOLD_REF = "foldRef";

	private static final String OPERATIONNOTALLOWED = "operationnotallowed";

	private static final String FOLDER_ID = "folderId";

	private static final String OPENED = "opened";

	/**
	 * String typed by the user inside the tree, to quickly select folders
	 */
	private String typed;

	/**
	 * When the user typed last time
	 */
	private Date lastTyped;

	private static FolderNavigator instance = new FolderNavigator();

	private boolean firstTime = true;

	// Indicates if the Navigator is in the process of opening this path
	private GUIFolder[] pathToOpen = null;

	private int currentIndexInPathToOpen = 0;

	private FolderNavigator() {
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

		addDragStartHandler((DragStartEvent ev) -> {
			if (EventHandler.getDragTarget() instanceof FolderNavigator) {
				// Workspaces cannot be moved
				if ("1".equals(getDragData()[0].getAttributeAsString("type"))) {
					ev.cancel();
					return;
				}
			}
		});

		addDropHandler();

		addCellContextHandler();

		addCellClickHandler();

		addFolderOpenedHandler();

		/*
		 * To refresh the folder's decoration
		 */
		addFolderClosedHandler((FolderClosedEvent folderClosedEvent) -> {
			folderClosedEvent.getNode().setAttribute(OPENED, false);
			updateData(folderClosedEvent.getNode());
		});

		// Used to expand root folder after login or to open in folder
		addDataArrivedHandler((DataArrivedEvent dataArrivedEvent) -> {
			FolderNavigator.this.onDataArrived(dataArrivedEvent);
		});

		FolderCursor.get().registerMaxChangedHandler((ChangedEvent maxChangedEvent) -> {
			reloadChildren();
		});

		FolderCursor.get().registerPageChangedHandler((ChangedEvent changedEvent) -> {
			reloadChildren();
		});

		/*
		 * A listener of the keypress to automatically collect a string typed by
		 * the user to quickly select the folder
		 */
		addKeyDownHandler((KeyDownEvent keyDown) -> {
			String key = EventHandler.getKey();
			if ("space".equals(key.toLowerCase())) {
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
		addDropHandler((final DropEvent dropEvent) -> {
			if (EventHandler.getDragTarget() instanceof FolderNavigator) {
				// Workspaces cannot be moved
				if ("1".equals(getDragData()[0].getAttributeAsString("type"))) {
					dropEvent.cancel();
					return;
				}

				doMoveFolderOnDrop();
			} else if (EventHandler.getDragTarget() instanceof DocumentsGrid) {
				/*
				 * In this case we are moving a document
				 */
				DocumentsGrid grid = (DocumentsGrid) EventHandler.getDragTarget();
				final GUIDocument[] selection = grid.getSelectedDocuments();
				if (selection == null || selection.length == 0)
					return;
				final long[] ids = getSelectedDocIDs(selection);

				final TreeNode selectedNode = getDropFolder();
				final long folderId = Long.parseLong(selectedNode.getAttribute(FOLDER_ID));

				if (FolderController.get().getCurrentFolder().getId() == folderId)
					return;

				doMoveDocumentOnDrop(selection, ids, selectedNode, folderId);
			}
		});
	}

	private long[] getSelectedDocIDs(final GUIDocument[] selection) {
		final long[] ids = new long[selection.length];
		for (int i = 0; i < selection.length; i++)
			ids[i] = selection[i].getId();
		return ids;
	}

	/**
	 * Installs a Timer that selects the typed folder once the user stops typing
	 */
	private void installTimer() {
		Scheduler.get().scheduleFixedDelay(() -> {
			Date now = new Date();
			if (lastTyped != null && typed != null) {
				long dist = now.getTime() - lastTyped.getTime();
				if (dist > 500) {
					AdvancedCriteria crit = new AdvancedCriteria("name", OperatorId.ISTARTS_WITH, typed);
					Record rec = find(crit);
					if (rec != null) {
						final Long folderId = rec.getAttributeAsLong(FOLDER_ID);
						deselectAllRecords();
						selectRecord(rec);
						selectFolder(folderId);
					}

					typed = null;
					lastTyped = null;
				}
			}
			return true;
		}, 500);
	}

	/**
	 * Refreshes the folder's decoration and in case of need we add a listener
	 * that closes all the previously opened branches.
	 */
	private void addFolderOpenedHandler() {
		addFolderOpenedHandler(new FolderOpenedHandler() {

			@Override
			public void onFolderOpened(FolderOpenedEvent event) {
				event.getNode().setAttribute(OPENED, true);
				updateData(event.getNode());

				if (Session.get().getConfigAsBoolean("gui.folder.autoclose")) {
					TreeNode selectedNode = event.getNode();
					long selectedFolderId = selectedNode.getAttributeAsLong(FOLDER_ID);

					// Expand the selected node if it is not already expanded
					TreeNode parentNode = getTree().getParent(selectedNode);
					TreeNode[] children = getTree().getChildren(parentNode);
					if (children != null)
						for (TreeNode child : children) {
							if (child.getAttributeAsLong(FOLDER_ID) == selectedFolderId)
								continue;
							else {
								getTree().closeAll(child);
								child.setAttribute(OPENED, false);
								updateData(child);
							}
						}
				}
			}
		});
	}

	/**
	 * Handles the click on a folder to show the contained documents
	 */
	private void addCellClickHandler() {
		addCellClickHandler(new CellClickHandler() {
			@Override
			public void onCellClick(CellClickEvent event) {
				long selectedFolderId = event.getRecord().getAttributeAsLong(FOLDER_ID);

				selectFolder(selectedFolderId);

				if (Session.get().getConfigAsBoolean("gui.folder.openonselect")) {
					// Expand the selected node if it is not already expanded
					TreeNode selectedNode = (TreeNode) getSelectedRecord();
					openFolder(selectedNode);
				}
			}
		});
	}

	/**
	 * Handles the body click on folder name to create the context menu
	 */
	private void addCellContextHandler() {
		addCellContextClickHandler((CellContextClickEvent contextClickEvent) -> {
			if (getSelectedRecord() != null && getSelectedRecords().length > 1) {
				Menu contextMenu = prepateContextMenu();
				contextMenu.showContextMenu();
			} else {
				FolderService.Instance.get().getFolder(getSelectedRecord().getAttributeAsLong(FOLDER_ID), false, true,
						false, new AsyncCallback<GUIFolder>() {

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

	private void doMoveDocumentOnDrop(final GUIDocument[] selection, final long[] ids, final TreeNode selectedNode,
			final long folderId) {
		final String sourceName = selection.length == 1 ? selection[0].getFileName()
				: (selection.length + " " + I18N.message("documents").toLowerCase());
		final String targetName = selectedNode.getAttributeAsString("name");

		LD.ask(I18N.message("move"), I18N.message("moveask", new String[] { sourceName, targetName }),
				(Boolean yes) -> {
					if (Boolean.TRUE.equals(yes)) {
						FolderService.Instance.get().paste(ids, folderId, "cut", new AsyncCallback<Void>() {
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
		final long[] source = getSelectedIds();
		final long target = Long.parseLong(getDropFolder().getAttributeAsString(FOLDER_ID));

		final String sourceName = getDragData()[0].getAttributeAsString("name");
		final String targetName = getDropFolder().getAttributeAsString("name");

		LD.ask(I18N.message("move"), I18N.message("moveask", new String[] { sourceName, targetName }),
				(Boolean yes) -> {
					if (Boolean.TRUE.equals(yes)) {
						for (long id : source) {
							TreeNode node = getTree().find(FOLDER_ID, (Object) id);
							getTree().remove(node);
						}

						FolderService.Instance.get().move(source, target, new AsyncCallback<Void>() {
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

	protected String getOriginalIcon(Record rec, boolean defaultState) {
		return super.getIcon(rec, defaultState);
	}

	/**
	 * Select the specified folder
	 * 
	 * @param folderId the folder's identifier
	 */
	public void selectFolder(final long folderId) {
		FolderService.Instance.get().getFolder(folderId, false, true, Session.get().isFolderPagination(),
				new AsyncCallback<GUIFolder>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIFolder result) {
						if (result != null) {
							result.setPathExtended(getPath(folderId));
							FolderController.get().selected(result);
							if (Session.get().isFolderPagination()) {
								if (result.getGrid() != null && !result.getGrid().isEmpty())
									FolderCursor.get().setPageSizeAndTotalRecords(
											DocumentGridUtil.getFolderPageSizeFromSpec(result.getGrid()),
											result.getSubfolderCount());
								else if (Session.get().getUser().getDocsGrid() != null
										&& !Session.get().getUser().getDocsGrid().isEmpty())
									FolderCursor.get()
											.setPageSizeAndTotalRecords(
													DocumentGridUtil.getFolderPageSizeFromSpec(
															Session.get().getUser().getDocsGrid()),
													result.getSubfolderCount());
								else
									FolderCursor.get().setTotalRecords(result.getSubfolderCount());
							}
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
		move.addClickHandler((MenuItemClickEvent event) -> {
			MoveDialog dialog = new MoveDialog();
			dialog.show();
		});

		MenuItem copy = new MenuItem();
		copy.setTitle(I18N.message("copy"));
		copy.addClickHandler((MenuItemClickEvent event) -> {
			FolderCopyDialog dialog = new FolderCopyDialog();
			dialog.show();
		});

		MenuItem merge = new MenuItem();
		merge.setTitle(I18N.message("merge"));
		merge.addClickHandler((MenuItemClickEvent event) -> {
			MergeDialog dialog = new MergeDialog();
			dialog.show();
		});

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler((MenuItemClickEvent event) -> onDelete());

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
		createWorkspace.addClickHandler((MenuItemClickEvent nwClick) -> onCreateWorkspace());

		MenuItem reload = new MenuItem();
		reload.setTitle(I18N.message("reload"));
		reload.addClickHandler((MenuItemClickEvent reloadClick) -> reload());

		MenuItem move = prepareMoveMenuItem(selectedFolder);

		MenuItem copy = prepareCopyMenuItem();

		MenuItem merge = prepareMergeMenuItem(selectedFolder);

		MenuItem paste = new MenuItem();
		paste.setTitle(I18N.message("paste"));
		paste.addClickHandler((MenuItemClickEvent pasteClick) -> onPaste());

		MenuItem pasteAsAlias = new MenuItem();
		pasteAsAlias.setTitle(I18N.message("pasteasalias"));
		pasteAsAlias.addClickHandler((MenuItemClickEvent pasteAliasClick) -> onPasteAsAlias());

		MenuItem exportZip = prepareExportZipMenuItem(selectedFolder);

		MenuItem addBookmark = new MenuItem();
		addBookmark.setTitle(I18N.message("addbookmark"));
		addBookmark.addClickHandler((MenuItemClickEvent bookmarkClick) -> onAddBookmark());

		if (!selectedFolder.hasPermission(Constants.PERMISSION_WRITE) || Clipboard.getInstance().isEmpty()) {
			paste.setEnabled(false);
			pasteAsAlias.setEnabled(false);
		}

		if (!selectedFolder.hasPermission(Constants.PERMISSION_ADD)) {
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
				&& Session.get().getUser().getCustomActions() != null
				&& Session.get().getUser().getCustomActions().length > 0) {
			MenuItem customActionsItem = prepareCustomActionsMenu(selectedFolder.getId());
			contextMenu.addItem(customActionsItem);
		}
	}

	private void addAutomationMenuItem(final GUIFolder selectedFolder, Menu contextMenu) {
		if (Feature.visible(Feature.AUTOMATION)) {
			MenuItem automation = new MenuItem();
			automation.setTitle(I18N.message("executeautomation"));
			automation.addClickHandler((MenuItemClickEvent eaClick) -> {
				onAutomation(selectedFolder.getId());
			});
			contextMenu.addItem(automation);
			if (!Feature.enabled(Feature.AUTOMATION)
					|| !selectedFolder.hasPermission(Constants.PERMISSION_AUTOMATION)) {
				automation.setEnabled(false);
			}
		}
	}

	private void addSendToExportArchiveMenuItem(final GUIFolder folder, Menu contextMenu) {
		if (Feature.visible(Feature.IMPEX)) {
			MenuItem sendToExpArchive = new MenuItem();
			sendToExpArchive.setTitle(I18N.message("sendtoexparchive"));
			sendToExpArchive.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
				public void onClick(MenuItemClickEvent event) {
					LD.ask(I18N.message("question"), I18N.message("confirmputinexparchive"), (Boolean yes) -> {
						if (Boolean.TRUE.equals(yes)) {
							SendToArchiveDialog archiveDialog = new SendToArchiveDialog(new long[] { folder.getId() },
									false);
							archiveDialog.show();
						}
					});
				}
			});
			contextMenu.addItem(sendToExpArchive);
			if (!Feature.enabled(Feature.IMPEX) || !folder.hasPermission(Constants.PERMISSION_EXPORT))
				sendToExpArchive.setEnabled(false);
		}
	}

	private void addArchiveMenuItem(final GUIFolder folder, Menu contextMenu) {
		if (Feature.visible(Feature.ARCHIVING)) {
			MenuItem archive = new MenuItem();
			archive.setTitle(I18N.message("archive"));
			archive.addClickHandler((MenuItemClickEvent archiveClick) -> {
				onArchive(folder.getId());
			});
			contextMenu.addItem(archive);
			if (!Feature.enabled(Feature.ARCHIVING) || !folder.hasPermission(Constants.PERMISSION_ARCHIVE))
				archive.setEnabled(false);
		}
	}

	private void addApplyTemplateMenuItem(final GUIFolder folder, Menu contextMenu) {
		if (Feature.visible(Feature.FOLDER_TEMPLATE)) {
			MenuItem applyTemplate = new MenuItem();
			applyTemplate.setTitle(I18N.message("applytemplate"));
			applyTemplate.addClickHandler((MenuItemClickEvent applyTemplateClick) -> {
				onApplyTemplate();
			});
			contextMenu.addItem(applyTemplate);
			if (!Feature.enabled(Feature.FOLDER_TEMPLATE) || !folder.hasPermission(Constants.PERMISSION_ADD))
				applyTemplate.setEnabled(false);
		}
	}

	private void addSubscribeMenuItem(GUIFolder folder, Menu contextMenu) {
		if (Feature.visible(Feature.AUDIT)) {
			MenuItem subscribe = new MenuItem();
			subscribe.setTitle(I18N.message("subscribe"));
			subscribe.addClickHandler((MenuItemClickEvent click) -> {
				SubscriptionDialog dialog = new SubscriptionDialog(folder.getId(), null);
				dialog.show();
			});
			subscribe.setEnabled(Feature.enabled(Feature.AUDIT));
			contextMenu.addItem(subscribe);
		}
	}

	private MenuItem prepareCopyMenuItem() {
		MenuItem copy = new MenuItem();
		copy.setTitle(I18N.message("copy"));
		copy.addClickHandler((MenuItemClickEvent copyClick) -> {
			FolderCopyDialog dialog = new FolderCopyDialog();
			dialog.show();
		});
		return copy;
	}

	private MenuItem prepareCreateAliasMenuItem() {
		MenuItem createAlias = new MenuItem();
		createAlias.setTitle(I18N.message("createalias"));
		createAlias.addClickHandler((MenuItemClickEvent caClick) -> {
			CreateAliasDialog dialog = new CreateAliasDialog();
			dialog.show();
		});
		createAlias.setEnabled(getSelectedRecord().getAttributeAsString(FOLD_REF) == null);
		return createAlias;
	}

	private MenuItem prepareExportZipMenuItem(final GUIFolder folder) {
		MenuItem exportZip = new MenuItem();
		exportZip.setTitle(I18N.message("exportzip"));
		exportZip.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				Window.open(Util.contextPath() + "zip-export?folderId=" + folder.getId(), "_blank", "");
			}
		});
		exportZip.setEnabled(folder.hasPermission(Constants.PERMISSION_EXPORT)
				&& folder.hasPermission(Constants.PERMISSION_DOWNLOAD));
		return exportZip;
	}

	private MenuItem prepareMoveMenuItem(final GUIFolder folder) {
		MenuItem move = new MenuItem();
		move.setTitle(I18N.message("move"));
		move.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				MoveDialog dialog = new MoveDialog();
				dialog.show();
			}
		});
		move.setEnabled(folder.hasPermission(Constants.PERMISSION_DELETE) && !folder.isDefaultWorkspace()
				&& GUIFolder.TYPE_ALIAS != getSelectedRecord().getAttributeAsInt("type"));
		return move;
	}

	private MenuItem prepareMergeMenuItem(final GUIFolder folder) {
		MenuItem merge = new MenuItem();
		merge.setTitle(I18N.message("merge"));
		merge.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				MergeDialog dialog = new MergeDialog();
				dialog.show();
			}
		});
		merge.setEnabled(folder.hasPermission(Constants.PERMISSION_DELETE) && !folder.isDefaultWorkspace()
				&& GUIFolder.TYPE_ALIAS != getSelectedRecord().getAttributeAsInt("type"));
		return merge;
	}

	private MenuItem prepareDeleteMenuItem(final GUIFolder folder) {
		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				onDelete();
			}
		});
		delete.setEnabled(folder.hasPermission(Constants.PERMISSION_DELETE) && !folder.isDefaultWorkspace()
				&& GUIFolder.TYPE_ALIAS != getSelectedRecord().getAttributeAsInt("type"));
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
		rename.addClickHandler((MenuItemClickEvent event) -> {
			onRename();
		});
		rename.setEnabled(folder.hasPermission(Constants.PERMISSION_RENAME) && !folder.isDefaultWorkspace());
		return rename;
	}

	private MenuItem prepareCreateMenuItem(final GUIFolder folder) {
		MenuItem create = new MenuItem();
		create.setTitle(I18N.message("newfolder"));
		create.addClickHandler((MenuItemClickEvent event) -> {
			onCreate(folder.getId());
		});
		create.setEnabled(folder.hasPermission(Constants.PERMISSION_ADD));
		return create;
	}

	private void onDelete() {
		final long[] selectedIds = getSelectedIds();
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
				LD.ask(I18N.message("question"), count.longValue() == 0L
						? (I18N.message(selectedIds.length == 1 ? "confirmdeletefolder" : "confirmdeletefolders"))
						: (I18N.message(selectedIds.length == 1 ? "confirmdeletefolderarchdocs"
								: "confirmdeletefoldersarchdocs")),
						(Boolean yes) -> {
							if (Boolean.TRUE.equals(yes)) {
								LD.contactingServer();
								doDelete(selectedIds);
							}
						});
			}
		});
	}

	private void doDelete(final long[] selectedIds) {
		FolderService.Instance.get().delete(selectedIds, new AsyncCallback<Void>() {
			@Override
			public void onFailure(Throwable caught) {
				LD.clearPrompt();

				if (caught instanceof RequestTimeoutException) {
					SC.say("timeout");
				}

				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void result) {
				LD.clearPrompt();
				TreeNode node = getTree().find(FOLDER_ID, (Object) getSelectedRecord().getAttributeAsString(FOLDER_ID));
				TreeNode parent = getTree().getParent(node);

				if (parent.getAttributeAsString(FOLDER_ID) != null)
					selectFolder(Long.parseLong(parent.getAttributeAsString(FOLDER_ID)));

				reloadParentsOfSelection();
			}
		});
	}

	/**
	 * Allows the selection of a folders template to apply to the current node
	 */
	private void onApplyTemplate() {
		ApplyTemplateDialog dialog = new ApplyTemplateDialog();
		dialog.show();
	}

	/**
	 * Adds a bookmark to the currently selected folder.
	 */
	private void onAddBookmark() {
		final TreeNode selectedNode = (TreeNode) getSelectedRecord();
		final long folderId = Long.parseLong(selectedNode.getAttributeAsString(FOLDER_ID));

		DocumentService.Instance.get().addBookmarks(new long[] { folderId }, 1, new AsyncCallback<Void>() {

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

	private void onDataArrived(DataArrivedEvent event) {
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
			FolderCursor.get().onFolderSelected(fld);

		if (fld.getName().equals("/"))
			fld = pathToOpen[++currentIndexInPathToOpen];

		while (fld.getName().isEmpty())
			fld = pathToOpen[++currentIndexInPathToOpen];

		TreeNode node = getTree().find(FOLDER_ID, "" + fld.getId());

		// Perhaps the node is in another page
		if (node == null) {
			if (isPaginationEnabled()) {
				GUIFolder parentFolder = pathToOpen[currentIndexInPathToOpen - 1];
				FolderCursor.get().onFolderSelected(parentFolder);
				FolderCursor.get().next();

				FolderService.Instance.get().setFolderPagination(
						FolderCursor.get().getCurrentPagination().getFolderId(),
						FolderCursor.get().getCurrentPagination().getStartRow(),
						FolderCursor.get().getCurrentPagination().getPageSize(), new AsyncCallback<Void>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void arg) {
								TreeNode parentNode = getTree().find(FOLDER_ID, "" + FolderCursor.get().getFolderId());
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
					new AsyncCallback<GUIFolder>() {

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

	private void openAllParents(TreeNode node) {
		TreeNode parent = getTree().getParent(node);
		while (parent != null && !node.equals(parent)) {
			getTree().openFolder(parent);
			parent = getTree().getParent(parent);
		}
	}

	private void onDataArrivedFirstTime() {
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
					new AsyncCallback<GUIFolder>() {

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

	private void resetPathToOpen(GUIFolder[] path) {
		pathToOpen = path;
		currentIndexInPathToOpen = 0;
	}

	private boolean isPaginationEnabled() {
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
				new AsyncCallback<GUIFolder>() {

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
						node.setAttribute(Constants.PERMISSION_ADD,
								Boolean.toString(folder.hasPermission(Constants.PERMISSION_ADD)));
						node.setAttribute(Constants.PERMISSION_DELETE,
								Boolean.toString(folder.hasPermission(Constants.PERMISSION_DELETE)));
						node.setAttribute(Constants.PERMISSION_RENAME,
								Boolean.toString(folder.hasPermission(Constants.PERMISSION_RENAME)));
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
			node.setAttribute(Constants.PERMISSION_ADD, fld.hasPermission(Constants.PERMISSION_ADD));
			node.setAttribute(Constants.PERMISSION_DELETE, fld.hasPermission(Constants.PERMISSION_DELETE));
			node.setAttribute(Constants.PERMISSION_RENAME, fld.hasPermission(Constants.PERMISSION_RENAME));

			getTree().add(node, parent);
			parent = node;
		}
		return parent;
	}

	public String getNodePath(TreeNode leafNode) {
		String path = "";
		if (leafNode == null)
			return path;

		TreeNode[] parents = getTree().getParents(leafNode);
		if (parents != null && parents.length > 0)
			for (int i = parents.length - 1; i >= 0; i--) {
				if (parents[i].getName() != null && !"/".equals(parents[i].getName()))
					path += "/" + parents[i].getName();
			}
		path += "/" + (leafNode.getName().equals("/") ? "" : leafNode.getName());
		return path;
	}

	public String getCurrentPath() {
		return getNodePath(getSelectedRecord());
	}

	public String getPath(long folderId) {
		TreeNode selectedNode = getTree().find(FOLDER_ID, Long.toString(folderId));
		return getNodePath(selectedNode);
	}

	/**
	 * Reloads the children of the current node and also re-select the current
	 * folder
	 */
	public void reload() {
		TreeNode selectedNode = (TreeNode) getSelectedRecord();
		getTree().reloadChildren(selectedNode);
		selectFolder(selectedNode.getAttributeAsLong(FOLDER_ID));
	}

	/**
	 * Reloads the children of the current node
	 */
	public void reloadChildren() {
		TreeNode selectedNode = (TreeNode) getSelectedRecord();
		getTree().reloadChildren(selectedNode);
	}

	void onCreate(long parentId) {
		GUIFolder folder = new GUIFolder();
		folder.setParentId(parentId);
		CreateDialog dialog = new CreateDialog(folder);
		dialog.show();
	}

	private void onRename() {
		final TreeNode selectedNode = (TreeNode) getSelectedRecord();
		LD.askForValue(I18N.message("rename"), I18N.message("name"), selectedNode.getAttributeAsString("name"),
				new ValueCallback() {
					@Override
					public void execute(final String value) {
						if (value == null || "".equals(value.trim()))
							return;
						final String val = value.trim().replace("/", "").replace("\\\\", "");
						final long folderId = Long.parseLong(selectedNode.getAttributeAsString(FOLDER_ID));
						FolderService.Instance.get().rename(folderId, val, new AsyncCallback<Void>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void v) {
								selectedNode.setAttribute("name", val);
								refreshRow(getRecordIndex(selectedNode));
								selectFolder(folderId);
							}
						});
					}
				});
	}

	void onCreateWorkspace() {
		GUIFolder folder = new GUIFolder();
		folder.setType(1);
		CreateDialog dialog = new CreateDialog(folder);
		dialog.show();
	}

	private void onPaste() {
		TreeNode selectedNode = (TreeNode) getSelectedRecord();
		final long folderId = Long.parseLong(selectedNode.getAttribute(FOLDER_ID));
		final long[] docIds = new long[Clipboard.getInstance().size()];
		int i = 0;
		for (GUIDocument doc : Clipboard.getInstance()) {
			docIds[i++] = doc.getId();
		}

		FolderService.Instance.get().paste(docIds, folderId, Clipboard.getInstance().getLastAction(),
				new AsyncCallback<Void>() {

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

	private void onPasteAsAlias() {
		TreeNode selectedNode = (TreeNode) getSelectedRecord();
		final long folderId = Long.parseLong(selectedNode.getAttribute(FOLDER_ID));
		final long[] docIds = new long[Clipboard.getInstance().size()];
		int i = 0;
		for (GUIDocument doc : Clipboard.getInstance())
			docIds[i++] = doc.getId();

		if (Feature.enabled(Feature.PDF))
			LD.askForValue(I18N.message("pasteasalias"), "type", "", ItemFactory.newAliasTypeSelector(),
					new ValueCallback() {

						@Override
						public void execute(String type) {
							pasteAsAlias(folderId, docIds, type);
						}
					});
		else
			pasteAsAlias(folderId, docIds, null);
	}

	private void pasteAsAlias(final long folderId, final long[] docIds, String type) {
		FolderService.Instance.get().pasteAsAlias(docIds, folderId, type, new AsyncCallback<Void>() {

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
	public long[] getSelectedIds() {
		ListGridRecord[] selection = getSelectedRecords();
		List<Long> ids = new ArrayList<>();
		for (ListGridRecord rec : selection)
			ids.add(Long.parseLong(rec.getAttributeAsString(FOLDER_ID)));
		long[] idsArray = new long[ids.size()];
		for (int i = 0; i < idsArray.length; i++) {
			idsArray[i] = ids.get(i);
		}
		return idsArray;
	}

	/**
	 * Moves the currently selected folder to the new parent folder
	 * 
	 * @param targetFolderId The parent folder
	 */
	public void moveTo(final long targetFolderId) {
		long[] ids = getSelectedIds();
		for (long id : ids) {
			TreeNode node = getTree().find(FOLDER_ID, (Object) id);
			getTree().remove(node);
		}

		LD.contactingServer();
		FolderService.Instance.get().move(ids, targetFolderId, new AsyncCallback<Void>() {

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
		long[] ids = getSelectedIds();
		for (long id : ids) {
			TreeNode node = getTree().find(FOLDER_ID, (Object) id);
			getTree().remove(node);
		}

		LD.contactingServer();
		FolderService.Instance.get().merge(ids, targetFolderId, new AsyncCallback<Void>() {

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
				new AsyncCallback<Void>() {

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
				new AsyncCallback<GUIFolder>() {

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
	protected String getCellCSSText(ListGridRecord rec, int rowNum, int colNum) {
		if ("1".equals(rec.getAttribute("type"))) {
			return "font-weight:bold;";
		} else
			return super.getCellCSSText(rec, rowNum, colNum);
	}

	@Override
	protected String getIcon(Record rec, boolean defaultState) {
		return "blank.gif";
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
		if (parent.getAttributeAsLong(FOLDER_ID) != null) {
			if (folder.getId() == FolderController.get().getCurrentFolder().getId())
				selectFolder(parent.getAttributeAsLong(FOLDER_ID));
		}

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
		LD.askForValue(I18N.message("warning"), I18N.message("archiveadvice"), "", 400, new ValueCallback() {

			@Override
			public void execute(String value) {
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
			}
		});
	}

	private void onAutomation(final long folderId) {
		AutomationDialog dialog = new AutomationDialog(folderId, null);
		dialog.show();
	}

	public TreeNode getNode(long folderId) {
		TreeNode node = getTree().find(FOLDER_ID, Long.toString(folderId));
		return node;
	}

	public long getSelectedFolderId() {
		return getSelectedRecord().getAttributeAsLong(FOLDER_ID);
	}

	public TreeNode getRootNode() {
		TreeNode node = getTree().getRoot();
		return node;
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
			} catch (Throwable t) {
				// Nothing to do
			}
		}
	}

	@Override
	public void destroy() {
		FolderController.get().removeObserver(this);
	}

	@Override
	protected void onUnload() {
		destroy();
		super.onUnload();
	}

	@Override
	protected void onDestroy() {
		destroy();
		super.onDestroy();
	}

	private MenuItem prepareCustomActionsMenu(final long folderId) {
		Menu customActionsMenu = new Menu();
		if (Session.get().getUser().getCustomActions() != null
				&& Session.get().getUser().getCustomActions().length > 0) {
			for (GUIMenu menuAction : Session.get().getUser().getCustomActions()) {
				prepareCustomActionMenuItem(folderId, menuAction, customActionsMenu);
			}
		}

		MenuItem customActionsItem = new MenuItem(I18N.message("customactions"));
		customActionsItem.setSubmenu(customActionsMenu);
		return customActionsItem;
	}

	private void prepareCustomActionMenuItem(final long folderId, GUIMenu menuAction, Menu customActionsMenu) {
		MenuItem actionItem = new MenuItem(I18N.message(menuAction.getName()));
		customActionsMenu.addItem(actionItem);

		actionItem.addClickHandler((MenuItemClickEvent event) -> {
			/**
			 * Check on the server if the action has been modified
			 */
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
						 * An automation cript is specified directly, so launch
						 * it's execution
						 */
						GUIAutomationRoutine routine = new GUIAutomationRoutine();
						routine.setAutomation(action.getAutomation());
						executeRoutine(folderId, null, routine);
					} else if (action.getRoutineId() != null && action.getRoutineId().longValue() != 0L) {
						AutomationService.Instance.get().getRoutine(action.getRoutineId(),
								new AsyncCallback<GUIAutomationRoutine>() {

									@Override
									public void onFailure(Throwable caught) {
										GuiLog.serverError(caught);
									}

									@Override
									public void onSuccess(GUIAutomationRoutine routine) {
										if (routine.getTemplateId() != null
												&& routine.getTemplateId().longValue() != 0L) {
											/*
											 * A routine with parameters is
											 * referenced, so open the input
											 * popup
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
			});
		});
	}

	private void executeRoutine(long folderId, long[] docIds, GUIAutomationRoutine routine) {
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
}