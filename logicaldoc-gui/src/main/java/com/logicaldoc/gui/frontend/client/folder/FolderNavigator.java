package com.logicaldoc.gui.frontend.client.folder;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.RepeatingCommand;
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
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.EventHandler;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.events.DragStartEvent;
import com.smartgwt.client.widgets.events.DragStartHandler;
import com.smartgwt.client.widgets.events.DropEvent;
import com.smartgwt.client.widgets.events.DropHandler;
import com.smartgwt.client.widgets.events.KeyDownEvent;
import com.smartgwt.client.widgets.events.KeyDownHandler;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellClickEvent;
import com.smartgwt.client.widgets.grid.events.CellClickHandler;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.ClickHandler;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.tree.TreeGrid;
import com.smartgwt.client.widgets.tree.TreeNode;
import com.smartgwt.client.widgets.tree.events.FolderClosedEvent;
import com.smartgwt.client.widgets.tree.events.FolderClosedHandler;
import com.smartgwt.client.widgets.tree.events.FolderOpenedEvent;
import com.smartgwt.client.widgets.tree.events.FolderOpenedHandler;

/**
 * The panel that shows the workspaces/folders navigation tree
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class FolderNavigator extends TreeGrid implements FolderObserver {

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

		FolderController.get().addObserver(this);

		addDragStartHandler(new DragStartHandler() {

			@Override
			public void onDragStart(DragStartEvent ev) {
				if (EventHandler.getDragTarget() instanceof FolderNavigator) {
					// Workspaces cannot be moved
					if ("1".equals(getDragData()[0].getAttributeAsString("type"))) {
						ev.cancel();
						return;
					}
				}
			}

		});

		addDropHandler(new DropHandler() {
			public void onDrop(final DropEvent event) {
				try {
					if (EventHandler.getDragTarget() instanceof FolderNavigator) {
						// Workspaces cannot be moved
						if ("1".equals(getDragData()[0].getAttributeAsString("type"))) {
							event.cancel();
							return;
						}

						final long[] source = getSelectedIds();
						final long target = Long.parseLong(getDropFolder().getAttributeAsString("folderId"));

						final String sourceName = getDragData()[0].getAttributeAsString("name");
						final String targetName = getDropFolder().getAttributeAsString("name");

						LD.ask(I18N.message("move"), I18N.message("moveask", new String[] { sourceName, targetName }),
								new BooleanCallback() {

									@Override
									public void execute(Boolean value) {
										if (value) {
											for (long id : source) {
												TreeNode node = getTree().find("folderId", (Object) id);
												getTree().remove(node);
											}

											FolderService.Instance.get().move(source, target,
													new AsyncCallback<Void>() {
														@Override
														public void onFailure(Throwable caught) {
															GuiLog.serverError(caught);
															GuiLog.warn(I18N.message("operationnotallowed"), null);
														}

														@Override
														public void onSuccess(Void ret) {
															TreeNode targetNode = getTree().find("folderId",
																	(Object) Long.valueOf(target));
															if (targetNode != null)
																getTree().reloadChildren(targetNode);
														}
													});
										}
									}
								});
					} else if (EventHandler.getDragTarget() instanceof DocumentsGrid) {
						/*
						 * In this case we are moving a document
						 */
						DocumentsGrid grid = (DocumentsGrid) EventHandler.getDragTarget();
						final GUIDocument[] selection = grid.getSelectedDocuments();
						if (selection == null || selection.length == 0)
							return;
						final long[] ids = new long[selection.length];
						for (int i = 0; i < selection.length; i++)
							ids[i] = selection[i].getId();

						final TreeNode selectedNode = getDropFolder();
						final long folderId = Long.parseLong(selectedNode.getAttribute("folderId"));

						if (FolderController.get().getCurrentFolder().getId() == folderId)
							return;

						final String sourceName = selection.length == 1 ? selection[0].getFileName()
								: (selection.length + " " + I18N.message("documents").toLowerCase());
						final String targetName = selectedNode.getAttributeAsString("name");

						LD.ask(I18N.message("move"), I18N.message("moveask", new String[] { sourceName, targetName }),
								new BooleanCallback() {

									@Override
									public void execute(Boolean value) {
										if (value) {
											FolderService.Instance.get().paste(ids, folderId, "cut",
													new AsyncCallback<Void>() {
														@Override
														public void onFailure(Throwable caught) {
															GuiLog.serverError(caught);
															GuiLog.warn(I18N.message("operationnotallowed"), null);
														}

														@Override
														public void onSuccess(Void result) {
															DocumentsPanel.get().onFolderSelected(
																	FolderController.get().getCurrentFolder());
															GuiLog.debug("Drag&Drop operation completed.");
														}
													});
										}

										TreeNode node = getTree().find("folderId", (Object) Long.valueOf(folderId));
										if (node != null) {
											getTree().reloadChildren(node);
										}
									}
								});
					}
				} catch (Throwable e) {
				}
			}
		});

		ListGridField name = new FolderListGridField();
		setFields(name);

		// Handles the body click on folder name to create the context menu
		addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				if (getSelectedRecord() != null && getSelectedRecords().length > 1) {
					Menu contextMenu = prepateContextMenu();
					contextMenu.showContextMenu();
				} else {
					FolderService.Instance.get().getFolder(getSelectedRecord().getAttributeAsLong("folderId"), false,
							true, false, new AsyncCallback<GUIFolder>() {

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
				if (event != null)
					event.cancel();
			}
		});

		// Handles the click on a folder to show the contained documents
		addCellClickHandler(new CellClickHandler() {
			@Override
			public void onCellClick(CellClickEvent event) {
				long selectedFolderId = event.getRecord().getAttributeAsLong("folderId");

				selectFolder(selectedFolderId);

				if(Session.get().getConfigAsBoolean("gui.folder.openonselect")) {
					// Expand the selected node if it is not already expanded
					TreeNode selectedNode = (TreeNode) getSelectedRecord();
					openFolder(selectedNode);
				}
			}
		});


		/*
		 * To refresh the folder's decoration and in case of need we add a
		 * listener that closes all the previously opened branches.
		 */
		addFolderOpenedHandler(new FolderOpenedHandler() {

			@Override
			public void onFolderOpened(FolderOpenedEvent event) {
				event.getNode().setAttribute("opened", true);
				updateData(event.getNode());

				if (Session.get().getConfigAsBoolean("gui.folder.autoclose")) {
					TreeNode selectedNode = event.getNode();
					long selectedFolderId = selectedNode.getAttributeAsLong("folderId");

					// Expand the selected node if it is not already expanded
					TreeNode parentNode = getTree().getParent(selectedNode);
					TreeNode[] children = getTree().getChildren(parentNode);
					if (children != null)
						for (TreeNode child : children) {
							if (child.getAttributeAsLong("folderId") == selectedFolderId)
								continue;
							else {
								getTree().closeAll(child);
								child.setAttribute("opened", false);
								updateData(child);
							}
						}
				}
			}

		});

		/*
		 * To refresh the folder's decoration
		 */
		addFolderClosedHandler(new FolderClosedHandler() {

			@Override
			public void onFolderClosed(FolderClosedEvent event) {
				event.getNode().setAttribute("opened", false);
				updateData(event.getNode());
			}
		});

		// Used to expand root folder after login or to open in folder
		addDataArrivedHandler(new DataArrivedHandler() {
			@Override
			public void onDataArrived(DataArrivedEvent event) {
				FolderNavigator.this.onDataArrived(event);
			}
		});

		FolderController.get().addObserver(this);

		FolderCursor.get().registerMaxChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				reloadChildren();
			}
		});

		FolderCursor.get().registerPageChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				reloadChildren();
			}
		});

		/*
		 * A listener of the keypress to automatically collect a string typed by
		 * the user to quickly select the folder
		 */
		addKeyDownHandler(new KeyDownHandler() {

			@Override
			public void onKeyDown(KeyDownEvent event) {
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
			}
		});

		/*
		 * Timer that selects the typed folder once the user stops typing
		 */
		Scheduler.get().scheduleFixedDelay(new RepeatingCommand() {

			@Override
			public boolean execute() {
				Date now = new Date();
				if (lastTyped != null && typed != null) {
					long dist = now.getTime() - lastTyped.getTime();
					if (dist > 500) {
						AdvancedCriteria crit = new AdvancedCriteria("name", OperatorId.ISTARTS_WITH, typed);
						Record record = find(crit);
						if (record != null) {
							final Long folderId = record.getAttributeAsLong("folderId");
							deselectAllRecords();
							selectRecord(record);
							selectFolder(folderId);
						}

						typed = null;
						lastTyped = null;
					}
				}
				return true;
			}
		}, 500);
	}

	protected String getOriginalIcon(Record record, boolean defaultState) {
		return super.getIcon(record, defaultState);
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
		move.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				MoveDialog dialog = new MoveDialog();
				dialog.show();
			}
		});

		MenuItem copy = new MenuItem();
		copy.setTitle(I18N.message("copy"));
		copy.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				FolderCopyDialog dialog = new FolderCopyDialog();
				dialog.show();
			}
		});

		MenuItem merge = new MenuItem();
		merge.setTitle(I18N.message("merge"));
		merge.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				MergeDialog dialog = new MergeDialog();
				dialog.show();
			}
		});

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				onDelete();
			}
		});

		Menu contextMenu = new Menu();
		contextMenu.setItems(move, copy, merge, delete);

		return contextMenu;
	}

	/**
	 * Prepares the context menu for single selection
	 * 
	 * @param folder the folder to use
	 */
	private Menu prepateContextMenu(final GUIFolder folder) {
		final TreeNode selectedNode = (TreeNode) getSelectedRecord();
		final long id = Long.parseLong(selectedNode.getAttribute("folderId"));
		final String name = selectedNode.getAttribute("name");

		MenuItem search = new MenuItem();
		search.setTitle(I18N.message("search"));
		search.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				Search.get().getOptions().setFolder(id);
				Search.get().getOptions().setFolderName(name);
				Search.get().getOptions().setSearchInSubPath(false);
				Search.get().setOptions(Search.get().getOptions());
				MainPanel.get().selectSearchTab();
			}
		});

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				onDelete();
			}
		});

		MenuItem create = new MenuItem();
		create.setTitle(I18N.message("newfolder"));
		create.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				onCreate(folder.getId());
			}
		});

		MenuItem createAlias = new MenuItem();
		createAlias.setTitle(I18N.message("createalias"));
		createAlias.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				CreateAliasDialog dialog = new CreateAliasDialog();
				dialog.show();
			}
		});
		createAlias.setEnabled(getSelectedRecord().getAttributeAsString("foldRef") == null);

		MenuItem rename = new MenuItem();
		rename.setTitle(I18N.message("rename"));
		rename.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				onRename();
			}
		});

		MenuItem createWorkspace = new MenuItem();
		createWorkspace.setTitle(I18N.message("newworkspace"));
		createWorkspace.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				onCreateWorkspace();
			}
		});

		MenuItem applyTemplate = new MenuItem();
		applyTemplate.setTitle(I18N.message("applytemplate"));
		applyTemplate.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				onApplyTemplate();
			}
		});

		MenuItem archive = new MenuItem();
		archive.setTitle(I18N.message("archive"));
		archive.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				onArchive(folder.getId());
			}
		});

		MenuItem reload = new MenuItem();
		reload.setTitle(I18N.message("reload"));
		reload.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				reload();
			}
		});

		MenuItem move = new MenuItem();
		move.setTitle(I18N.message("move"));
		move.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				MoveDialog dialog = new MoveDialog();
				dialog.show();
			}
		});

		MenuItem copy = new MenuItem();
		copy.setTitle(I18N.message("copy"));
		copy.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				FolderCopyDialog dialog = new FolderCopyDialog();
				dialog.show();
			}
		});

		MenuItem merge = new MenuItem();
		merge.setTitle(I18N.message("merge"));
		merge.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				MergeDialog dialog = new MergeDialog();
				dialog.show();
			}
		});

		MenuItem paste = new MenuItem();
		paste.setTitle(I18N.message("paste"));
		paste.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				onPaste();
			}
		});

		MenuItem pasteAsAlias = new MenuItem();
		pasteAsAlias.setTitle(I18N.message("pasteasalias"));
		pasteAsAlias.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				onPasteAsAlias();
			}
		});

		MenuItem exportZip = new MenuItem();
		exportZip.setTitle(I18N.message("exportzip"));
		exportZip.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				Window.open(Util.contextPath() + "zip-export?folderId=" + id, "_blank", "");
			}
		});

		MenuItem audit = new MenuItem();
		audit.setTitle(I18N.message("subscribe"));
		audit.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				SubscriptionDialog dialog = new SubscriptionDialog(id, null);
				dialog.show();
			}
		});

		MenuItem addBookmark = new MenuItem();
		addBookmark.setTitle(I18N.message("addbookmark"));
		addBookmark.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				onAddBookmark();
			}
		});

		MenuItem sendToExpArchive = new MenuItem();
		sendToExpArchive.setTitle(I18N.message("sendtoexparchive"));
		sendToExpArchive.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmputinexparchive"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							SendToArchiveDialog archiveDialog = new SendToArchiveDialog(new long[] { id }, false);
							archiveDialog.show();
						}
					}
				});
			}
		});

		MenuItem automation = new MenuItem();
		automation.setTitle(I18N.message("executeautomation"));
		automation.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				onAutomation(folder.getId());
			}
		});

		MenuItem customActionsItem = prepareCustomActionsMenu(folder.getId());

		if (!folder.hasPermission(Constants.PERMISSION_ADD)) {
			create.setEnabled(false);
		}

		if (!folder.hasPermission(Constants.PERMISSION_RENAME)) {
			rename.setEnabled(false);
		}

		if (!folder.hasPermission(Constants.PERMISSION_EXPORT)
				|| !folder.hasPermission(Constants.PERMISSION_DOWNLOAD)) {
			exportZip.setEnabled(false);
		}

		// Avoid alterations of the Default workspace and check DELETE
		// permission
		if ((folder.isDefaultWorkspace() && GUIFolder.TYPE_ALIAS != getSelectedRecord().getAttributeAsInt("type"))
				|| !folder.hasPermission(Constants.PERMISSION_DELETE)) {
			delete.setEnabled(false);
			merge.setEnabled(false);
		}

		// Avoid alterations of the Default workspace and check MOVE permission
		if ((folder.isDefaultWorkspace() && GUIFolder.TYPE_ALIAS != getSelectedRecord().getAttributeAsInt("type"))
				|| !folder.hasPermission(Constants.PERMISSION_DELETE)) {
			move.setEnabled(false);
			merge.setEnabled(false);
		}

		if (folder.isDefaultWorkspace()) {
			rename.setEnabled(false);
		}

		if (!folder.hasPermission(Constants.PERMISSION_WRITE) || Clipboard.getInstance().isEmpty()) {
			paste.setEnabled(false);
			pasteAsAlias.setEnabled(false);
		}

		if (!folder.hasPermission(Constants.PERMISSION_ADD)) {
			createAlias.setEnabled(false);
		}

		if (Clipboard.getInstance().getLastAction().equals(Clipboard.CUT))
			pasteAsAlias.setEnabled(false);

		Menu contextMenu = new Menu();

		if (Session.get().getUser().isMemberOf("admin") && folder.isWorkspace()
				&& Feature.visible(Feature.MULTI_WORKSPACE)) {
			delete.setEnabled(!folder.isDefaultWorkspace());
			move.setEnabled(false);
			merge.setEnabled(false);
			rename.setEnabled(!folder.isDefaultWorkspace());
			createWorkspace.setEnabled(Feature.enabled(Feature.MULTI_WORKSPACE));
			contextMenu.setItems(reload, search, create, createAlias, rename, createWorkspace, delete, addBookmark,
					paste, pasteAsAlias, move, copy, merge, exportZip);
		} else {
			contextMenu.setItems(reload, search, create, createAlias, rename, delete, addBookmark, paste, pasteAsAlias,
					move, copy, merge, exportZip);
		}

		if (Feature.visible(Feature.AUDIT)) {
			contextMenu.addItem(audit);
			audit.setEnabled(Feature.enabled(Feature.AUDIT));
		}

		if (Feature.visible(Feature.FOLDER_TEMPLATE)) {
			contextMenu.addItem(applyTemplate);
			if (!Feature.enabled(Feature.FOLDER_TEMPLATE) || !folder.hasPermission(Constants.PERMISSION_ADD))
				applyTemplate.setEnabled(false);
		}

		if (Feature.visible(Feature.ARCHIVING)) {
			contextMenu.addItem(archive);
			if (!Feature.enabled(Feature.ARCHIVING) || !folder.hasPermission(Constants.PERMISSION_ARCHIVE))
				archive.setEnabled(false);
		}

		if (Feature.visible(Feature.IMPEX)) {
			contextMenu.addItem(sendToExpArchive);
			if (!Feature.enabled(Feature.IMPEX) || !folder.hasPermission(Constants.PERMISSION_EXPORT))
				sendToExpArchive.setEnabled(false);
		}

		if (Feature.visible(Feature.AUTOMATION)) {
			contextMenu.addItem(automation);
			if (!Feature.enabled(Feature.AUTOMATION) || !folder.hasPermission(Constants.PERMISSION_AUTOMATION)) {
				automation.setEnabled(false);
			}
		}

		if (Feature.enabled(Feature.CUSTOM_ACTIONS)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.CUSTOM_ACTIONS)
				&& Session.get().getUser().getCustomActions() != null
				&& Session.get().getUser().getCustomActions().length > 0)
			contextMenu.addItem(customActionsItem);

		return contextMenu;
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
						new BooleanCallback() {
							@Override
							public void execute(Boolean value) {
								if (value) {
									LD.contactingServer();
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
											TreeNode node = getTree().find("folderId",
													(Object) getSelectedRecord().getAttributeAsString("folderId"));
											TreeNode parent = getTree().getParent(node);

											if (parent.getAttributeAsString("folderId") != null)
												selectFolder(Long.parseLong(parent.getAttributeAsString("folderId")));

											reloadParentsOfSelection();
										}
									});
								}
							}
						});
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
		final long folderId = Long.parseLong(selectedNode.getAttributeAsString("folderId"));

		DocumentService.Instance.get().addBookmarks(new long[] { folderId }, 1, new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void v) {
			}
		});
	}

	public static FolderNavigator get() {
		return instance;
	}

	private void onDataArrived(DataArrivedEvent event) {
		if (isFirstTime()) {
			/*
			 * Redirect the user to the correct folder and/or document
			 */
			RequestInfo loc = WindowUtils.getRequestInfo();
			if (loc.getParameter("folderId") != null) {
				DocumentsPanel.get().openInFolder(Long.parseLong(loc.getParameter("folderId")), null);
			} else if (loc.getParameter("docId") != null) {
				DocumentsPanel.get().openInFolder(Long.parseLong(loc.getParameter("docId")));
			} else {
				if (Session.get().getConfigAsBoolean("gui.folder.opentree")) {
					long folderId = 0;

					TreeNode rootNode = getTree().getRoot();
					TreeNode[] children = getTree().getChildren(rootNode);
					TreeNode nodeToOpen = null;

					if (children != null && children.length > 0) {
						/*
						 * Get the first workspace and use it as the node to
						 * open
						 */
						folderId = Long.parseLong(children[0].getAttributeAsString("folderId"));
						nodeToOpen = children[0];

						/*
						 * Check if the user has specified a different default
						 * workspace
						 */
						if (Session.get().getUser().getDefaultWorkspace() != null) {
							folderId = Session.get().getUser().getDefaultWorkspace();
							for (TreeNode child : children) {
								long val = Long.parseLong(child.getAttributeAsString("folderId"));
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
			}

			FolderNavigator.this.firstTime = false;
		} else {
			/*
			 * A folder has been opened, check if we have to handle an
			 * openInFolder task
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

			TreeNode node = getTree().find("folderId", "" + fld.getId());

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
									TreeNode parentNode = getTree().find("folderId",
											"" + FolderCursor.get().getFolderId());
									getTree().reloadChildren(parentNode);
								}
							});
				}
				return;
			}

			if (!getTree().isOpen(node)) {
				getTree().openFolder(node);
				currentIndexInPathToOpen++;
				getTree().reloadChildren(node);
			}

			// Open all the parents
			TreeNode parent = getTree().getParent(node);
			while (parent != null && !node.equals(parent)) {
				getTree().openFolder(parent);
				parent = getTree().getParent(parent);
			}

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
						TreeNode parent = getTree().getRoot();

						long folderId = folder.getId();
						Long folderRef = folder.getFoldRef();
						if (folder.getFoldRef() != null) {
							folderId = folder.getFoldRef();
							folderRef = folder.getId();
						}

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
							node.setAttribute("folderId", Long.toString(fldId));
							node.setAttribute("type", Integer.toString(fld.getType()));
							node.setAttribute("foldRef", fldRef != null ? Long.toString(fldRef) : null);
							if (fld.getColor() != null)
								node.setAttribute("color", fld.getColor());
							node.setAttribute(Constants.PERMISSION_ADD, fld.hasPermission(Constants.PERMISSION_ADD));
							node.setAttribute(Constants.PERMISSION_DELETE,
									fld.hasPermission(Constants.PERMISSION_DELETE));
							node.setAttribute(Constants.PERMISSION_RENAME,
									fld.hasPermission(Constants.PERMISSION_RENAME));

							getTree().add(node, parent);
							parent = node;
						}

						TreeNode node = new TreeNode(folder.getName());
						node.setAttribute("id", parent.getAttributeAsString("id") + "-" + Long.toString(folderId));
						node.setAttribute("folderId", Long.toString(folderId));
						node.setAttribute("type", Integer.toString(folder.getType()));
						node.setAttribute("foldRef", folderRef != null ? Long.toString(folderRef) : null);
						if (folder.getColor() != null)
							node.setAttribute("color", folder.getColor());
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
		TreeNode selectedNode = getTree().find("folderId", Long.toString(folderId));
		return getNodePath(selectedNode);
	}

	/**
	 * Reloads the children of the current node and also re-select the current
	 * folder
	 */
	public void reload() {
		TreeNode selectedNode = (TreeNode) getSelectedRecord();
		getTree().reloadChildren(selectedNode);
		selectFolder(selectedNode.getAttributeAsLong("folderId"));
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
						final String val = value.trim().replaceAll("/", "").replaceAll("\\\\", "");
						final long folderId = Long.parseLong(selectedNode.getAttributeAsString("folderId"));
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
		final long folderId = Long.parseLong(selectedNode.getAttribute("folderId"));
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
		final long folderId = Long.parseLong(selectedNode.getAttribute("folderId"));
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
		List<Long> ids = new ArrayList<Long>();
		for (ListGridRecord record : selection)
			ids.add(Long.parseLong(record.getAttributeAsString("folderId")));
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
			TreeNode node = getTree().find("folderId", (Object) id);
			getTree().remove(node);
		}

		LD.contactingServer();
		FolderService.Instance.get().move(ids, targetFolderId, new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				LD.clearPrompt();
				GuiLog.serverError(caught);
				GuiLog.warn(I18N.message("operationnotallowed"), null);
			}

			@Override
			public void onSuccess(Void ret) {
				LD.clearPrompt();
				TreeNode target = getTree().find("folderId", Long.toString(targetFolderId));
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
			TreeNode node = getTree().find("folderId", (Object) id);
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
				TreeNode target = getTree().find("folderId", Long.toString(targetFolderId));
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
	 * @param securityOption how to setup the security for the new folder'none', 'inherit' or 'replicate'
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

		FolderService.Instance.get().createAlias(parent.getAttributeAsLong("folderId"), referencedFolderId,
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
	protected String getCellCSSText(ListGridRecord record, int rowNum, int colNum) {
		if ("1".equals(record.getAttribute("type"))) {
			return "font-weight:bold;";
		} else
			return super.getCellCSSText(record, rowNum, colNum);
	}

	@Override
	protected String getIcon(Record record, boolean defaultState) {
		return "blank.gif";
	}

	@Override
	public void onFolderChanged(GUIFolder folder) {
		TreeNode folderNode = getTree().find("folderId", Long.toString(folder.getId()));

		if (folderNode != null) {
			folderNode.setTitle(folder.getName());
			folderNode.setName(folder.getName());
			folderNode.setAttribute("color", folder.getColor());
			
			getTree().reloadChildren(folderNode);

			boolean positionChanged = folderNode.getAttributeAsInt("position") != folder.getPosition();

			if (positionChanged) {
				folderNode.setAttribute("position", folder.getPosition());
				TreeNode parentNode = getTree().find("folderId", Long.toString(folder.getParentId()));
				if (parentNode != null)
					getTree().reloadChildren(parentNode);
				else
					getTree().reloadChildren(getTree().getRoot());
			}
		}
	}

	@Override
	public void onFolderBeginEditing(GUIFolder folder) {
	}

	@Override
	public void onFolderCancelEditing(GUIFolder folder) {
	}

	@Override
	public void onFolderSelected(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderCreated(GUIFolder folder) {
		TreeNode parent = getTree().find("folderId", (Object) folder.getParentId());
		if (parent == null)
			return;

		TreeNode node = new TreeNode();
		node.setTitle(folder.getName());
		node.setName(folder.getName());
		node.setAttribute("color", folder.getColor());
		node.setAttribute("folderId", folder.getId());
		node.setAttribute("id", parent.getAttribute("id") + "-" + folder.getId());
		node.setAttribute("type", folder.getType());
		node.setAttribute("customIcon", "folder");
		node.setAttribute("status", 0);
		node.setAttribute("publishedStatus", "yes");
		node.setAttribute("position", folder.getPosition());
		if (folder.getFoldRef() != null) {
			node.setAttribute("foldRef", folder.getFoldRef());
			node.setAttribute("customIcon", "folder_alias");
		}

		getTree().add(node, parent);
	}

	@Override
	public void onFolderDeleted(GUIFolder folder) {
		TreeNode node = getTree().find("folderId", (Object) folder.getId());
		if (node == null)
			return;

		TreeNode parent = getTree().getParent(node);
		if (parent.getAttributeAsLong("folderId") != null) {
			if (folder.getId() == FolderController.get().getCurrentFolder().getId())
				selectFolder(parent.getAttributeAsLong("folderId"));
		}

		getTree().remove(node);
		getTree().reloadChildren(parent);
	}

	@Override
	public void onFolderMoved(GUIFolder folder) {
		TreeNode node = getTree().find("folderId", (Object) folder.getId());
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
		TreeNode node = getTree().find("folderId", Long.toString(folderId));
		return node;
	}

	public long getSelectedFolderId() {
		return getSelectedRecord().getAttributeAsLong("folderId");
	}

	public TreeNode getRootNode() {
		TreeNode node = getTree().getRoot();
		return node;
	}

	private void reloadParentsOfSelection() {
		ListGridRecord[] selection = getSelectedRecords();
		for (ListGridRecord record : selection) {
			try {
				TreeNode node = getTree().find("id", record.getAttributeAsString("id"));
				TreeNode parentNode = getTree().getParent(node);
				if (parentNode != null) {
					getTree().reloadChildren(parentNode);
				} else {
					getTree().reloadChildren(getRootNode());
				}
			} catch (Throwable t) {
			}
		}
	}

	@Override
	public void destroy() {
		FolderController.get().removeObserver(this);
	}

	@Override
	protected void finalize() throws Throwable {
		destroy();
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
				MenuItem actionItem = new MenuItem(I18N.message(menuAction.getName()));
				customActionsMenu.addItem(actionItem);

				actionItem.addClickHandler(new ClickHandler() {

					@Override
					public void onClick(MenuItemClickEvent event) {
						/**
						 * Check on the server if the action has been modified
						 */
						SecurityService.Instance.get().getMenu(menuAction.getId(), I18N.getLocale(),
								new AsyncCallback<GUIMenu>() {

									@Override
									public void onFailure(Throwable caught) {
										GuiLog.serverError(caught);
									}

									@Override
									public void onSuccess(GUIMenu action) {
										Session.get().getUser().updateCustomAction(action);

										if ((action.getRoutineId() == null || action.getRoutineId().longValue() == 0L)
												&& action.getAutomation() != null
												&& !action.getAutomation().trim().isEmpty()) {
											/*
											 * An automation cript is specified
											 * directly, so launch it's
											 * execution
											 */
											GUIAutomationRoutine routine = new GUIAutomationRoutine();
											routine.setAutomation(action.getAutomation());
											executeRoutine(folderId, null, routine);
										} else if (action.getRoutineId() != null
												&& action.getRoutineId().longValue() != 0L) {
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
																 * A routine
																 * with
																 * parameters is
																 * referenced,
																 * so open the
																 * input popup
																 */
																FillRoutineParams dialog = new FillRoutineParams(
																		action.getName(), routine, folderId, null);
																dialog.show();
															} else {
																/*
																 * A routine
																 * without
																 * parameters is
																 * referenced,
																 * so launch
																 * directly
																 */
																executeRoutine(folderId, null, routine);
															}
														}
													});
										}
									}
								});
					}
				});
			}
		}

		MenuItem customActionsItem = new MenuItem(I18N.message("customactions"));
		customActionsItem.setSubmenu(customActionsMenu);
		return customActionsItem;
	}

	private void executeRoutine(long folderId, Long[] docIds, GUIAutomationRoutine routine) {
		AutomationService.Instance.get().execute(routine, docIds, folderId, new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg0) {
			}
		});
	}
}