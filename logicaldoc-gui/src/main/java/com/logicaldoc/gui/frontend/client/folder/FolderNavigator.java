package com.logicaldoc.gui.frontend.client.folder;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.controllers.FolderObserver;
import com.logicaldoc.gui.common.client.data.FoldersDS;
import com.logicaldoc.gui.common.client.grid.FolderListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.RequestInfo;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsGrid;
import com.logicaldoc.gui.frontend.client.folder.browser.FolderCursor;
import com.logicaldoc.gui.frontend.client.folder.browser.FolderTree;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.util.EventHandler;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.tree.TreeNode;

/**
 * The panel that shows the workspaces/folders navigation tree
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class FolderNavigator extends FolderTree implements FolderObserver {

	private static final String POSITION = "position";

	private static final String OPERATIONNOTALLOWED = "operationnotallowed";

	private static FolderNavigator instance = new FolderNavigator();

	private boolean firstTime = true;

	// Indicates if the Navigator is in the process of opening this path
	private GUIFolder[] pathToOpen = null;

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
			} else if (EventHandler.getDragTarget() instanceof DocumentsGrid grid) {
				/*
				 * In this case we are moving documents
				 */
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
			showContextMenu();
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
						folderId, "cut", false, false, false, new DefaultAsyncCallback<>() {
							@Override
							public void onFailure(Throwable caught) {
								super.onFailure(caught);
								GuiLog.warn(I18N.message(OPERATIONNOTALLOWED), null);
							}

							@Override
							public void handleSuccess(Void result) {
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

				FolderService.Instance.get().move(source, target, new DefaultAsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						super.onFailure(caught);
						GuiLog.warn(I18N.message(OPERATIONNOTALLOWED), null);
					}

					@Override
					public void handleSuccess(Void ret) {
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
				new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(GUIFolder result) {
						if (result != null) {
							result.setPathExtended(getPath(folderId));
							FolderController.get().selected(result);
						}
					}
				});
	}

	/**
	 * Shows the context menu
	 * 
	 * @return the prepared context menu
	 */
	private void showContextMenu() {
		FolderService.Instance.get().getAllowedPermissions(getSelectedIds(), new DefaultAsyncCallback<>() {
			@Override
			public void handleSuccess(GUIAccessControlEntry acl) {
				new ContextMenu(FolderNavigator.this, acl).showContextMenu();
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
			if (Session.get().getTenantId() == Session.get().getUser().getTenant().getTenantId()
					&& Session.get().getUser().getDefaultWorkspace() != null) {
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
					new DefaultAsyncCallback<>() {
						@Override
						public void handleSuccess(GUIFolder folder) {
							selectFolder(folder.getId());
						}
					});
		}
	}

	void resetPathToOpen(GUIFolder[] path) {
		pathToOpen = path;
		currentIndexInPathToOpen = 0;
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

	public String getCurrentPath() {
		return getNodePath(getSelectedRecord());
	}

	void onCreate(long parentId) {
		GUIFolder folder = new GUIFolder();
		folder.setParentId(parentId);
		CreateDialog dialog = new CreateDialog(folder);
		dialog.show();
	}

	void onCreateWorkspace() {
		GUIFolder folder = new GUIFolder();
		folder.setType(1);
		CreateDialog dialog = new CreateDialog(folder);
		dialog.show();
	}

	@Override
	public void enable() {
		super.enable();
		getTree().setReportCollisions(false);
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
		FolderService.Instance.get().move(ids, targetFolderId, new DefaultAsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				super.onFailure(caught);
				GuiLog.warn(I18N.message(OPERATIONNOTALLOWED), null);
			}

			@Override
			public void handleSuccess(Void ret) {
				TreeNode target = getTree().find(FOLDER_ID, Long.toString(targetFolderId));
				if (target != null)
					getTree().reloadChildren(target);
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
		if (parent == null && folder.getType() == GUIFolder.TYPE_WORKSPACE)
			parent = getTree().getRoot();

		if (parent == null)
			return;

		TreeNode node = new TreeNode();
		node.setTitle(folder.getName());
		node.setName(folder.getName());
		node.setAttribute(COLOR, folder.getColor());
		node.setAttribute(FOLDER_ID, folder.getId());
		node.setAttribute(PARENT_ID, folder.getParentId());
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
				DocumentService.Instance.get().archiveFolder(folderId, value, new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(Long result) {
						GuiLog.info(I18N.message("documentswerearchived", "" + result), null);
						reload();
					}
				});
		});
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

	void reloadParentsOfSelection() {
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
						new DefaultAsyncCallback<>() {
							@Override
							public void handleSuccess(Void arg) {
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
					new DefaultAsyncCallback<>() {
						@Override
						public void handleSuccess(GUIFolder folder) {
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

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}