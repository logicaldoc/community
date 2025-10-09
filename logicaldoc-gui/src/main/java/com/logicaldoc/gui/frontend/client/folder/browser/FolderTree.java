package com.logicaldoc.gui.frontend.client.folder.browser;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.google.gwt.core.client.Scheduler;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.data.FoldersDS;
import com.logicaldoc.gui.common.client.grid.FolderListGridField;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentGridUtil;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.util.EventHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.tree.TreeGrid;
import com.smartgwt.client.widgets.tree.TreeNode;

/**
 * A folders tree
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
public class FolderTree extends TreeGrid {

	public static final String FOLDER_ID = "folderId";

	protected static final String OPENED = "opened";

	public static final String PARENT_ID = "parentId";

	public static final String FOLD_REF = "foldRef";

	protected static final String COLOR = "color";

	/**
	 * String typed by the user inside the tree, to quickly select folders
	 */
	protected String typed;

	/**
	 * When the user typed last time
	 */
	protected Date lastTyped;

	/**
	 * The folder cursor to use for this browser
	 */
	protected FolderCursor cursor;

	public FolderTree() {
		this(null);
	}

	public FolderTree(FolderCursor cursor) {
		super();

		this.cursor = cursor;

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

		addFolderOpenedHandler();

		/*
		 * To refresh the folder's decoration
		 */
		addFolderClosedHandler(event -> {
			event.getNode().setAttribute(OPENED, false);
			updateData(event.getNode());
		});

		if (cursor != null) {
			cursor.registerMaxChangedHandler(event -> reloadChildren());

			cursor.registerPageChangedHandler(event -> reloadChildren());

			addCellClickHandler(event ->

			FolderService.Instance.get().getFolder(getSelectedFolderId(), false, false, true,
					new DefaultAsyncCallback<>() {
						@Override
						public void handleSuccess(GUIFolder folder) {
							cursor.onFolderSelected(folder);
						}
					}));
		}

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

	/**
	 * Refreshes the folder's decoration and in case of need we add a listener
	 * that closes all the previously opened branches.
	 */
	protected void addFolderOpenedHandler() {
		addFolderOpenedHandler(event -> {
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
		});
	}

	protected String getOriginalIcon(Record rec, boolean defaultState) {
		return this.getIcon(rec, defaultState);
	}

	/**
	 * Reloads the children of the current node
	 */
	public void reloadChildren() {
		TreeNode selectedNode = getSelectedRecord();
		getTree().reloadChildren(selectedNode);
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
	protected void onUnload() {
		destroy();
		super.onUnload();
	}

	@Override
	protected void onDestroy() {
		destroy();
		super.onDestroy();
	}

	/**
	 * Installs a Timer that selects the typed folder once the user stops typing
	 */
	protected void installTimer() {
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

	public long getSelectedFolderId() {
		return getSelectedRecord().getAttributeAsLong(FOLDER_ID);
	}

	/**
	 * Gets all the IDs of the selected folders
	 * 
	 * @return identifiers of folders
	 */
	public List<Long> getSelectedIds() {
		ListGridRecord[] selection = getSelectedRecords();
		List<Long> ids = new ArrayList<>();
		for (ListGridRecord rec : selection) {
			ids.add(rec.getAttributeAsLong(FOLDER_ID));
		}
		return ids;
	}

	/**
	 * Gets all the selected folders
	 * 
	 * @return list of folders
	 */
	public List<GUIFolder> getSelectedFolders() {
		ListGridRecord[] selection = getSelectedRecords();
		List<GUIFolder> folders = new ArrayList<>();
		for (ListGridRecord rec : selection) {
			GUIFolder folder = new GUIFolder();
			folder.setId(rec.getAttributeAsLong(FOLDER_ID));
			folder.setName(rec.getAttributeAsString("name"));
			folder.setParentId(rec.getAttributeAsLong(PARENT_ID));
			folders.add(folder);
		}
		return folders;
	}

	/**
	 * Select the specified folder
	 * 
	 * @param folderId the folder's identifier
	 */
	public void selectFolder(final long folderId) {
		FolderService.Instance.get().getFolder(folderId, false, false, Session.get().isFolderPagination(),
				new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(GUIFolder result) {
						if (result != null) {
							result.setPathExtended(getPath(folderId));
							if (Session.get().isFolderPagination()) {
								if (result.getGrid() != null && !result.getGrid().isEmpty())
									cursor.setPageSizeAndTotalRecords(
											DocumentGridUtil.getFolderPageSizeFromSpec(result.getGrid()),
											(int) result.getSubfolderCount());
								else if (Session.get().getUser().getDocsGrid() != null
										&& !Session.get().getUser().getDocsGrid().isEmpty())
									cursor.setPageSizeAndTotalRecords(
											DocumentGridUtil
													.getFolderPageSizeFromSpec(Session.get().getUser().getDocsGrid()),
											(int) result.getSubfolderCount());
								else
									cursor.setTotalRecords((int) result.getSubfolderCount());
							}
						}
					}
				});
	}

	public String getPath(long folderId) {
		TreeNode selectedNode = getTree().find(FOLDER_ID, Long.toString(folderId));
		return getNodePath(selectedNode);
	}

	public String getNodePath(TreeNode leafNode) {
		StringBuilder path = new StringBuilder();
		if (leafNode == null)
			return path.toString();

		TreeNode[] parents = getTree().getParents(leafNode);
		if (parents != null && parents.length > 0)
			for (int i = parents.length - 1; i >= 0; i--) {
				if (parents[i].getName() != null && !"/".equals(parents[i].getName())) {
					path.append("/");
					path.append(parents[i].getName());
				}
			}

		path.append("/");
		path.append(leafNode.getName().equals("/") ? "" : leafNode.getName());
		return path.toString();
	}

	/**
	 * Opens the branch to show the specified folder, it cycles the path
	 * creating minimal tree nodes. At the end the folder is also selected as
	 * currently selected folder
	 * 
	 * @param folderId identifier of the folder to open
	 */
	public void openFolder(long folderId) {
		openFolder(folderId, true);
	}

	/**
	 * Opens the branch to show the specified folder, it cycles the path
	 * creating minimal tree nodes
	 * 
	 * @param folderId identifier of the folder to open
	 * @param select if the open node must also be selected as currently
	 *        selected folder
	 */
	public void openFolder(long folderId, boolean select) {
		getTree().closeAll();

		FolderService.Instance.get().getFolder(folderId, true, true, isPaginationEnabled(),
				new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(GUIFolder folder) {
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
						node.setAttribute(PARENT_ID, Long.toString(folder.getParentId()));
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

						if (select)
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
			node.setAttribute(PARENT_ID, Long.toString(fld.getParentId()));
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

	protected boolean isPaginationEnabled() {
		return Session.get().getConfigAsBoolean("gui.folder.pagination");
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