package com.logicaldoc.gui.frontend.client.folder.browser;

import java.util.Date;

import com.google.gwt.core.client.Scheduler;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.FoldersDS;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.widgets.grid.FolderListGridField;
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

	protected static final String FOLDER_ID = "folderId";

	protected static final String OPENED = "opened";

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
					new AsyncCallback<>() {
						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(GUIFolder folder) {
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
	 * Select the specified folder
	 * 
	 * @param folderId the folder's identifier
	 */
	public void selectFolder(final long folderId) {
		FolderService.Instance.get().getFolder(folderId, false, false, Session.get().isFolderPagination(),
				new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIFolder result) {
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
}