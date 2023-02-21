package com.logicaldoc.gui.frontend.client.folder;

import java.util.HashMap;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.observer.FolderController;
import com.logicaldoc.gui.common.client.observer.FolderObserver;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SpacerItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;

/**
 * A cursor to browse among pages in a folders tree. If pagination is enabled.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.1
 */
public class FolderCursor extends DynamicForm implements FolderObserver {

	private static final String GUI_FOLDER_MAXCHILDREN = "gui.folder.maxchildren";

	private static FolderCursor instance = null;

	private SpinnerItem maxItem;

	private SpinnerItem pageItem;

	private FolderPagination currentPagination = new FolderPagination(0L, 1000, 0, 1);

	private Map<Long, FolderPagination> paginations = new HashMap<>();

	public static FolderCursor get() {
		if (instance == null)
			instance = new FolderCursor();
		return instance;
	}

	private FolderCursor() {
		setNumCols(3);
		setHeight(1);
		setAlign(Alignment.RIGHT);

		maxItem = ItemFactory.newSpinnerItem("max", "display", Session.get().getConfigAsInt(GUI_FOLDER_MAXCHILDREN),
				2, (Integer) null);
		maxItem.setWidth(60);
		maxItem.setStep(20);
		maxItem.setSaveOnEnter(true);
		maxItem.setImplicitSave(true);
		maxItem.setShowTitle(false);
		maxItem.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				onMaxChange();
			}
		});

		pageItem = ItemFactory.newSpinnerItem("page", "page", 1, 1, 1);
		pageItem.setHint("");
		pageItem.setSaveOnEnter(true);
		pageItem.setImplicitSave(true);
		pageItem.setShowTitle(false);
		pageItem.setWidth(45);
		pageItem.setMinHintWidth(1);
		pageItem.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				onPageChange();
			}
		});
		pageItem.setHint("/" + (currentPagination.getTotalPages() > 0 ? currentPagination.getTotalPages() : 1));

		SpacerItem spacer2 = new SpacerItem("spc2");
		spacer2.setWidth(6);
		spacer2.setTitle("|");

		setItems(maxItem, spacer2, pageItem);

		FolderController.get().addObserver(this);
	}

	public void setPageSizeAndTotalRecords(int pageSize, int totalRecords) {
		currentPagination.setTotalElements(totalRecords);
		currentPagination.setPageSize(pageSize);
		update();
	}

	public void setTotalRecords(int totalRecords) {
		currentPagination.setTotalElements(totalRecords);
		update();
	}

	private void onMaxChange() {
		if (Boolean.TRUE.equals(maxItem.validate())) {
			currentPagination.setPage(1);
			currentPagination.setPageSize(Integer.parseInt(maxItem.getValue().toString()));
			update();
		}
	}

	private void onPageChange() {
		if (Boolean.TRUE.equals(pageItem.validate())) {
			currentPagination.setPage(Integer.parseInt(pageItem.getValue().toString()));
			update();
		}
	}

	public void registerMaxChangedHandler(ChangedHandler handler) {
		maxItem.addChangedHandler(handler);
	}

	public void registerPageChangedHandler(ChangedHandler handler) {
		pageItem.addChangedHandler(handler);
	}

	@Override
	public void onFolderSelected(GUIFolder folder) {
		FolderPagination pagination = paginations.get(folder.getId());
		if (pagination == null) {
			pagination = new FolderPagination(folder.getId(), Session.get().getConfigAsInt(GUI_FOLDER_MAXCHILDREN),
					(int)folder.getSubfolderCount(), 1);
			// Save it only if there are more than one page
			if (pagination.getTotalPages() >= 2)
				paginations.put(folder.getId(), pagination);
			currentPagination = pagination;
		} else {
			pagination.setTotalElements((int)folder.getSubfolderCount());
			currentPagination = pagination;

			// Remove from client and server if there are less than two pages
			if (pagination.getTotalPages() < 2)
				paginations.remove(folder.getId());
		}
		update();
	}

	private void update() {
		updateClient();
		updateServer();
	}

	private void updateClient() {
		maxItem.setValue(currentPagination.getPageSize());
		pageItem.setValue(currentPagination.getPage());
		pageItem.setMax(currentPagination.getTotalPages());
		pageItem.setHint("/" + (currentPagination.getTotalPages() > 0 ? currentPagination.getTotalPages() : 1));
	}

	public boolean hasMorePages() {
		return currentPagination.getPage() < currentPagination.getTotalPages();
	}

	/**
	 * Moves to the next page and updates the client(not the server)
	 */
	public void next() {
		if (hasMorePages()) {
			currentPagination.setPage(currentPagination.getPage() + 1);
		} else
			currentPagination.setPage(1);
		updateClient();
	}

	public long getFolderId() {
		return currentPagination.getFolderId();
	}

	private void updateServer() {
		if (currentPagination.getTotalPages() < 2
				&& currentPagination.getPageSize() == Session.get().getConfigAsInt(GUI_FOLDER_MAXCHILDREN))
			FolderService.Instance.get().setFolderPagination(currentPagination.getFolderId(), null, null,
					new AsyncCallback<Void>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Void arg) {
							// Nothing to do
						}
					});
		else
			FolderService.Instance.get().setFolderPagination(currentPagination.getFolderId(),
					currentPagination.getStartRow(), currentPagination.getPageSize(), new AsyncCallback<Void>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Void arg) {
							// Nothing to do
						}
					});
	}

	@Override
	public void onFolderChanged(GUIFolder folder) {
		onFolderCancelEditing(folder);
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
		if (currentPagination.getFolderId() == folder.getId())
			disable();
	}

	@Override
	public void onFolderCancelEditing(GUIFolder folder) {
		enable();
	}

	public FolderPagination getCurrentPagination() {
		return currentPagination;
	}
}