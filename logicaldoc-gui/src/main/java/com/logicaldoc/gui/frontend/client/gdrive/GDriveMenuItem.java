package com.logicaldoc.gui.frontend.client.gdrive;

import java.util.Arrays;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsGrid;
import com.logicaldoc.gui.frontend.client.panels.MainPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.GDriveService;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * A menu item for interactig with GDrive
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.4
 */
public class GDriveMenuItem extends MenuItem {

	public GDriveMenuItem(GUIFolder folder, GUIDocument document) {
		super(I18N.message("googledrive"));

		final MenuItem importDocs = prepareGDriveImportDocsMenuItem(folder);

		final MenuItem exportDocs = prepareGDriveExportDocsMenuItem(folder);

		final MenuItem authorize = prepareGDriveAuthorizeMenuItem();

		final MenuItem edit = prepareGDriveEditMenuItem(folder, document);

		final MenuItem create = prepareGDriveCreateMenuItem(folder);

		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);
		menu.setItems(authorize, importDocs, exportDocs, edit, create);

		setSubmenu(menu);
	}

	private MenuItem prepareGDriveCreateMenuItem(GUIFolder folder) {
		final MenuItem create = new MenuItem(I18N.message("createdoc"));
		create.addClickHandler(event -> new GDriveCreate().show());
		create.setEnabled(folder != null && folder.isWrite() && Feature.enabled(Feature.GDRIVE)
				&& MainPanel.get().isOnDocumentsTab());
		return create;
	}

	private MenuItem prepareGDriveAuthorizeMenuItem() {
		final MenuItem authorize = new MenuItem(I18N.message("authorize"));
		authorize.addClickHandler((MenuItemClickEvent authorizeClick) -> new GDriveAuthorization().show());
		authorize.setEnabled(Feature.enabled(Feature.GDRIVE));
		return authorize;
	}

	private MenuItem prepareGDriveExportDocsMenuItem(GUIFolder folder) {
		final MenuItem exportDocs = new MenuItem(I18N.message("exporttogdrive"));
		exportDocs.addClickHandler((MenuItemClickEvent exportDocsClick) -> {
			DocumentsGrid grid = DocumentsPanel.get().getDocumentsGrid();
			List<Long> ids = grid.getSelectedIds();

			LD.contactingServer();
			GDriveService.Instance.get().exportDocuments(ids, new AsyncCallback<String[]>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
					LD.clearPrompt();
				}

				@Override
				public void onSuccess(String[] settings) {
					LD.clearPrompt();
					GuiLog.info(I18N.message("gdriveexportok"), null);
				}
			});
		});
		exportDocs.setEnabled(folder != null && folder.isDownload() && Feature.enabled(Feature.GDRIVE));
		return exportDocs;
	}

	private MenuItem prepareGDriveImportDocsMenuItem(GUIFolder folder) {
		final MenuItem importDocs = new MenuItem(I18N.message("importfromgdrive"));
		importDocs.addClickHandler((MenuItemClickEvent importDocsClick) -> {
			GDriveImport popup = new GDriveImport();
			popup.show();
		});
		importDocs.setEnabled(folder != null && folder.isDownload() && folder.isWrite()
				&& Feature.enabled(Feature.GDRIVE) && MainPanel.get().isOnDocumentsTab());
		return importDocs;
	}

	private MenuItem prepareGDriveEditMenuItem(GUIFolder folder, final GUIDocument document) {
		final MenuItem edit = new MenuItem(I18N.message("editdoc"));
		edit.addClickHandler((MenuItemClickEvent editClick) -> {
			if (document == null)
				return;

			if (document.getStatus() == 0) {
				checkoutAndUploadToGDrive(document);
			} else {
				if (document.getStatus() == 1 && document.getExtResId() != null) {
					GDriveEditor popup = new GDriveEditor(document);
					popup.show();
				} else {
					SC.warn(I18N.message("event.locked"));
				}
			}
		});
		edit.setEnabled(document != null && document.getImmutable() == 0 && folder != null && folder.isDownload()
				&& folder.isWrite() && Feature.enabled(Feature.GDRIVE));
		return edit;
	}

	private void checkoutAndUploadToGDrive(final GUIDocument document) {
		// Need to checkout first
		DocumentService.Instance.get().checkout(Arrays.asList(document.getId()), new AsyncCallback<Void>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void result) {
				document.setStatus(Constants.DOC_CHECKED_OUT);
				document.setLockUserId(Session.get().getUser().getId());
				document.setLockUser(Session.get().getUser().getFullName());
				DocumentController.get().modified(document);

				Session.get().getUser().setCheckedOutDocs(Session.get().getUser().getCheckedOutDocs() + 1);
				GuiLog.info(I18N.message("documentcheckedout"), null);

				LD.contactingServer();
				GDriveService.Instance.get().upload(document.getId(), new AsyncCallback<String>() {
					@Override
					public void onFailure(Throwable caught) {
						LD.clearPrompt();
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(String resourceId) {
						LD.clearPrompt();
						if (resourceId == null) {
							GuiLog.error(I18N.message("gdriveerror"), null, null);
							return;
						}
						document.setExtResId(resourceId);
						DocumentController.get().modified(document);
						GDriveEditor popup = new GDriveEditor(document);
						popup.show();
					}
				});
			}
		});
	}
}