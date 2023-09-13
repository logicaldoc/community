package com.logicaldoc.gui.frontend.client.zoho;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.panels.MainPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.ZohoService;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

public class ZohoMenuItem extends MenuItem {

	public ZohoMenuItem(GUIFolder folder, GUIDocument document) {
		super(I18N.message("zoho"));

		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);

		final MenuItem authorize = new MenuItem(I18N.message("authorize"));
		authorize.addClickHandler(authorizeClick -> new ZohoAuthorization().show());

		final MenuItem importDocs = new MenuItem(I18N.message("importfromzoho"));
		importDocs.addClickHandler(importDocsClick -> {
			ZohoDialog dialog = new ZohoDialog(false);
			dialog.show();
		});
		final MenuItem exportDocs = new MenuItem(I18N.message("exporttozoho"));
		exportDocs.addClickHandler(exportDocsClick -> {
			ZohoDialog dialog = new ZohoDialog(true);
			dialog.show();
		});

		final MenuItem edit = prepareEditItem(document);

		menu.setItems(authorize, importDocs, exportDocs, edit);

		importDocs.setEnabled(folder != null && folder.isDownload() && folder.isWrite() && Feature.enabled(Feature.ZOHO)
				&& MainPanel.get().isOnDocumentsTab());
		exportDocs.setEnabled(folder != null && folder.isDownload() && Feature.enabled(Feature.ZOHO));
		authorize.setEnabled(Feature.enabled(Feature.ZOHO));
		edit.setEnabled(document != null && document.getImmutable() == 0 && folder != null && folder.isDownload()
				&& folder.isWrite() && Feature.enabled(Feature.ZOHO));

		setSubmenu(menu);

	}

	private MenuItem prepareEditItem(GUIDocument document) {
		final MenuItem edit = new MenuItem(I18N.message("editdoc"));
		edit.addClickHandler((MenuItemClickEvent editClick) -> {
			if (document == null)
				return;

			if (document.getStatus() == 0) {
				// Need to checkout first
				DocumentService.Instance.get().checkout(new Long[] { document.getId() }, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						DocUtil.markCheckedOut(document);

						LD.contactingServer();
						ZohoService.Instance.get().upload(document.getId(), new AsyncCallback<String>() {
							@Override
							public void onFailure(Throwable caught) {
								LD.clearPrompt();
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(String resourceId) {
								LD.clearPrompt();
								if (resourceId == null) {
									GuiLog.error(I18N.message("zohoerror"), null, null);
									return;
								}
								document.setExtResId(resourceId);
								DocumentController.get().modified(document);
								ZohoEditor popup = new ZohoEditor(document);
								popup.show();
							}
						});
					}
				});
			} else {
				if (document.getStatus() == 1 && document.getExtResId() != null) {
					ZohoEditor popup = new ZohoEditor(document);
					popup.show();
				} else {
					SC.warn(I18N.message("event.locked"));
				}
			}
		});
		return edit;
	}
}