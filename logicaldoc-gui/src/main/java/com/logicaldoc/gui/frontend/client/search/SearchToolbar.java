package com.logicaldoc.gui.frontend.client.search;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.observer.FolderController;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsGrid;
import com.logicaldoc.gui.frontend.client.document.update.UpdateDialog;
import com.smartgwt.client.types.SelectionType;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * The toolbar uses in the search panel
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.1
 */
public class SearchToolbar extends ToolStrip {

	public SearchToolbar(final HitsListPanel hitsPanel) {
		setVisible(true);
		setHeight(20);
		setWidth100();
		addSpacer(2);

		ToolStripButton showSnippets = AwesomeFactory.newToolStripButton("file", "showsnippets");
		showSnippets.setDisabled(Search.get().getOptions().getType() != GUISearchOptions.TYPE_FULLTEXT);
		addButton(showSnippets);
		showSnippets.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				hitsPanel.getGrid().expandVisibleRows();
			}
		});

		ToolStripButton save = AwesomeFactory.newToolStripButton("save", "savesearch");
		save.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				SaveDialog dialog = new SaveDialog();
				dialog.show();
			}
		});

		if (Feature.visible(Feature.SAVED_SEARCHES)) {
			addSeparator();
			addButton(save);
			if (!Feature.enabled(Feature.SAVED_SEARCHES)) {
				save.setDisabled(true);
				save.setTooltip(I18N.message("featuredisabled"));
			}
		}

		ToolStripButton print = AwesomeFactory.newToolStripButton("print", "print");
		print.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				if (hitsPanel.getGrid() instanceof ListGrid)
					GridUtil.print((ListGrid) hitsPanel.getGrid());
				else
					Canvas.printComponents(new Object[] { hitsPanel.getGrid() });
			}
		});
		addSeparator();
		addButton(print);

		if (Feature.visible(Feature.EXPORT_CSV)) {
			ToolStripButton export = AwesomeFactory.newToolStripButton("angle-double-down", "export");
			addButton(export);
			export.addClickHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					GridUtil.exportCSV((ListGrid) hitsPanel.getGrid(), false);
				}
			});
			if (!Feature.enabled(Feature.EXPORT_CSV)) {
				export.setDisabled(true);
				export.setTooltip(I18N.message("featuredisabled"));
			}
		}

		ToolStripButton saveLayout = AwesomeFactory.newToolStripButton("save", "savelayoutinuserprofile");
		saveLayout.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				saveGridState();
			}
		});
		addButton(saveLayout);

		addSeparator();
		ToolStripButton download = AwesomeFactory.newToolStripButton("download", "download");
		addButton(download);
		download.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (Search.get().getOptions().getType() == GUISearchOptions.TYPE_FOLDERS
						|| Search.get().getLastResult() == null || Search.get().getLastResult().length < 1)
					return;

				String url = GWT.getHostPageBaseURL() + "zip-export?1=1";
				for (GUIDocument record : Search.get().getLastResult()) {
					url += "&docId=" + record.getId();
				}

				Util.download(url);
			}
		});

		if (Feature.visible(Feature.BULK_UPDATE))
			addSeparator();

		if (Feature.visible(Feature.BULK_UPDATE)) {
			ToolStripButton bulkUpdate = AwesomeFactory.newToolStripButton("edit", "bulkupdate");
			addButton(bulkUpdate);
			if (!Feature.enabled(Feature.BULK_UPDATE)) {
				bulkUpdate.setDisabled(true);
				bulkUpdate.setTooltip(I18N.message("featuredisabled"));
			}

			bulkUpdate.addClickHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					if (Search.get().getOptions().getType() == GUISearchOptions.TYPE_FOLDERS
							|| Search.get().getLastResult() == null || Search.get().getLastResult().length < 1)
						return;

					if (hitsPanel.getGrid().getSelectedIds().length > 1) {
						GUIDocument metadata = new GUIDocument();
						metadata.setBulkUpdate(true);
						metadata.setStartPublishing(null);
						metadata.setPublished(-1);
						metadata.setLockUserId(Session.get().getUser().getId());
						GUIFolder fld = new GUIFolder();
						fld.setPermissions(new String[] { "read", "write" });
						metadata.setFolder(fld);

						UpdateDialog dialog = new UpdateDialog(hitsPanel.getGrid().getSelectedIds(), metadata,
								UpdateDialog.CONTEXT_UPDATE, false);
						dialog.show();
					}
				}
			});

		}

		final ToolStripButton toggle = AwesomeFactory.newToolStripButton("toggle-on", "closeseleftpanel");
		if (SearchMenu.get().getWidth() > 0) {
			toggle.setTitle(AwesomeFactory.getIconHtml("toggle-on"));
			toggle.setTooltip(I18N.message("closeseleftpanel"));
		} else {
			toggle.setTitle(AwesomeFactory.getIconHtml("toggle-off"));
			toggle.setTooltip(I18N.message("openleftpanel"));
		}
		toggle.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				SearchPanel.get().toggleMenu();
				if (SearchPanel.get().isMenuOpened()) {
					toggle.setTitle(AwesomeFactory.getIconHtml("toggle-on"));
					toggle.setTooltip(I18N.message("closeseleftpanel"));
				} else {
					toggle.setTitle(AwesomeFactory.getIconHtml("toggle-off"));
					toggle.setTooltip(I18N.message("openleftpanel"));
				}
			}
		});
		addSeparator();
		addButton(toggle);

		final ToolStripButton list = AwesomeFactory.newToolStripButton("bars", "list");
		list.setRadioGroup("mode");
		list.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				CookiesManager.save(CookiesManager.COOKIE_HITSLIST_MODE, DocumentsGrid.MODE_LIST);
				hitsPanel.setVisualizationMode(DocumentsGrid.MODE_LIST);
				hitsPanel.initialize();
			}
		});

		final ToolStripButton gallery = AwesomeFactory.newToolStripButton("images", "gallery");
		gallery.setActionType(SelectionType.RADIO);
		gallery.setRadioGroup("mode");
		gallery.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (FolderController.get().getCurrentFolder() != null)
					CookiesManager.save(CookiesManager.COOKIE_HITSLIST_MODE, DocumentsGrid.MODE_GALLERY);
				hitsPanel.setVisualizationMode(DocumentsGrid.MODE_GALLERY);
				hitsPanel.initialize();
			}
		});
		gallery.setVisible(Session.get().getConfigAsBoolean("gui.galleryenabled"));

		final ToolStripButton togglePreview = AwesomeFactory.newToolStripButton("toggle-on", "closepreview");
		try {
			// Retrieve the saved preview width
			String w = CookiesManager.get(CookiesManager.COOKIE_HITSLIST_PREV_W);
			if (Integer.parseInt(w) <= 0) {
				togglePreview.setTitle(AwesomeFactory.getIconHtml("toggle-off"));
				togglePreview.setTooltip(I18N.message("openpreview"));
			}
		} catch (Throwable t) {
		}
		togglePreview.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (SearchPanel.get().getPreviewPanel().isVisible()
						&& SearchPanel.get().getPreviewPanel().getWidth() > 1) {
					SearchPanel.get().getPreviewPanel().setWidth(0);
					togglePreview.setTitle(AwesomeFactory.getIconHtml("toggle-off"));
					togglePreview.setTooltip(I18N.message("openpreview"));
				} else {
					try {
						String w = CookiesManager.get(CookiesManager.COOKIE_HITSLIST_PREV_W);
						SearchPanel.get().getPreviewPanel().setWidth(Integer.parseInt(w));
					} catch (Throwable t) {
						SearchPanel.get().getPreviewPanel().setWidth(350);
					}
					SearchPanel.get().getPreviewPanel().setDocument(hitsPanel.getGrid().getSelectedDocument());
					togglePreview.setTitle(AwesomeFactory.getIconHtml("toggle-on"));
					togglePreview.setTooltip(I18N.message("closepreview"));
				}
			}
		});

		if (hitsPanel.getVisualizationMode() == DocumentsGrid.MODE_LIST)
			list.setSelected(true);
		else
			gallery.setSelected(true);

		addSeparator();
		addButton(list);
		addButton(gallery);
		addSeparator();
		addButton(togglePreview);

		addFill();
	}

	private void saveGridState() {
		Session.get().getUser().setHitsGrid(SearchPanel.get().getDocsGridLayout());
		SecurityService.Instance.get().saveInterfaceSettings(Session.get().getUser(), new AsyncCallback<GUIUser>() {

			@Override
			public void onFailure(Throwable e) {
				// Nothing to do
			}

			@Override
			public void onSuccess(GUIUser usr) {
				GuiLog.info(I18N.message("settingssaved"));
			}
		});
	}
}
