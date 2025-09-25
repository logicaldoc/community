package com.logicaldoc.gui.frontend.client.search;

import com.google.gwt.core.client.GWT;
import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.IgnoreAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsGrid;
import com.logicaldoc.gui.frontend.client.document.update.UpdateDialog;
import com.smartgwt.client.types.SelectionType;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
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

	private static final String TOGGLE_OFF = "toggle-off";

	private static final String CLOSESELEFTPANEL = "closeseleftpanel";

	private static final String TOGGLE_ON = "toggle-on";

	public SearchToolbar(final HitsListPanel hitsPanel) {
		setVisible(true);
		setHeight(20);
		setWidth100();
		addSpacer(2);

		addShowSnippetsButton(hitsPanel);

		addSaveButton();

		addSeparator();

		addPrint(hitsPanel);

		addExport(hitsPanel);

		addSaveLayout();

		addSeparator();

		addDownload();

		addBulkUpdate(hitsPanel);

		addSeparator();

		addToggleLeftPanel();

		addSeparator();

		addList(hitsPanel);

		addGallery(hitsPanel);

		addSeparator();

		addTogglePreview(hitsPanel);

		addFill();
	}

	private void addShowSnippetsButton(final HitsListPanel hitsPanel) {
		ToolStripButton showSnippets = AwesomeFactory.newToolStripButton("file", "showsnippets");
		showSnippets.setDisabled(Search.get().getOptions().getType() != GUISearchOptions.TYPE_FULLTEXT);
		addButton(showSnippets);
		showSnippets.addClickHandler(event -> hitsPanel.getGrid().expandVisibleRows());
	}

	private void addSaveButton() {
		ToolStripButton save = AwesomeFactory.newToolStripButton("save", "savesearch");
		save.addClickHandler(event -> new SaveDialog().show());
		if (Feature.visible(Feature.SAVED_SEARCHES)) {
			addSeparator();
			addButton(save);
			if (!Feature.enabled(Feature.SAVED_SEARCHES))
				setFeatureDisabled(save);
		}
	}

	private void setFeatureDisabled(ToolStripButton button) {
		button.setDisabled(true);
		button.setTooltip(I18N.message("featuredisabled"));
	}

	private void addPrint(final HitsListPanel hitsPanel) {
		ToolStripButton print = AwesomeFactory.newToolStripButton("print", "print");
		print.addClickHandler(event -> {
			if (hitsPanel.getGrid() instanceof ListGrid listGrid)
				GridUtil.print(listGrid);
			else
				Canvas.printComponents(new Object[] { hitsPanel.getGrid() });
		});
		addButton(print);
	}

	private void addExport(final HitsListPanel hitsPanel) {
		if (Feature.visible(Feature.EXPORT_CSV)) {
			ToolStripButton export = AwesomeFactory.newToolStripButton("angle-double-down", "export");
			addButton(export);
			export.addClickHandler(click -> GridUtil.exportCSV((ListGrid) hitsPanel.getGrid(), false));
			if (!Feature.enabled(Feature.EXPORT_CSV))
				setFeatureDisabled(export);
		}
	}

	private void addSaveLayout() {
		ToolStripButton saveLayout = AwesomeFactory.newToolStripButton("save", "savelayoutinuserprofile");
		saveLayout.addClickHandler(click -> saveGridState());
		addButton(saveLayout);
	}

	private void addDownload() {
		ToolStripButton download = AwesomeFactory.newToolStripButton("download", "download");
		addButton(download);
		download.addClickHandler(click -> {
			if (Search.get().getOptions().getType() == GUISearchOptions.TYPE_FOLDERS
					|| Search.get().getLastResult().isEmpty())
				return;

			StringBuilder url = new StringBuilder(GWT.getHostPageBaseURL() + "zip-export?1=1");
			for (GUIDocument rec : Search.get().getLastResult()) {
				url.append("&docId=");
				url.append(rec.getId());
			}

			Util.download(url.toString());
		});
	}

	private void addBulkUpdate(final HitsListPanel hitsPanel) {
		if (Feature.visible(Feature.BULK_UPDATE)) {
			addSeparator();
			ToolStripButton bulkUpdate = AwesomeFactory.newToolStripButton("edit", "bulkupdate");
			addButton(bulkUpdate);
			if (!Feature.enabled(Feature.BULK_UPDATE)) {
				setFeatureDisabled(bulkUpdate);
			}

			bulkUpdate.addClickHandler(event -> {
				if (Search.get().getOptions().getType() == GUISearchOptions.TYPE_FOLDERS
						|| Search.get().getLastResult().isEmpty())
					return;

				if (!hitsPanel.getGrid().getSelectedIds().isEmpty()) {
					GUIDocument metadata = new GUIDocument();
					metadata.setBulkUpdate(true);
					metadata.setStartPublishing(null);
					metadata.setPublished(-1);
					metadata.setLockUserId(Session.get().getUser().getId());
					GUIFolder fld = new GUIFolder();
					fld.setAllowedPermissions(new GUIAccessControlEntry(GUIAccessControlEntry.PERMISSION_READ,
							GUIAccessControlEntry.PERMISSION_WRITE));
					metadata.setFolder(fld);

					UpdateDialog dialog = new UpdateDialog(hitsPanel.getGrid().getSelectedIds(), metadata,
							UpdateDialog.BULKUPDATE, false);
					dialog.show();
				}
			});
		}
	}

	private void addToggleLeftPanel() {
		ToolStripButton toggle = AwesomeFactory.newToolStripButton(TOGGLE_ON, CLOSESELEFTPANEL);
		if (SearchMenu.get().getWidth() > 0) {
			toggle.setTitle(AwesomeFactory.getIconHtml(TOGGLE_ON));
			toggle.setTooltip(I18N.message(CLOSESELEFTPANEL));
		} else {
			toggle.setTitle(AwesomeFactory.getIconHtml(TOGGLE_OFF));
			toggle.setTooltip(I18N.message("openleftpanel"));
		}
		toggle.addClickHandler((ClickEvent event) -> {
			SearchPanel.get().toggleMenu();
			if (SearchPanel.get().isMenuOpened()) {
				toggle.setTitle(AwesomeFactory.getIconHtml(TOGGLE_ON));
				toggle.setTooltip(I18N.message(CLOSESELEFTPANEL));
			} else {
				toggle.setTitle(AwesomeFactory.getIconHtml(TOGGLE_OFF));
				toggle.setTooltip(I18N.message("openleftpanel"));
			}
		});

		addButton(toggle);
	}

	private void addTogglePreview(HitsListPanel hitsPanel) {
		ToolStripButton togglePreview = AwesomeFactory.newToolStripButton(TOGGLE_ON, "closepreview");
		// Retrieve the saved preview width
		String w = CookiesManager.get(CookiesManager.COOKIE_HITSLIST_PREV_W);
		if (w != null && !w.isEmpty() && Integer.parseInt(w) <= 0) {
			togglePreview.setTitle(AwesomeFactory.getIconHtml(TOGGLE_OFF));
			togglePreview.setTooltip(I18N.message("openpreview"));
		}
		togglePreview.addClickHandler(event -> {
			if (SearchPanel.get().getPreviewPanel().isVisible() && SearchPanel.get().getPreviewPanel().getWidth() > 1) {
				SearchPanel.get().getPreviewPanel().setWidth(0);
				togglePreview.setTitle(AwesomeFactory.getIconHtml(TOGGLE_OFF));
				togglePreview.setTooltip(I18N.message("openpreview"));
			} else {
				try {
					String width = CookiesManager.get(CookiesManager.COOKIE_HITSLIST_PREV_W);
					SearchPanel.get().getPreviewPanel().setWidth(Integer.parseInt(width));
				} catch (Exception t) {
					SearchPanel.get().getPreviewPanel().setWidth(350);
				}
				SearchPanel.get().getPreviewPanel().setDocument(hitsPanel.getGrid().getSelectedDocument());
				togglePreview.setTitle(AwesomeFactory.getIconHtml(TOGGLE_ON));
				togglePreview.setTooltip(I18N.message("closepreview"));
			}
		});
		addButton(togglePreview);
	}

	private void addGallery(HitsListPanel hitsPanel) {
		ToolStripButton gallery = AwesomeFactory.newToolStripButton("images", "gallery");
		gallery.setActionType(SelectionType.RADIO);
		gallery.setRadioGroup("mode");
		gallery.addClickHandler(event -> {
			if (FolderController.get().getCurrentFolder() != null)
				CookiesManager.save(CookiesManager.COOKIE_HITSLIST_MODE, DocumentsGrid.MODE_GALLERY);
			hitsPanel.setVisualizationMode(DocumentsGrid.MODE_GALLERY);
			hitsPanel.initialize();
		});
		gallery.setVisible(Session.get().getConfigAsBoolean("gui.galleryenabled"));
		gallery.setSelected(hitsPanel.getVisualizationMode() != DocumentsGrid.MODE_LIST);
		addButton(gallery);
	}

	private void addList(HitsListPanel hitsPanel) {
		ToolStripButton list = AwesomeFactory.newToolStripButton("bars", "list");
		list.setActionType(SelectionType.RADIO);
		list.setRadioGroup("mode");
		list.addClickHandler(event -> {
			CookiesManager.save(CookiesManager.COOKIE_HITSLIST_MODE, DocumentsGrid.MODE_LIST);
			hitsPanel.setVisualizationMode(DocumentsGrid.MODE_LIST);
			hitsPanel.initialize();
		});
		list.setSelected(hitsPanel.getVisualizationMode() == DocumentsGrid.MODE_LIST);
		addButton(list);
	}

	private void saveGridState() {
		Session.get().getUser().setHitsGrid(SearchPanel.get().getDocsGridLayout());
		SecurityService.Instance.get().saveInterfaceSettings(Session.get().getUser(), new IgnoreAsyncCallback<>() {
			@Override
			public void onSuccess(GUIUser usr) {
				GuiLog.info(I18N.message("settingssaved"));
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
