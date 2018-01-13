package com.logicaldoc.gui.frontend.client.dashboard.dashlet;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.widgets.PreviewPopup;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.smartgwt.client.types.DragAppearance;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.layout.Portlet;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * A small window inside the dashboard.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.6
 */
public class Dashlet extends Portlet {
	private int id;

	public Dashlet(int id) {
		super();
		this.id = id;

		setMinHeight(150);
		setMinWidth(100);

		// enable predefined component animation
		setAnimateMinimize(true);

		// Window is draggable with "outline" appearance by default.
		// "target" is the solid appearance.
		setDragAppearance(DragAppearance.OUTLINE);
		setCanDrop(true);

		// show either a shadow, or translucency, when dragging a portlet
		// (could do both at the same time, but these are not visually
		// compatible effects)
		// setShowDragShadow(true);
		setDragOpacity(30);

		setCloseConfirmationMessage(I18N.message("closedashletconfirm"));

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.MAXIMIZE_BUTTON, HeaderControls.CLOSE_BUTTON);
	}

	public static Dashlet getDashlet(int dashletId) {
		Dashlet dashlet = null;
		switch (dashletId) {
		case Constants.DASHLET_CHECKOUT:
			dashlet = new StatusDashlet(Constants.DASHLET_CHECKOUT, Constants.EVENT_CHECKEDOUT);
			break;
		case Constants.DASHLET_CHECKIN:
			dashlet = new HistoryDashlet(Constants.DASHLET_CHECKIN, Constants.EVENT_CHECKEDIN);
			break;
		case Constants.DASHLET_LOCKED:
			dashlet = new StatusDashlet(Constants.DASHLET_LOCKED, Constants.EVENT_LOCKED);
			break;
		case Constants.DASHLET_DOWNLOADED:
			dashlet = new HistoryDashlet(Constants.DASHLET_DOWNLOADED, Constants.EVENT_DOWNLOADED);
			break;
		case Constants.DASHLET_CHANGED:
			dashlet = new HistoryDashlet(Constants.DASHLET_CHANGED, Constants.EVENT_CHANGED);
			break;
		case Constants.DASHLET_LAST_NOTES:
			dashlet = new LastNotesDashlet();
			break;
		case Constants.DASHLET_TAGCLOUD:
			dashlet = new TagCloudDashlet();
			break;
		}

		return dashlet;
	}

	public int getId() {
		return id;
	}

	@Override
	public String toString() {
		return "Dashlet " + getId();
	}

	/**
	 * Prepares the context menu.
	 */
	protected Menu prepareContextMenu(final GUIDocument document) {
		Menu contextMenu = new Menu();

		MenuItem download = new MenuItem();
		download.setTitle(I18N.message("download"));
		download.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				DocUtil.download(document.getId(), null);
			}
		});
		download.setEnabled(document.getFolder().isDownload());

		MenuItem preview = new MenuItem();
		preview.setTitle(I18N.message("preview"));
		preview.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				PreviewPopup iv = new PreviewPopup(document);
				iv.show();
			}
		});

		MenuItem openInFolder = new MenuItem();
		openInFolder.setTitle(I18N.message("openinfolder"));
		openInFolder.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				DocumentsPanel.get().openInFolder(document.getFolder().getId(), document.getId());
			}
		});

		contextMenu.setItems(preview, download, openInFolder);

		return contextMenu;
	}
}