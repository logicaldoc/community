package com.logicaldoc.gui.frontend.client.dashboard.dashlet;

import com.logicaldoc.gui.common.client.beans.GUIDashlet;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.widgets.preview.PreviewPopup;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.smartgwt.client.types.DragAppearance;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.layout.Portlet;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * A small window inside the dashboard.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.6
 */
public abstract class Dashlet extends Portlet {

	protected GUIDashlet guiDashlet;

	protected HeaderControl refreshControl;

	public Dashlet(GUIDashlet guiDashlet) {
		super();
		this.guiDashlet = guiDashlet;

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

		refreshControl = new HeaderControl(HeaderControl.REFRESH, event -> refresh());
		refreshControl.setTooltip(I18N.message("refresh"));

		setHeaderControls(HeaderControls.HEADER_LABEL, refreshControl, HeaderControls.MINIMIZE_BUTTON,
				HeaderControls.MAXIMIZE_BUTTON, HeaderControls.CLOSE_BUTTON);

		if (guiDashlet.getTitle() != null && !guiDashlet.getTitle().isEmpty())
			setTitle(AwesomeFactory.getIconHtml("cube", I18N.message(guiDashlet.getTitle())));
		else
			setTitle(AwesomeFactory.getIconHtml("cube", I18N.message(guiDashlet.getName())));
	}

	public static Dashlet getDashlet(GUIDashlet guiDashlet) {
		Dashlet dashlet = null;
		if (GUIDashlet.TYPE_DOCEVENT.equals(guiDashlet.getType()))
			dashlet = new DocumentHistoryDashlet(guiDashlet);
		else if (GUIDashlet.TYPE_DOCUMENT.equals(guiDashlet.getType()))
			dashlet = new DocumentDashlet(guiDashlet);
		else if (GUIDashlet.TYPE_NOTE.equals(guiDashlet.getType()))
			dashlet = new NotesDashlet(guiDashlet);
		else if (GUIDashlet.TYPE_CONTENT.equals(guiDashlet.getType())) {
			if (guiDashlet.getName().equals("tagcloud"))
				dashlet = new TagCloudDashlet(guiDashlet);
			else
				dashlet = new ContentDashlet(guiDashlet);
		}
		return dashlet;
	}

	protected String getDataSourceUrl() {
		return "dashletcontent?locale=" + I18N.getLocale() + "&dashletId=" + guiDashlet.getId();
	}

	@Override
	public String toString() {
		return "Dashlet " + guiDashlet.getName();
	}

	/**
	 * Prepares the context menu.
	 */
	protected Menu prepareContextMenu(final GUIDocument document) {
		Menu contextMenu = new Menu();

		MenuItem download = new MenuItem();
		download.setTitle(I18N.message("download"));
		download.addClickHandler(event -> DocUtil.download(document.getId(), null));
		download.setEnabled(document.getFolder().isDownload());

		MenuItem preview = new MenuItem();
		preview.setTitle(I18N.message("preview"));
		preview.addClickHandler(event -> {
			PreviewPopup iv = new PreviewPopup(document);
			iv.show();
		});
		preview.setEnabled(
				com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.PREVIEW));

		MenuItem openInFolder = new MenuItem();
		openInFolder.setTitle(I18N.message("openinfolder"));
		openInFolder.addClickHandler(
				event -> DocumentsPanel.get().openInFolder(document.getFolder().getId(), document.getId()));

		contextMenu.setItems(preview, download, openInFolder);

		return contextMenu;
	}

	public GUIDashlet getGuiDashlet() {
		return guiDashlet;
	}

	protected void refresh() {
		// Nothing to do
	}
}