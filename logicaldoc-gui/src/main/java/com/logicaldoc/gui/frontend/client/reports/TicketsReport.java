package com.logicaldoc.gui.frontend.client.reports;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.TicketsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.widgets.preview.PreviewPopup;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows a list of tickets
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4
 */
public class TicketsReport extends ReportPanel {

	private static final String DOC_ID = "docId";

	private static final String VALID = "valid";

	private static final String EENABLED = "eenabled";

	private SpinnerItem max;

	public TicketsReport() {
		super("tickets", "showntickets");
	}

	@Override
	protected void fillToolBar(ToolStrip toolStrip) {
		max = ItemFactory.newSpinnerItem("max", "", 100, 5, null);
		max.setHint(I18N.message("elements"));
		max.setShowTitle(false);
		max.setStep(10);
		max.addChangedHandler(event -> refresh());

		ToolStripButton display = new ToolStripButton();
		display.setTitle(I18N.message("display"));
		display.addClickHandler(event -> {
			if (Boolean.TRUE.equals(max.validate()))
				refresh();
		});
		toolStrip.addButton(display);
		toolStrip.addFormItem(max);
	}

	@Override
	protected void prepareListGrid() {
		ListGridField id = new ListGridField("id");
		id.setHidden(true);
		id.setCanGroupBy(false);

		ListGridField enabled = new ListGridField(EENABLED, " ", 24);
		enabled.setType(ListGridFieldType.IMAGE);
		enabled.setCanSort(false);
		enabled.setAlign(Alignment.CENTER);
		enabled.setShowDefaultContextMenu(false);
		enabled.setImageURLPrefix(Util.imagePrefix());
		enabled.setImageURLSuffix(".gif");
		enabled.setCanFilter(false);

		ListGridField ticketId = new ListGridField("ticketId", I18N.message("ticket"), 210);
		ticketId.setAlign(Alignment.CENTER);
		ticketId.setCanFilter(true);
		ticketId.setCanGroupBy(false);

		ListGridField type = new ListGridField("type", I18N.message("type"));
		type.setAutoFitWidth(true);
		type.setAlign(Alignment.CENTER);
		type.setCanFilter(true);
		type.setCanGroupBy(false);
		type.setCellFormatter(
				(value, record, rowNum, colNum) -> I18N.message("0".equals(value.toString()) ? "download" : "view"));

		ListGridField creation = new DateListGridField("creation", "createdon");
		creation.setCanGroupBy(false);

		ListGridField expired = new DateListGridField("expired", "expireson");
		expired.setCanGroupBy(false);

		FileNameListGridField fileName = new FileNameListGridField();
		fileName.setWidth(200);
		fileName.setCanFilter(true);

		ListGridField count = new ListGridField("count", I18N.message("downloads"), 80);
		count.setAlign(Alignment.CENTER);
		count.setCanFilter(false);
		count.setCanGroupBy(false);

		ListGridField maxCount = new ListGridField("maxCount", I18N.message("maxdownloads"), 100);
		maxCount.setAlign(Alignment.CENTER);
		maxCount.setCanFilter(false);
		maxCount.setCanGroupBy(false);

		ListGridField views = new ListGridField("views", I18N.message("views"), 80);
		views.setAlign(Alignment.CENTER);
		views.setCanFilter(false);
		views.setCanGroupBy(false);

		ListGridField maxViews = new ListGridField("maxViews", I18N.message("maxviews"), 100);
		maxViews.setAlign(Alignment.CENTER);
		maxViews.setCanFilter(false);
		maxViews.setCanGroupBy(false);

		list.addDoubleClickHandler(event -> DocUtil.download(list.getSelectedRecord().getAttributeAsLong("id"), null));

		list.setFields(enabled, id, ticketId, type, count, maxCount, views, maxViews, creation, expired, fileName);
	}

	@Override
	protected RefreshableListGrid createListGrid() {
		return new RefreshableListGrid() {
			@Override
			protected String getCellCSSText(ListGridRecord rec, int rowNum, int colNum) {
				if (Boolean.FALSE.equals(rec.getAttributeAsBoolean(VALID)))
					return "color: #888888; font-style: italic;";
				else
					return super.getCellCSSText(rec, rowNum, colNum);
			}
		};
	}

	@Override
	protected void refresh() {
		int maxElements = max.getValueAsInteger();
		list.refresh(new TicketsDS(maxElements));
	}

	@Override
	protected void showContextMenu() {
		final ListGridRecord rec = list.getSelectedRecord();

		Menu contextMenu = new Menu();
		MenuItem preview = new MenuItem();
		preview.setTitle(I18N.message("preview"));
		preview.addClickHandler((MenuItemClickEvent event) -> {
			long id = Long.parseLong(rec.getAttribute(DOC_ID));
			DocumentService.Instance.get().getById(id, new AsyncCallback<GUIDocument>() {

				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(GUIDocument doc) {
					PreviewPopup iv = new PreviewPopup(doc);
					iv.show();
				}
			});
		});
		preview.setEnabled(
				com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.PREVIEW));

		MenuItem download = new MenuItem();
		download.setTitle(I18N.message("download"));
		download.addClickHandler((MenuItemClickEvent event) -> {
			String id = rec.getAttribute(DOC_ID);
			WindowUtils.openUrl(GWT.getHostPageBaseURL() + "download?docId=" + id);
		});

		MenuItem ticketURL = new MenuItem();
		ticketURL.setTitle(I18N.message("ticketurl"));
		ticketURL.addClickHandler((MenuItemClickEvent event) -> {
			String ticketId = rec.getAttributeAsString("ticketId");

			String url = Session.get().getConfig("server.url");
			if (!url.endsWith("/"))
				url += "/";
			url += "download-ticket?ticketId=" + ticketId;

			SC.confirm(I18N.message("downloadticket") + " - " + ticketId,
					"<a href='" + url + "' target='_blank'>" + url + "</a>", null);
		});

		MenuItem openInFolder = new MenuItem();
		openInFolder.setTitle(I18N.message("openinfolder"));
		openInFolder.addClickHandler(
				event -> DocumentsPanel.get().openInFolder(Long.parseLong(rec.getAttributeAsString("folderId")),
						Long.parseLong(rec.getAttributeAsString(DOC_ID))));

		MenuItem enable = new MenuItem();
		enable.setTitle(I18N.message("enable"));
		enable.addClickHandler(event -> DocumentService.Instance.get().enableTicket(rec.getAttributeAsLong("id"),
				new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(EENABLED, "0");
						rec.setAttribute(VALID, true);
						list.refreshRow(list.getRecordIndex(rec));
					}
				}));

		MenuItem disable = new MenuItem();
		disable.setTitle(I18N.message("disable"));
		disable.addClickHandler(event -> DocumentService.Instance.get().disableTicket(rec.getAttributeAsLong("id"),
				new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(EENABLED, "2");
						rec.setAttribute(VALID, false);
						list.refreshRow(list.getRecordIndex(rec));
					}
				}));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), answer -> {
			if (Boolean.TRUE.equals(answer))
				DocumentService.Instance.get().deleteTicket(rec.getAttributeAsLong("id"), new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						list.removeSelectedData();
						list.deselectAllRecords();
					}
				});
		}));

		if (!(list.getSelectedRecords() != null && list.getSelectedRecords().length == 1)) {
			ticketURL.setEnabled(false);
			download.setEnabled(false);
			preview.setEnabled(false);
			openInFolder.setEnabled(false);
			enable.setEnabled(false);
			disable.setEnabled(false);
			delete.setEnabled(false);
		}

		if ("0".equals(rec.getAttributeAsString(EENABLED)))
			contextMenu.setItems(disable, ticketURL, download, preview, openInFolder, delete);
		else
			contextMenu.setItems(enable, ticketURL, download, preview, openInFolder, delete);
		contextMenu.showContextMenu();
	}
}