package com.logicaldoc.gui.frontend.client.reports;

import com.google.gwt.core.client.GWT;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.TicketsDS;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.common.client.grid.EnabledListGridField;
import com.logicaldoc.gui.common.client.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.preview.PreviewPopup;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.document.TicketDisplay;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
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

	private static final String ENABLED = "eenabled";

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
		display.addClickHandler(click -> {
			if (Boolean.TRUE.equals(max.validate()))
				refresh();
		});
		toolStrip.addButton(display);
		toolStrip.addFormItem(max);
	}

	@Override
	protected void prepareListGrid() {
		ListGridField id = new ListGridField("id");
		id.setWidth(90);
		id.setHidden(true);
		id.setCanGroupBy(false);

		ListGridField enabled = new EnabledListGridField();

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
				(value, rcrd, rowNum, colNum) -> I18N.message("0".equals(value.toString()) ? "download" : "view"));

		ListGridField creation = new DateListGridField("creation", "createdon", DateCellFormatter.FORMAT_LONG);
		creation.setCanGroupBy(false);

		ListGridField expired = new DateListGridField("expired", "expireson", DateCellFormatter.FORMAT_LONG);
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

		list.addDoubleClickHandler(click -> DocUtil.download(list.getSelectedRecord().getAttributeAsLong("id"), null));

		list.setFields(id, enabled, ticketId, type, count, maxCount, views, maxViews, creation, expired, fileName);
	}

	@Override
	protected RefreshableListGrid createListGrid() {
		return new RefreshableListGrid() {
			@Override
			protected String getCellCSSText(ListGridRecord rec, int rowNum, int colNum) {
				if (Boolean.FALSE.equals(rec.getAttributeAsBoolean(VALID))
						|| Boolean.FALSE.equals(rec.getAttributeAsBoolean(ENABLED))) {
					return Boolean.TRUE.equals(rec.getAttributeAsBoolean(ENABLED) ? "color: #888888;" : "color: red;")
							+ " font-style: italic;";
				} else {
					return super.getCellCSSText(rec, rowNum, colNum);
				}
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
		preview.addClickHandler(click -> {
			long id = Long.parseLong(rec.getAttribute(DOC_ID));
			DocumentService.Instance.get().getById(id, new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(GUIDocument doc) {
					new PreviewPopup(doc).show();
				}
			});
		});
		preview.setEnabled(
				com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.PREVIEW));

		MenuItem download = new MenuItem();
		download.setTitle(I18N.message("download"));
		download.addClickHandler(
				click -> WindowUtils.openUrl(GWT.getHostPageBaseURL() + "download?docId=" + rec.getAttribute(DOC_ID)));

		MenuItem ticketURL = new MenuItem();
		ticketURL.setTitle(I18N.message("ticketurl"));
		ticketURL.addClickHandler((MenuItemClickEvent event) -> {
			String ticketId = rec.getAttributeAsString("ticketId");
			String type = rec.getAttributeAsString("type");

			new TicketDisplay(ticketId, sampleTicketUrl(Session.get().getConfig("server.url"), ticketId, type),
					sampleTicketUrl(Util.contextPath(), ticketId, type)).show();
		});

		MenuItem openInFolder = new MenuItem();
		openInFolder.setTitle(I18N.message("openinfolder"));
		openInFolder.addClickHandler(
				event -> DocumentsPanel.get().openInFolder(Long.parseLong(rec.getAttributeAsString("folderId")),
						Long.parseLong(rec.getAttributeAsString(DOC_ID))));

		MenuItem enable = new MenuItem();
		enable.setTitle(I18N.message("enable"));
		enable.setEnabled(Boolean.FALSE.equals(rec.getAttributeAsBoolean(ENABLED)));
		enable.addClickHandler(event -> DocumentService.Instance.get().enableTicket(rec.getAttributeAsLong("id"),
				new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(ENABLED, true);
						rec.setAttribute(VALID, true);
						list.refreshRow(list.getRecordIndex(rec));
					}
				}));

		MenuItem disable = new MenuItem();
		disable.setTitle(I18N.message("disable"));
		disable.setEnabled(Boolean.TRUE.equals(rec.getAttributeAsBoolean(ENABLED)));
		disable.addClickHandler(event -> DocumentService.Instance.get().disableTicket(rec.getAttributeAsLong("id"),
				new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(ENABLED, false);
						rec.setAttribute(VALID, false);
						list.refreshRow(list.getRecordIndex(rec));
					}
				}));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), choice -> {
			if (Boolean.TRUE.equals(choice))
				DocumentService.Instance.get().deleteTicket(rec.getAttributeAsLong("id"), new DefaultAsyncCallback<>() {
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

		contextMenu.setItems(enable, disable, ticketURL, download, preview, openInFolder, delete);
		contextMenu.showContextMenu();
	}

	private String sampleTicketUrl(String urlBase, String ticketId, String type) {
		if (!urlBase.endsWith("/"))
			urlBase += "/";
		if ("2".equals(type))
			urlBase += "view/" + ticketId;
		else
			urlBase += "download-ticket?ticketId=" + ticketId;
		return urlBase;
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