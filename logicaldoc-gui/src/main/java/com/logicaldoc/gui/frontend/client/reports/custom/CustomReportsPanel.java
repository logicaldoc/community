package com.logicaldoc.gui.frontend.client.reports.custom;

import java.util.List;

import com.google.gwt.user.client.Timer;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIReport;
import com.logicaldoc.gui.common.client.data.ReportsDS;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.EnabledListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.grid.RunningListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.preview.PreviewPopup;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.document.DocumentUtil;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.ReportService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Panel showing the list of all custom reports.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3.1
 */
public class CustomReportsPanel extends AdminPanel {

	private static final String ENABLED = "eenabled";

	private static final String OUTPUT_DOC_ID = "outputDocId";

	private Layout detailsContainer = new VLayout();

	private RefreshableListGrid list;

	private Canvas details = SELECT_REPORT;

	static final Canvas SELECT_REPORT = new HTMLPanel("&nbsp;" + I18N.message("selectareport"));

	public CustomReportsPanel() {
		super("customreports");
	}

	@Override
	public void onDraw() {
		final InfoPanel infoPanel = new InfoPanel("");

		// Initialize the listing panel
		Layout listing = new VLayout();
		listing.setHeight("70%");
		listing.setShowResizeBar(true);

		listing.addMember(infoPanel);

		ListGridField id = new ListGridField("id", 50);
		id.setHidden(true);

		ListGridField name = new ListGridField("name", I18N.message("name"), 200);
		name.setCanFilter(true);

		ListGridField outputFormat = new ListGridField("outputFormat", I18N.message("format"), 70);
		outputFormat.setCanFilter(true);

		ListGridField enabled = new EnabledListGridField();

		ListGridField running = new RunningListGridField();

		ListGridField lastRun = new DateListGridField("lastRun", "lastrun");

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		list.setFields(id, enabled, running, name, outputFormat, lastRun);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setFilterOnKeypress(true);

		listing.addMember(list);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		toolStrip.addButton(refresh);
		refresh.addClickHandler(event -> refresh());

		ToolStripButton newReport = new ToolStripButton();
		newReport.setTitle(I18N.message("newreport"));
		newReport.addClickHandler(event -> {
			list.deselectAllRecords();
			new ReportUploader(CustomReportsPanel.this, null).show();
		});

		/**
		 * Only admin users of the default tenant can upload new reports
		 */
		if (canUploadDesign())
			toolStrip.addButton(newReport);

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		body.setMembers(toolStrip, listing, detailsContainer);

		list.addCellContextClickHandler(click -> {
			showContextMenu();
			click.cancel();
		});

		list.addSelectionChangedHandler(click -> onSelectedReport());

		list.addDataArrivedHandler(
				click -> infoPanel.setMessage(I18N.message("showreports", Integer.toString(list.getTotalRows()))));

		refresh();

		/*
		 * Create the timer that synchronizes the view
		 */
		Timer timer = new Timer() {
			public void run() {
				update();
			}
		};
		timer.scheduleRepeating(5 * 1000);
	}

	public void refresh() {
		list.refresh(new ReportsDS());
		detailsContainer.removeMembers(detailsContainer.getMembers());
		details = SELECT_REPORT;
		detailsContainer.setMembers(details);
	}

	public void update() {
		ReportService.Instance.get().getReports(new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(List<GUIReport> reports) {
				for (GUIReport report : reports)
					updateReportRecord(report);
			}
		});
	}

	void updateReportRecord(GUIReport report) {
		for (ListGridRecord rcd : list.getRecords()) {
			if (Long.parseLong(rcd.getAttributeAsString("id")) == report.getId()) {
				updateRecord(rcd, report);
				break;
			}
		}
	}

	private void updateRecord(ListGridRecord rcd, GUIReport report) {
		long oldVersion = rcd.getAttributeAsLong("recordVersion");

		rcd.setAttribute("running",
				rcd.getAttribute("name").equals(report.getName()) && report.getStatus() == GUIReport.STATUS_RUNNING);
		rcd.setAttribute("name", report.getName());
		rcd.setAttribute("status", report.getStatus());
		rcd.setAttribute("lastRun", report.getLastRun());
		rcd.setAttribute("lastModified", report.getLastModified());
		rcd.setAttribute("recordVersion", report.getRecordVersion());

		if (report.getOutputDocId() != null)
			rcd.setAttribute(OUTPUT_DOC_ID, "" + report.getOutputDocId());
		else
			rcd.setAttribute(OUTPUT_DOC_ID, (String) null);
		list.refreshRow(list.getRecordIndex(rcd));

		boolean selected = list.getSelectedRecord() != null && rcd.equals(list.getSelectedRecord());

		// Decide if we have to refresh the properties panel
		if (selected && report.getRecordVersion() != oldVersion) {
			onSelectedReport();
		}
	}

	private boolean canUploadDesign() {
		return Session.get().getUser().getTenant().isDefault()
				&& Session.get().getUser().isMemberOf(Constants.GROUP_ADMIN);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord rec = list.getSelectedRecord();
		final long selectedId = Long.parseLong(rec.getAttributeAsString("id"));
		final Long outputDocId = rec.getAttribute(OUTPUT_DOC_ID) != null
				? Long.parseLong(rec.getAttributeAsString(OUTPUT_DOC_ID))
				: null;
		final long outputFolderId = Long.parseLong(rec.getAttributeAsString("outputFolderId"));

		MenuItem execute = new MenuItem();
		execute.setTitle(I18N.message("execute"));
		execute.addClickHandler(
				event -> ReportService.Instance.get().getReport(selectedId, false, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(GUIReport report) {
						new ReportParametersForm(report, CustomReportsPanel.this).show();
					}
				}));

		if (GUIReport.STATUS_IDLE != list.getSelectedRecord().getAttributeAsInt("status")
				|| Boolean.FALSE.equals(list.getSelectedRecord().getAttributeAsBoolean(ENABLED)))
			execute.setEnabled(false);

		MenuItem upload = new MenuItem();
		upload.setTitle(I18N.message("uploadnewdesign"));
		upload.addClickHandler(event -> {
			GUIReport report = new GUIReport();
			report.setId(selectedId);
			report.setName(rec.getAttributeAsString("name"));
			new ReportUploader(CustomReportsPanel.this, report).show();
		});
		upload.setEnabled(canUploadDesign());

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), chioice -> {
			if (Boolean.TRUE.equals(chioice))
				ReportService.Instance.get().delete(selectedId, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						list.removeSelectedData();
						list.deselectAllRecords();
						showReportDetails(null);
					}
				});
		}));

		MenuItem enable = new MenuItem();
		enable.setTitle(I18N.message("enable"));
		enable.addClickHandler(event -> ReportService.Instance.get()
				.changeStatus(Long.parseLong(rec.getAttributeAsString("id")), true, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(ENABLED, true);
						list.refreshRow(list.getRecordIndex(rec));
					}
				}));
		enable.setEnabled(Boolean.FALSE.equals(list.getSelectedRecord().getAttributeAsBoolean(ENABLED)));

		MenuItem disable = new MenuItem();
		disable.setTitle(I18N.message("disable"));
		disable.addClickHandler(event -> ReportService.Instance.get()
				.changeStatus(Long.parseLong(rec.getAttributeAsString("id")), false, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						rec.setAttribute(ENABLED, false);
						list.refreshRow(list.getRecordIndex(rec));
					}
				}));
		disable.setEnabled(Boolean.TRUE.equals(list.getSelectedRecord().getAttributeAsBoolean(ENABLED)));

		MenuItem openInFolder = new MenuItem();
		openInFolder.setTitle(I18N.message("openinfolder"));
		openInFolder.addClickHandler(event -> DocumentsPanel.get().openInFolder(outputFolderId, outputDocId));

		MenuItem download = new MenuItem();
		download.setTitle(I18N.message("download"));
		download.addClickHandler(event -> DocumentUtil.downloadDocument(outputDocId));

		MenuItem preview = new MenuItem();
		preview.setTitle(I18N.message("preview"));
		preview.setEnabled(outputDocId != null);
		preview.addClickHandler(
				event -> DocumentService.Instance.get().getById(outputDocId, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(GUIDocument doc) {
						new PreviewPopup(doc).show();
					}
				}));

		MenuItem export = new MenuItem();
		export.setTitle(I18N.message("export"));
		export.addClickHandler(event -> Util.download(
				Util.contextPath() + "report/controller?command=export&reportId=" + rec.getAttributeAsString("id")));

		contextMenu.setItems(enable, disable, execute, preview, upload, export, openInFolder, download, new MenuItemSeparator(), delete);
		contextMenu.showContextMenu();
	}

	public void showReportDetails(GUIReport report) {
		if (!(details instanceof ReportDetailsPanel)) {
			detailsContainer.removeMember(details);
			details = new ReportDetailsPanel(null);
			detailsContainer.addMember(details);
		}
		((ReportDetailsPanel) details).setReport(report);
	}

	public ListGrid getList() {
		return list;
	}

	/**
	 * Updates the selected rec with new data
	 * 
	 * @param report the report to update
	 */
	public void updateRecord(GUIReport report) {
		Record rec = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, report.getId()));
		if (rec == null) {
			rec = new ListGridRecord();
			// Append a new rec
			rec.setAttribute("id", report.getId());
			list.addData(rec);
			list.selectRecord(rec);
		}

		rec.setAttribute("name", report.getName());
		rec.setAttribute(ENABLED, report.getEnabled() == 1);
		rec.setAttribute("running", report.getStatus() != GUIReport.STATUS_IDLE);
		rec.setAttribute("outputFormat", report.getOutputFormat());
		if (report.getOutputFolder() != null) {
			rec.setAttribute("outputFolder", report.getOutputFolder().getName());
			rec.setAttribute("outputFolderId", report.getOutputFolder().getId());
		}

		list.refreshRow(list.getRecordIndex(rec));
	}

	private void onSelectedReport() {
		Record rec = list.getSelectedRecord();
		if (rec != null)
			ReportService.Instance.get().getReport(Long.parseLong(rec.getAttributeAsString("id")), true,
					new DefaultAsyncCallback<>() {
						@Override
						public void onSuccess(GUIReport report) {
							showReportDetails(report);
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