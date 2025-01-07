package com.logicaldoc.gui.frontend.client.reports.custom;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIReport;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.logicaldoc.gui.frontend.client.services.ReportService;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel collects details about a Report
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3.1
 */
public class ReportDetailsPanel extends VLayout {
	private GUIReport report;

	private Layout standardTabPanel;

	private ReportStandardProperties standardPanel;

	private Layout logTabPanel;

	private Label logLabel;

	private EditingTabSet tabSet;

	private CustomReportsPanel reportsPanel;

	public ReportDetailsPanel(CustomReportsPanel reportsPanel) {
		super();

		this.reportsPanel = reportsPanel;
		setHeight100();
		setWidth100();
		setMembersMargin(10);

		tabSet = new EditingTabSet(saveEvent -> onSave(), cancelEvent -> {
			if (report.getId() != 0) {
				ReportService.Instance.get().getReport(report.getId(), true, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(GUIReport report) {
						setReport(report);
					}
				});
			} else {
				GUIReport newreport = new GUIReport();
				setReport(newreport);
			}
			tabSet.hideSave();
		});

		Tab propertiesTab = new Tab(I18N.message("properties"));
		standardTabPanel = new HLayout();
		standardTabPanel.setWidth100();
		standardTabPanel.setHeight100();
		propertiesTab.setPane(standardTabPanel);
		tabSet.addTab(propertiesTab);

		Tab logTab = new Tab(I18N.message("log"));
		logTabPanel = new HLayout();
		logTabPanel.setWidth100();
		logTabPanel.setHeight100();
		logTab.setPane(logTabPanel);
		tabSet.addTab(logTab);

		addMember(tabSet);
	}

	private void refresh() {
		tabSet.hideSave();

		/*
		 * Prepare the standard properties tab
		 */
		if (standardPanel != null) {
			standardPanel.destroy();
			if (Boolean.TRUE.equals(standardTabPanel.contains(standardPanel)))
				standardTabPanel.removeMember(standardPanel);
		}

		if (logLabel != null) {
			logLabel.destroy();
			if (Boolean.TRUE.equals(logTabPanel.contains(logLabel)))
				logTabPanel.removeMember(logLabel);
		}

		standardPanel = new ReportStandardProperties(report, (ChangedEvent event) -> onModified());
		standardTabPanel.addMember(standardPanel);

		logLabel = new Label(report.getLog() != null ? report.getLog() : "");
		logLabel.setCanSelectText(true);
		logLabel.setWidth100();
		logLabel.setHeight100();
		logTabPanel.addMember(logLabel);
	}

	public GUIReport getReport() {
		return report;
	}

	public void setReport(GUIReport report) {
		this.report = report;
		refresh();
	}

	public void onModified() {
		tabSet.displaySave();
	}

	private boolean validate() {
		boolean stdValid = standardPanel.validate();
		if (!stdValid)
			tabSet.selectTab(0);
		return stdValid;
	}

	public void onSave() {
		if (validate()) {
			ReportService.Instance.get().save(report, new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(GUIReport report) {
					tabSet.hideSave();
					if (report != null) {
						reportsPanel.updateRecord(report);
						reportsPanel.showReportDetails(report);
					}
				}
			});
		}
	}
}