package com.logicaldoc.gui.frontend.client.reports.custom;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIReport;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.ReportService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Cursor;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.Side;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

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

	private HLayout savePanel;

	private TabSet tabSet = new TabSet();

	private ReportsPanel reportsPanel;

	public ReportDetailsPanel(ReportsPanel reportsPanel) {
		super();

		this.reportsPanel = reportsPanel;
		setHeight100();
		setWidth100();
		setMembersMargin(10);

		savePanel = new HLayout();
		savePanel.setHeight(20);
		savePanel.setVisible(false);
		savePanel.setStyleName("warn");
		savePanel.setWidth100();
		Button saveButton = new Button(I18N.message("save"));
		saveButton.setAutoFit(true);
		saveButton.setMargin(2);
		saveButton.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		});
		saveButton.setLayoutAlign(VerticalAlignment.CENTER);

		HTMLPane spacer = new HTMLPane();
		spacer.setContents("<div>&nbsp;</div>");
		spacer.setWidth("70%");
		spacer.setOverflow(Overflow.HIDDEN);

		Img closeImage = ItemFactory.newImgIcon("delete.png");
		closeImage.setHeight("16px");
		closeImage.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (report.getId() != 0) {
					ReportService.Instance.get().getReport(report.getId(), true, new AsyncCallback<GUIReport>() {
						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
						}

						@Override
						public void onSuccess(GUIReport report) {
							setReport(report);
						}
					});
				} else {
					GUIReport newreport = new GUIReport();
					setReport(newreport);
				}
				savePanel.setVisible(false);
			}
		});
		closeImage.setCursor(Cursor.HAND);
		closeImage.setTooltip(I18N.message("close"));
		closeImage.setLayoutAlign(Alignment.RIGHT);
		closeImage.setLayoutAlign(VerticalAlignment.CENTER);

		savePanel.addMember(saveButton);
		savePanel.addMember(spacer);
		savePanel.addMember(closeImage);
		addMember(savePanel);

		tabSet = new TabSet();
		tabSet.setTabBarPosition(Side.TOP);
		tabSet.setTabBarAlign(Side.LEFT);
		tabSet.setWidth100();
		tabSet.setHeight100();

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
		if (savePanel != null)
			savePanel.setVisible(false);

		/*
		 * Prepare the standard properties tab
		 */
		if (standardPanel != null) {
			standardPanel.destroy();
			if (standardTabPanel.contains(standardPanel))
				standardTabPanel.removeMember(standardPanel);
		}

		if (logLabel != null) {
			logLabel.destroy();
			if (logTabPanel.contains(logLabel))
				logTabPanel.removeMember(logLabel);
		}

		ChangedHandler changeHandler = new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				onModified();
			}
		};
		standardPanel = new ReportStandardProperties(report, changeHandler);
		standardTabPanel.addMember(standardPanel);

		logLabel = new Label(report.getLog() != null ? report.getLog() : "");
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
		savePanel.setVisible(true);
	}

	private boolean validate() {
		boolean stdValid = standardPanel.validate();
		if (!stdValid)
			tabSet.selectTab(0);
		return stdValid;
	}

	public void onSave() {
		if (validate()) {
			ReportService.Instance.get().save(report, new AsyncCallback<GUIReport>() {
				@Override
				public void onFailure(Throwable caught) {
					Log.serverError(caught);
				}

				@Override
				public void onSuccess(GUIReport report) {
					savePanel.setVisible(false);
					if (report != null) {
						reportsPanel.updateRecord(report);
						reportsPanel.showReportDetails(report);
					}
				}
			});
		}
	}
}