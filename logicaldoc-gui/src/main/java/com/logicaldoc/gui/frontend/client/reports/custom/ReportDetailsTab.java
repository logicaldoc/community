package com.logicaldoc.gui.frontend.client.reports.custom;

import com.logicaldoc.gui.common.client.beans.GUIReport;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Superclass for all tab panels in the Report details area
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3.1
 */
public abstract class ReportDetailsTab extends HLayout {
	protected GUIReport report;

	protected ChangedHandler changedHandler;

	/**
	 * Constructor
	 * 
	 * @param report The report this instance refers to
	 * @param changedHandler The handler to be invoked in case of changes in the
	 *        importFolder
	 */
	protected ReportDetailsTab(GUIReport report, ChangedHandler changedHandler) {
		super();
		this.report = report;
		this.changedHandler = changedHandler;
	}

	public GUIReport getReport() {
		return report;
	}

	public ChangedHandler getChangedHandler() {
		return changedHandler;
	}
}
