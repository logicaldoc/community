package com.logicaldoc.gui.frontend.client.reports;

import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.settings.protocols.ApiCallsPanel;

/**
 * This panel is used to show the API calls.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class ApiCallsReport extends AdminPanel {

	public ApiCallsReport() {
		super("apicalls");
		body.setMembers(new ApiCallsPanel());
	}
}