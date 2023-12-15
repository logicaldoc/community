package com.logicaldoc.gui.frontend.client.system;

import com.google.gwt.core.client.GWT;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.ContentsType;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Main log panel
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class LogPanel extends VLayout {

	private String appender;

	public LogPanel(String appender) {
		this.appender = appender;
		setHeight100();
	}

	@Override
	public void onDraw() {
		final HTMLPane htmlPane = new HTMLPane();
		htmlPane.setWidth100();
		htmlPane.setHeight100();
		htmlPane.setShowEdges(true);
		htmlPane.setContentsURL(GWT.getHostPageBaseURL() + "log?appender=" + appender);
		htmlPane.setContentsType(ContentsType.PAGE);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton(I18N.message("refresh"));
		refresh.addClickHandler(event -> {
			htmlPane.redraw();
			htmlPane.setWidth100();
			htmlPane.setHeight100();
		});

		ToolStripButton download = new ToolStripButton(I18N.message("downloadlogs"));
		download.addClickHandler(event -> Util.download(Util.contextPath() + "log?appender=all"));

		toolStrip.addButton(refresh);
		if (!Session.get().isDemo())
			toolStrip.addButton(download);
		toolStrip.addFill();
		addMember(toolStrip);
		addMember(htmlPane);
	}
}