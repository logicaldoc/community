package com.logicaldoc.gui.frontend.client.document;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.Window;

/**
 * Show differences between two versions at file conten level
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.0.1
 */
public class ContentDiff extends Window {

	public ContentDiff(long docId, String fileVersion1, String fileVersion2) {
		super();

		setTitle(I18N.message("compare") + " " + fileVersion1 + " - " + fileVersion2);
		setWidth100();
		setHeight100();
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.MAXIMIZE_BUTTON, HeaderControls.MINIMIZE_BUTTON,
				HeaderControls.CLOSE_BUTTON);
		setCanDragReposition(true);
		setCanDragResize(true);
		centerInPage();

		String url = Util.contextPath() + "/diff/diff.jsp?docId=" + docId + "&v1=" + fileVersion1 + "&v2="
				+ fileVersion2;
		HTMLFlow html = new HTMLFlow("<iframe src='" + url + "' style='border:0px solid white; width:"
				+ (getWidth() - 15) + "px; height:" + (getHeight() - 45) + "px;'  seamless='seamless'></iframe>");

		addItem(html);
	}
}