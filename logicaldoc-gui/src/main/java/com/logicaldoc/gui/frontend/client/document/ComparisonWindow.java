package com.logicaldoc.gui.frontend.client.document;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIVersion;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.Window;

/**
 * Show differences between two versions at file content level
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.0.1
 */
public class ComparisonWindow extends Window {

	private HTMLFlow preview = null;

	private GUIVersion version1;

	private GUIVersion version2;

	public ComparisonWindow(GUIVersion version1, GUIVersion version2) {
		super();
		this.version1 = version1;
		this.version2 = version2;

		setTitle(I18N.message("compare") + " " + version1.getFileName() + "(" + version1.getFileVersion() + ") > "
				+ version2.getFileName() + "(" + version2.getFileVersion() + ")");
		setWidth100();
		setHeight100();
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.MAXIMIZE_BUTTON, HeaderControls.MINIMIZE_BUTTON,
				HeaderControls.CLOSE_BUTTON);
		setCanDragReposition(true);
		setCanDragResize(true);
		centerInPage();

		reloadPreview();
	}

	/**
	 * Reloads a preview.
	 */
	private void reloadPreview() {
		if (preview != null)
			removeMember(preview);

		preview = new HTMLFlow();
		String contents = "";

		String locale = Session.get().getUser().getLanguage();
		String url = Util.contextPath() + "prev/index.jsp?docId=" + version1.getDocId() + "&docId1="
				+ version1.getDocId() + "&fileVersion1=" + version1.getFileVersion() + "&docId2=" + version2.getDocId()
				+ "&fileVersion2=" + version2.getFileVersion() + "&path=compare" + "&control=ignore&locale=" + locale
				+ "#locale=" + locale.replace('_', '-');
		contents = "<iframe src='" + url + "' style='border:0px solid white; width:" + (getWidth() - 1) + "px; height:"
				+ (getHeight() - 1) + "px; overflow:hidden;'  scrolling='no' seamless='seamless'></iframe>";

		preview.setContents(contents);
		addMember(preview);
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