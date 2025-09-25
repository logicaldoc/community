package com.logicaldoc.gui.common.client.preview;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;

public class TileImageLightbox extends Window {
	public TileImageLightbox(long docId, String title) {
		int size = 800;
		if (Session.get().getConfig("gui.tile.size") != null)
			size = Integer.parseInt(Session.get().getConfig("gui.tile.size"));
		int windowHeight = size + 10;
		if (windowHeight > com.google.gwt.user.client.Window.getClientHeight())
			windowHeight = com.google.gwt.user.client.Window.getClientHeight();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(title);
		setWidth("50%");
		setHeight(windowHeight);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		HTMLPanel html = new HTMLPanel(Util.tileImageHTML(docId, null, null, size));
		addItem(html);
	}
}
