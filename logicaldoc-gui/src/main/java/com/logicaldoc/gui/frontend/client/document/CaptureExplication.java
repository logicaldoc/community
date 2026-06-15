package com.logicaldoc.gui.frontend.client.document;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;

/**
 * A popup that explain how the capture process worked
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3
 */
public class CaptureExplication extends Window {

    public CaptureExplication(String explication) {
        addCloseClickHandler(event -> destroy());

        setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
        setTitle(I18N.message("explication"));
        setWidth(400);
        setHeight(350);
        setCanDragResize(true);
        setIsModal(true);
        setShowModalMask(true);
        centerInPage();
        setPadding(5);
        setAutoSize(true);

        addItem(new HTMLPanel(explication));
    }
}