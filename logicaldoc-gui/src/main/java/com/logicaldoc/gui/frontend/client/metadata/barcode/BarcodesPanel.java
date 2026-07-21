package com.logicaldoc.gui.frontend.client.metadata.barcode;

import com.logicaldoc.gui.frontend.client.administration.AdminPanel;

/**
 * Panel showing the barcode processing infos.
 * 
 * @author Marco Mescheri - LogicalDOC
 * @since 6.1
 */
public class BarcodesPanel extends AdminPanel {
    public BarcodesPanel() {
        super("barcodes");
    }

    @Override
    public void onDraw() {
        tab.setPane(new BarcodeTemplatesPanel(null));
    }
}