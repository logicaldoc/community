package com.logicaldoc.gui.frontend.client.metadata.barcode;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.smartgwt.client.widgets.tab.Tab;

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
		Tab processingQueueTab = new Tab(I18N.message("processingqueue"));
		processingQueueTab.setPane(new BarcodeQueuePanel(100));
		tabs.addTab(processingQueueTab);
	}
}