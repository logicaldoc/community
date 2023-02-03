package com.logicaldoc.gui.frontend.client.metadata.zonalocr;

import com.logicaldoc.gui.common.client.beans.GUIOCRTemplate;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * Panel showing the Zonal OCR administration panel.
 * 
 * @author Marco Mescheri - LogicalDOC
 * @since 8.4.2
 */
public class ZonalOCRPanel extends AdminPanel {

	private GUIOCRTemplate selectedOcrTemplate;

	private GUITemplate selectedDocumentTemplate;

	public ZonalOCRPanel(GUITemplate selectedDocumentTemplate, GUIOCRTemplate selectedOcrTemplate) {
		super("zonalocr");

		this.selectedDocumentTemplate = selectedDocumentTemplate;
		this.selectedOcrTemplate = selectedOcrTemplate;
	}

	@Override
	public void onDraw() {
		tab.setPane(new ZonalOCRTemplatesPanel(selectedDocumentTemplate, selectedOcrTemplate));
		Tab processingQueueTab = new Tab(I18N.message("processingqueue"));
		processingQueueTab.setPane(new ZonalOCRQueuePanel(100));
		tabs.addTab(processingQueueTab);
	}
}