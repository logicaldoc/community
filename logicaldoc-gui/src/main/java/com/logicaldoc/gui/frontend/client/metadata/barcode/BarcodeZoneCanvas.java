package com.logicaldoc.gui.frontend.client.metadata.barcode;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIBarcodeZone;
import com.logicaldoc.gui.common.client.beans.GUIZone;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.panels.zone.ZoneCanvas;
import com.logicaldoc.gui.frontend.client.services.BarcodeService;

/**
 * A canvas that contains a note to be displayed in a page of a document
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
public class BarcodeZoneCanvas extends ZoneCanvas {

	/**
	 * Constructor
	 * 
	 * @param zone the zone associated to this canvas
	 * @param barcodePanel the Barcode panel with the zones editor
	 */
	public BarcodeZoneCanvas(GUIZone zone, BarcodeTemplatesPanel barcodePanel) {
		super(zone, barcodePanel);
	}

	@Override
	protected void onEdit() {
		if (zone.getSample() == null || zone.getSampleText() == null) {
			LD.contactingServer();
			BarcodeService.Instance.get().updateZone((GUIBarcodeZone) zone, new DefaultAsyncCallback<>() {
				@Override
				public void handleSuccess(GUIBarcodeZone newZone) {
					BarcodeZoneCanvas.this.zone.setSample(newZone.getSample());
					BarcodeZoneCanvas.this.zone.setSampleText(newZone.getSampleText());
					new ZoneEditor((GUIBarcodeZone) zone).show();
				}
			});
		} else {
			new ZoneEditor((GUIBarcodeZone) zone).show();
		}
	}
}