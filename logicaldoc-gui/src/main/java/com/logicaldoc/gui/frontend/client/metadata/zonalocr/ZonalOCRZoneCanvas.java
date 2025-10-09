package com.logicaldoc.gui.frontend.client.metadata.zonalocr;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIZone;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.panels.zone.ZoneCanvas;
import com.logicaldoc.gui.frontend.client.services.ZonalOCRService;

/**
 * A canvas that contains a zone to be displayed in a ZonalOCR sample
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class ZonalOCRZoneCanvas extends ZoneCanvas {

	/**
	 * Constructor
	 * 
	 * @param zone the zone associated to this canvas
	 * @param ocrPanel the OCR panel with the zones editor
	 */
	public ZonalOCRZoneCanvas(GUIZone zone, ZonalOCRTemplatesPanel ocrPanel) {
		super(zone, ocrPanel);
	}

	@Override
	public void onEdit() {
		if (zone.getSample() == null || zone.getSampleText() == null) {
			LD.contactingServer();
			ZonalOCRService.Instance.get().updateZone(zone, new DefaultAsyncCallback<>() {
				@Override
				public void handleSuccess(GUIZone newZone) {
					ZonalOCRZoneCanvas.this.zone.setSample(newZone.getSample());
					ZonalOCRZoneCanvas.this.zone.setSampleText(newZone.getSampleText());
					new ZoneEditor(zone).show();
				}
			});
		} else {
			new ZoneEditor(zone).show();
		}
	}
}